module ProjectM36.Transaction.Persist where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Transaction
import ProjectM36.DatabaseContextFunction
import ProjectM36.AtomFunction
import ProjectM36.Persist (writeBSFileSync, DiskSync, renameSync)
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import System.FilePath
import System.Directory
import qualified Data.Text as T
import Control.Monad
import ProjectM36.ScriptSession
import ProjectM36.AtomFunctions.Basic (precompiledAtomFunctions)
import Control.Exception
import GHC
import GHC.Paths

import Debug.Trace

getDirectoryNames :: FilePath -> IO [FilePath]
getDirectoryNames path = do
  subpaths <- getDirectoryContents path
  return $ filter (\n -> n `notElem` ["..", "."]) subpaths

tempTransactionDir :: FilePath -> TransactionId -> FilePath
tempTransactionDir dbdir transId = dbdir </> "." ++ show transId

transactionDir :: FilePath -> TransactionId -> FilePath
transactionDir dbdir transId = dbdir </> show transId

transactionInfoPath :: FilePath -> FilePath
transactionInfoPath transdir = transdir </> "info"

relvarsDir :: FilePath -> FilePath        
relvarsDir transdir = transdir </> "relvars"

incDepsDir :: FilePath -> FilePath
incDepsDir transdir = transdir </> "incdeps"

atomFuncsDir :: FilePath -> FilePath
atomFuncsDir transdir = transdir </> "atomfuncs"

dbcFuncsDir :: FilePath -> FilePath
dbcFuncsDir transdir = transdir </> "dbcfuncs"

typeConsPath :: FilePath -> FilePath
typeConsPath transdir = transdir </> "typecons"

subschemasPath :: FilePath -> FilePath
subschemasPath transdir = transdir </> "schemas"

readTransaction :: FilePath -> TransactionId -> Maybe ScriptSession -> IO (Either PersistenceError Transaction)
readTransaction dbdir transId mScriptSession = do
  let transDir = transactionDir dbdir transId
  transDirExists <- doesDirectoryExist transDir
  if not transDirExists then    
    return $ Left $ MissingTransactionError transId
    else do
    relvars <- readRelVars transDir
    transInfo <- liftM B.decode $ BS.readFile (transactionInfoPath transDir)
    incDeps <- readIncDeps transDir
    typeCons <- readTypeConstructorMapping transDir
    sschemas <- readSubschemas transDir
    dbcFuncs <- readDBCFuncs transDir mScriptSession
    atomFuncs <- readAtomFuncs transDir mScriptSession
    let newContext = DatabaseContext { inclusionDependencies = incDeps,
                                       relationVariables = relvars,
                                       typeConstructorMapping = typeCons,
                                       notifications = M.empty,
                                       atomFunctions = atomFuncs, 
                                       dbcFunctions = dbcFuncs, 
                                       tupleFunctions = undefined}
        newSchemas = Schemas newContext sschemas
    return $ Right $ Transaction transId transInfo newSchemas
        
writeTransaction :: DiskSync -> FilePath -> Transaction -> IO ()
writeTransaction sync dbdir trans = do
  let tempTransDir = tempTransactionDir dbdir (transactionId trans)
      finalTransDir = transactionDir dbdir (transactionId trans)
      context = concreteDatabaseContext trans
  transDirExists <- doesDirectoryExist finalTransDir
  if not transDirExists then do
    --create sub directories
    mapM_ createDirectory [tempTransDir, relvarsDir tempTransDir, incDepsDir tempTransDir, atomFuncsDir tempTransDir, dbcFuncsDir tempTransDir]
    writeRelVars sync tempTransDir (relationVariables context)
    writeIncDeps sync tempTransDir (inclusionDependencies context)
    writeAtomFuncs sync tempTransDir (atomFunctions context)
    writeDBCFuncs sync tempTransDir (dbcFunctions context)
    writeTypeConstructorMapping sync tempTransDir (typeConstructorMapping context)
    writeSubschemas sync tempTransDir (subschemas trans)
    BS.writeFile (transactionInfoPath tempTransDir) (B.encode $ transactionInfo trans)
    --move the temp directory to final location
    renameSync sync tempTransDir finalTransDir
    else
      return ()
  
writeRelVar :: DiskSync -> FilePath -> (RelVarName, Relation) -> IO ()
writeRelVar sync transDir (relvarName, rel) = do
  let relvarPath = relvarsDir transDir </> T.unpack relvarName
  writeBSFileSync sync relvarPath (B.encode rel)
  
writeRelVars :: DiskSync -> FilePath -> (M.Map RelVarName Relation) -> IO ()
writeRelVars sync transDir relvars = mapM_ (writeRelVar sync transDir) $ M.toList relvars
    
readRelVars :: FilePath -> IO (M.Map RelVarName Relation)
readRelVars transDir = do
  let relvarsPath = relvarsDir transDir
  relvarNames <- getDirectoryNames relvarsPath
  relvars <- mapM (\name -> do
                      rel <- liftM B.decode $ BS.readFile (relvarsPath </> name)
                      return (T.pack name, rel)) relvarNames
  return $ M.fromList relvars

writeAtomFuncs :: DiskSync -> FilePath -> AtomFunctions -> IO ()
writeAtomFuncs sync transDir funcs = mapM_ (writeAtomFunc sync transDir) $ HS.toList funcs

--all the atom functions are in one file (???)
readAtomFuncs :: FilePath -> Maybe ScriptSession -> IO AtomFunctions
readAtomFuncs transDir mScriptSession = do
  funcNames <- getDirectoryNames (atomFuncsDir transDir)
  --only Haskell script functions can be serialized
  --we always return the pre-compiled functions
  funcs <- mapM (\name -> readAtomFunc transDir name mScriptSession precompiledAtomFunctions) (map T.pack funcNames)
  pure (HS.union precompiledAtomFunctions (HS.fromList funcs))
  
--to write the atom functions, we really some bytecode to write (GHCi bytecode?)
writeAtomFunc :: DiskSync -> FilePath -> AtomFunction -> IO ()
writeAtomFunc sync transDir func = do
  let atomFuncPath = atomFuncsDir transDir </> T.unpack (atomFuncName func)
  writeBSFileSync sync atomFuncPath (B.encode (atomFuncType func, atomFunctionScript func))
  
--if the script session is enabled, compile the script, otherwise, hard error!  
readAtomFunc :: FilePath -> AtomFunctionName -> Maybe ScriptSession -> AtomFunctions -> IO (AtomFunction)
readAtomFunc transDir funcName mScriptSession precompiledFuncs = do
  let atomFuncPath = atomFuncsDir transDir </> T.unpack funcName  
  (funcType, mFuncScript) <- liftM B.decode (BS.readFile atomFuncPath)
  case mFuncScript of
    --handle pre-compiled case- pull it from the precompiled list
    Nothing -> case atomFunctionForName funcName precompiledFuncs of
      --WARNING: possible landmine here if we remove a precompiled atom function in the future, then the transaction cannot be restored
      Left _ -> error ("expected precompiled atom function: " ++ T.unpack funcName)
      Right realFunc -> pure realFunc
    --handle a real Haskell scripted function- compile and load
    Just funcScript -> do
      case mScriptSession of
        Nothing -> error "attempted to read serialized AtomFunction without scripting enabled"
        Just scriptSession -> do
          --risk of GHC exception during compilation here
          eCompiledScript <- runGhc (Just libdir) $ do
            setSession (hscEnv scriptSession)
            compileScript (atomFunctionBodyType scriptSession) funcScript
          case eCompiledScript of
            Left err -> throwIO err
            Right compiledScript -> pure (AtomFunction { atomFuncName = funcName,
                                                         atomFuncType = funcType,
                                                         atomFuncBody = AtomFunctionBody (Just funcScript) compiledScript })

writeDBCFuncs :: DiskSync -> FilePath -> DatabaseContextFunctions -> IO ()
writeDBCFuncs sync transDir funcs = mapM_ (writeDBCFunc sync transDir) (HS.toList funcs)
  
writeDBCFunc :: DiskSync -> FilePath -> DatabaseContextFunction -> IO ()
writeDBCFunc sync transDir func = do
  let dbcFuncPath = dbcFuncsDir transDir </> T.unpack (dbcFuncName func)  
  writeBSFileSync sync (traceShowId dbcFuncPath) (B.encode (dbcFuncType func, databaseContextFunctionScript func))

readDBCFuncs :: FilePath -> Maybe ScriptSession -> IO DatabaseContextFunctions
readDBCFuncs transDir mScriptSession = do
  funcNames <- getDirectoryNames (dbcFuncsDir transDir)
  --only Haskell script functions can be serialized
  --we always return the pre-compiled functions
  funcs <- mapM (\name -> readDBCFunc transDir name mScriptSession precompiledDatabaseContextFunctions) (map T.pack funcNames)
  return $ HS.union basicDatabaseContextFunctions (HS.fromList funcs)
  
readDBCFunc :: FilePath -> DatabaseContextFunctionName -> Maybe ScriptSession -> DatabaseContextFunctions -> IO DatabaseContextFunction  
readDBCFunc transDir funcName mScriptSession precompiledFuncs = do
  let dbcFuncPath = dbcFuncsDir transDir </> T.unpack funcName
  (funcType, mFuncScript) <- liftM B.decode (BS.readFile dbcFuncPath)
  case mFuncScript of
    Nothing -> case databaseContextFunctionForName funcName precompiledFuncs of
      Left _ -> error ("expected precompiled dbc function: " ++ T.unpack funcName)
      Right realFunc -> pure realFunc --return precompiled function
    Just funcScript -> do
      case mScriptSession of
        Nothing -> error "attempted to read serialized AtomFunction without scripting enabled"
        Just scriptSession -> do
          eCompiledScript <- runGhc (Just libdir) $ do
            setSession (hscEnv scriptSession)
            compileScript (dbcFunctionBodyType scriptSession) funcScript
          case eCompiledScript of
            Left err -> throwIO err
            Right compiledScript -> pure (DatabaseContextFunction { dbcFuncName = funcName,
                                                                    dbcFuncType = funcType,
                                                                    dbcFuncBody = DatabaseContextFunctionBody (Just funcScript) compiledScript})

writeIncDep :: DiskSync -> FilePath -> (IncDepName, InclusionDependency) -> IO ()  
writeIncDep sync transDir (incDepName, incDep) = do
  writeBSFileSync sync (incDepsDir transDir </> T.unpack incDepName) $ B.encode incDep
  
writeIncDeps :: DiskSync -> FilePath -> M.Map IncDepName InclusionDependency -> IO ()  
writeIncDeps sync transDir incdeps = mapM_ (writeIncDep sync transDir) $ M.toList incdeps 
  
readIncDep :: FilePath -> IncDepName -> IO (IncDepName, InclusionDependency)
readIncDep transDir incdepName = do
  let incDepPath = incDepsDir transDir </> T.unpack incdepName
  incDepData <- BS.readFile incDepPath
  return $ (incdepName, B.decode incDepData)
  
readIncDeps :: FilePath -> IO (M.Map IncDepName InclusionDependency)  
readIncDeps transDir = do
  let incDepsPath = incDepsDir transDir
  incDepNames <- getDirectoryNames incDepsPath
  incDeps <- mapM (readIncDep transDir) (map T.pack incDepNames)
  return $ M.fromList incDeps
  
readSubschemas :: FilePath -> IO Subschemas  
readSubschemas transDir = do
  let sschemasPath = subschemasPath transDir
  bytes <- BS.readFile sschemasPath
  pure (B.decode bytes)
  
writeSubschemas :: DiskSync -> FilePath -> Subschemas -> IO ()  
writeSubschemas sync transDir sschemas = do
  let sschemasPath = subschemasPath transDir
  writeBSFileSync sync sschemasPath (B.encode sschemas)
  
writeTypeConstructorMapping :: DiskSync -> FilePath -> TypeConstructorMapping -> IO ()  
writeTypeConstructorMapping sync path types = let atPath = typeConsPath path in
  writeBSFileSync sync atPath $ B.encode types

readTypeConstructorMapping :: FilePath -> IO (TypeConstructorMapping)
readTypeConstructorMapping path = do
  let atPath = typeConsPath path
  liftM B.decode (BS.readFile atPath)
  
  