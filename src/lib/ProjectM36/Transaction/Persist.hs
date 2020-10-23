{-# LANGUAGE TypeApplications #-}
module ProjectM36.Transaction.Persist where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Transaction
import ProjectM36.DatabaseContextFunction
import ProjectM36.AtomFunction
import ProjectM36.Persist (writeBSFileSync, DiskSync, renameSync)
import qualified Data.Map as M
import qualified Data.HashSet as HS
import System.FilePath
import System.Directory
import qualified Data.Text as T
import Control.Monad
import ProjectM36.ScriptSession
import ProjectM36.AtomFunctions.Basic (precompiledAtomFunctions)
import Codec.Winery

#ifdef PM36_HASKELL_SCRIPTING
import GHC
import Control.Exception
import GHC.Paths
#endif

getDirectoryNames :: FilePath -> IO [FilePath]
getDirectoryNames path =
  filter (\ n -> n `notElem` ["..", "."]) <$> getDirectoryContents path


tempTransactionDir :: FilePath -> TransactionId -> FilePath
tempTransactionDir dbdir transId = dbdir </> "." ++ show transId

transactionDir :: FilePath -> TransactionId -> FilePath
transactionDir dbdir transId = dbdir </> show transId

transactionInfoPath :: FilePath -> FilePath
transactionInfoPath transdir = transdir </> "info"

relvarsPath :: FilePath -> FilePath        
relvarsPath transdir = transdir </> "relvars"

incDepsDir :: FilePath -> FilePath
incDepsDir transdir = transdir </> "incdeps"

atomFuncsPath :: FilePath -> FilePath
atomFuncsPath transdir = transdir </> "atomfuncs"

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
    transInfo <- readFileDeserialise (transactionInfoPath transDir)
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
                                       dbcFunctions = dbcFuncs }
        newSchemas = Schemas newContext sschemas
    return $ Right $ Transaction transId transInfo newSchemas
        
writeTransaction :: DiskSync -> FilePath -> Transaction -> IO ()
writeTransaction sync dbdir trans = do
  let tempTransDir = tempTransactionDir dbdir (transactionId trans)
      finalTransDir = transactionDir dbdir (transactionId trans)
      context = concreteDatabaseContext trans
  transDirExists <- doesDirectoryExist finalTransDir
  unless transDirExists $ do
    --create sub directories
    mapM_ createDirectory [tempTransDir, incDepsDir tempTransDir, dbcFuncsDir tempTransDir]
    writeRelVars sync tempTransDir (relationVariables context)
    writeIncDeps sync tempTransDir (inclusionDependencies context)
    writeAtomFuncs sync tempTransDir (atomFunctions context)
    writeDBCFuncs sync tempTransDir (dbcFunctions context)
    writeTypeConstructorMapping sync tempTransDir (typeConstructorMapping context)
    writeSubschemas sync tempTransDir (subschemas trans)
    writeFileSerialise (transactionInfoPath tempTransDir) (transactionInfo trans)
    --move the temp directory to final location
    renameSync sync tempTransDir finalTransDir

writeRelVars :: DiskSync -> FilePath -> RelationVariables -> IO ()
writeRelVars sync transDir relvars = do
  let path = relvarsPath transDir
  writeBSFileSync sync path (serialise relvars)

readRelVars :: FilePath -> IO RelationVariables
readRelVars transDir = 
  readFileDeserialise (relvarsPath transDir)

writeAtomFuncs :: DiskSync -> FilePath -> AtomFunctions -> IO ()
writeAtomFuncs sync transDir funcs = do
  let atomFuncPath = atomFuncsPath transDir 
  writeBSFileSync sync atomFuncPath (serialise $ map (\f -> (atomFuncType f, atomFuncName f, atomFunctionScript f)) (HS.toList funcs))

--all the atom functions are in one file
readAtomFuncs :: FilePath -> Maybe ScriptSession -> IO AtomFunctions
readAtomFuncs transDir mScriptSession = do
  atomFuncsList <- readFileDeserialise (atomFuncsPath transDir)
  --only Haskell script functions can be serialized
  --we always return the pre-compiled functions
  funcs <- mapM (\(funcType, funcName, mFuncScript) -> loadAtomFunc precompiledAtomFunctions mScriptSession funcName funcType mFuncScript) atomFuncsList
  pure (HS.union precompiledAtomFunctions (HS.fromList funcs))
  
loadAtomFunc :: AtomFunctions -> Maybe ScriptSession -> AtomFunctionName -> [AtomType] -> Maybe AtomFunctionBodyScript -> IO AtomFunction
loadAtomFunc precompiledFuncs _mScriptSession funcName _funcType mFuncScript = case mFuncScript of
    --handle pre-compiled case- pull it from the precompiled list
    Nothing -> case atomFunctionForName funcName precompiledFuncs of
      --WARNING: possible landmine here if we remove a precompiled atom function in the future, then the transaction cannot be restored
      Left _ -> error ("expected precompiled atom function: " ++ T.unpack funcName)
      Right realFunc -> pure realFunc
    --handle a real Haskell scripted function- compile and load
    Just _funcScript ->
#ifdef PM36_HASKELL_SCRIPTING
      case _mScriptSession of
        Nothing -> error "attempted to read serialized AtomFunction without scripting enabled"
        Just scriptSession -> do
          --risk of GHC exception during compilation here
          eCompiledScript <- runGhc (Just libdir) $ do
            setSession (hscEnv scriptSession)
            compileScript (atomFunctionBodyType scriptSession) _funcScript
          case eCompiledScript of
            Left err -> throwIO err
            Right compiledScript -> pure AtomFunction { atomFuncName = funcName,
                                                        atomFuncType = _funcType,
                                                        atomFuncBody = AtomFunctionBody (Just _funcScript) compiledScript }
#else
      error "Haskell scripting is disabled"
#endif                                    

--if the script session is enabled, compile the script, otherwise, hard error!  
  
readAtomFunc :: FilePath -> AtomFunctionName -> Maybe ScriptSession -> AtomFunctions -> IO AtomFunction
#if !defined(PM36_HASKELL_SCRIPTING)
readAtomFunc _ _ _ _ = error "Haskell scripting is disabled"
#else
readAtomFunc transDir funcName mScriptSession precompiledFuncs = do
  let atomFuncPath = atomFuncsPath transDir
  (funcType, mFuncScript) <- readFileDeserialise @([AtomType],Maybe T.Text) atomFuncPath
  case mFuncScript of
    --handle pre-compiled case- pull it from the precompiled list
    Nothing -> case atomFunctionForName funcName precompiledFuncs of
      --WARNING: possible landmine here if we remove a precompiled atom function in the future, then the transaction cannot be restored
      Left _ -> error ("expected precompiled atom function: " ++ T.unpack funcName)
      Right realFunc -> pure realFunc
    --handle a real Haskell scripted function- compile and load
    Just funcScript ->

      case mScriptSession of
        Nothing -> error "attempted to read serialized AtomFunction without scripting enabled"
        Just scriptSession -> do
          --risk of GHC exception during compilation here
          eCompiledScript <- runGhc (Just libdir) $ do
            setSession (hscEnv scriptSession)
            compileScript (atomFunctionBodyType scriptSession) funcScript
          case eCompiledScript of
            Left err -> throwIO err
            Right compiledScript -> pure AtomFunction { atomFuncName = funcName,
                                                         atomFuncType = funcType,
                                                         atomFuncBody = AtomFunctionBody (Just funcScript) compiledScript }
#endif


writeDBCFuncs :: DiskSync -> FilePath -> DatabaseContextFunctions -> IO ()
writeDBCFuncs sync transDir funcs = mapM_ (writeDBCFunc sync transDir) (HS.toList funcs)
  
writeDBCFunc :: DiskSync -> FilePath -> DatabaseContextFunction -> IO ()
writeDBCFunc sync transDir func = do
  let dbcFuncPath = dbcFuncsDir transDir </> T.unpack (dbcFuncName func)  
  writeBSFileSync sync dbcFuncPath (serialise (dbcFuncType func, databaseContextFunctionScript func))

readDBCFuncs :: FilePath -> Maybe ScriptSession -> IO DatabaseContextFunctions
readDBCFuncs transDir mScriptSession = do
  funcNames <- getDirectoryNames (dbcFuncsDir transDir)
  --only Haskell script functions can be serialized
  --we always return the pre-compiled functions
  let funcs = mapM ((\name -> readDBCFunc transDir name mScriptSession precompiledDatabaseContextFunctions) . T.pack) funcNames
  HS.union basicDatabaseContextFunctions . HS.fromList <$> funcs
  
readDBCFunc :: FilePath -> DatabaseContextFunctionName -> Maybe ScriptSession -> DatabaseContextFunctions -> IO DatabaseContextFunction
#if !defined(PM36_HASKELL_SCRIPTING)
readDBCFunc transDir funcName _ precompiledFuncs = do
#else
readDBCFunc transDir funcName mScriptSession precompiledFuncs = do
#endif
  let dbcFuncPath = dbcFuncsDir transDir </> T.unpack funcName
  (_funcType, mFuncScript) <- readFileDeserialise @([AtomType], Maybe T.Text) dbcFuncPath
  case mFuncScript of
    Nothing -> case databaseContextFunctionForName funcName precompiledFuncs of
      Left _ -> error ("expected precompiled dbc function: " ++ T.unpack funcName)
      Right realFunc -> pure realFunc --return precompiled function
    Just _funcScript ->
#ifdef PM36_HASKELL_SCRIPTING
      case mScriptSession of
        Nothing -> error "attempted to read serialized AtomFunction without scripting enabled"
        Just scriptSession -> do
          eCompiledScript <- runGhc (Just libdir) $ do
            setSession (hscEnv scriptSession)
            compileScript (dbcFunctionBodyType scriptSession) _funcScript
          case eCompiledScript of
            Left err -> throwIO err
            Right compiledScript -> pure DatabaseContextFunction { dbcFuncName = funcName,
                                                                    dbcFuncType = _funcType,
                                                                    dbcFuncBody = DatabaseContextFunctionBody (Just _funcScript) compiledScript}
#else
      error "Haskell scripting is disabled"
#endif

writeIncDep :: DiskSync -> FilePath -> (IncDepName, InclusionDependency) -> IO ()  
writeIncDep sync transDir (incDepName, incDep) = 
  writeBSFileSync sync (incDepsDir transDir </> T.unpack incDepName) $ serialise incDep
  
writeIncDeps :: DiskSync -> FilePath -> M.Map IncDepName InclusionDependency -> IO ()  
writeIncDeps sync transDir incdeps = mapM_ (writeIncDep sync transDir) $ M.toList incdeps 
  
readIncDep :: FilePath -> IncDepName -> IO (IncDepName, InclusionDependency)
readIncDep transDir incdepName = do
  let incDepPath = incDepsDir transDir </> T.unpack incdepName
  incDepData <- readFileDeserialise incDepPath
  pure (incdepName, incDepData)
  
readIncDeps :: FilePath -> IO (M.Map IncDepName InclusionDependency)  
readIncDeps transDir = do
  let incDepsPath = incDepsDir transDir
  incDepNames <- getDirectoryNames incDepsPath
  M.fromList <$> mapM (readIncDep transDir . T.pack) incDepNames
  
readSubschemas :: FilePath -> IO Subschemas  
readSubschemas transDir = do
  let sschemasPath = subschemasPath transDir
  readFileDeserialise sschemasPath
  
writeSubschemas :: DiskSync -> FilePath -> Subschemas -> IO ()  
writeSubschemas sync transDir sschemas = do
  let sschemasPath = subschemasPath transDir
  writeBSFileSync sync sschemasPath (serialise sschemas)
  
writeTypeConstructorMapping :: DiskSync -> FilePath -> TypeConstructorMapping -> IO ()  
writeTypeConstructorMapping sync path types = let atPath = typeConsPath path in
  writeBSFileSync sync atPath $ serialise types

readTypeConstructorMapping :: FilePath -> IO TypeConstructorMapping
readTypeConstructorMapping path = do
  let atPath = typeConsPath path
  readFileDeserialise atPath
  
  
