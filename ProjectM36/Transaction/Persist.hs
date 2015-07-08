module ProjectM36.Transaction.Persist where
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.UUID as U
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import System.FilePath
import System.Directory
import qualified Data.Text as T
import Control.Monad

tempTransactionDir :: FilePath -> U.UUID -> FilePath
tempTransactionDir dbdir transUUID = dbdir </> "." ++ U.toString transUUID

transactionDir :: FilePath -> U.UUID -> FilePath
transactionDir dbdir transUUID = dbdir </> U.toString transUUID

transactionInfoPath :: FilePath -> FilePath
transactionInfoPath = (</>) "info"

relvarsDir :: FilePath -> FilePath        
relvarsDir = (</>) "relvars"

incDepsDir :: FilePath -> FilePath
incDepsDir = (</>) "incdeps"

atomFuncsDir :: FilePath -> FilePath
atomFuncsDir = (</>) "atomfuncs"

readTransaction :: FilePath -> U.UUID -> IO (Either PersistenceError Transaction)
readTransaction dbdir transUUID = do
  let transDir = transactionDir dbdir transUUID
  transDirExists <- doesDirectoryExist transDir
  if not transDirExists then    
    return $ Left $ MissingTransactionError transUUID
    else do
    relvars <- readRelVars transDir
    transInfo <- liftM B.decode $ BS.readFile (transactionInfoPath transDir)
    incDeps <- readIncDeps transDir
    atomFuncs <- readAtomFuncs transDir
    let newContext = DatabaseContext { inclusionDependencies = incDeps,
                                       relationVariables = relvars,
                                       atomFunctions = atomFuncs }
    
    return $ Right $ Transaction transUUID transInfo newContext
        
writeTransaction :: FilePath -> Transaction -> IO ()
writeTransaction dbdir trans = do
  let tempTransDir = tempTransactionDir dbdir (transactionUUID trans)
      finalTransDir = transactionDir dbdir (transactionUUID trans)
      context = transactionContext trans
  transDirExists <- doesDirectoryExist finalTransDir      
  if not transDirExists then do
    --create sub directories
    mapM_ createDirectory [tempTransDir, relvarsDir tempTransDir, incDepsDir tempTransDir, atomFuncsDir tempTransDir]
    writeRelVars tempTransDir (relationVariables context)
    writeIncDeps tempTransDir (inclusionDependencies context)
    writeAtomFuncs tempTransDir (atomFunctions context)
    BS.writeFile (transactionInfoPath tempTransDir) (B.encode $ transactionInfo trans)
    --move the temp directory to final location
    renameDirectory tempTransDir finalTransDir
    else
      return ()
  
writeRelVar :: FilePath -> (RelVarName, Relation) -> IO ()
writeRelVar transDir (relvarName, rel) = do
  let relvarPath = relvarsDir transDir </> T.unpack relvarName
  BS.writeFile relvarPath (B.encode rel)
  
writeRelVars :: FilePath -> (M.Map RelVarName Relation) -> IO ()
writeRelVars transDir relvars = mapM_ (writeRelVar transDir) $ M.toList relvars
    
readRelVars :: FilePath -> IO (M.Map RelVarName Relation)
readRelVars transDir = do
  let relvarsPath = relvarsDir transDir
  relvarNames <- getDirectoryContents relvarsPath
  relvars <- mapM (\name -> do
                      rel <- liftM B.decode $ BS.readFile (relvarsPath </> name)
                      return (T.pack name, rel)) relvarNames
  return $ M.fromList relvars

writeAtomFuncs :: FilePath -> AtomFunctions -> IO ()
writeAtomFuncs transDir funcs = mapM_ (writeAtomFunc (atomFuncsDir transDir)) $ HS.toList funcs

--all the atom functions are in one file (???)
readAtomFuncs :: FilePath -> IO (AtomFunctions)
readAtomFuncs transDir = do
  funcNames <- getDirectoryContents (incDepsDir transDir)
  funcs <- mapM (readAtomFunc transDir) (map T.pack funcNames)
  return $ HS.fromList funcs
  
--to write the atom functions, we really some bytecode to write (GHCi bytecode?)
writeAtomFunc :: FilePath -> AtomFunction -> IO ()
writeAtomFunc transDir func = do
  let atomFuncPath = atomFuncsDir transDir </> T.unpack (atomFuncName func)
  BS.writeFile atomFuncPath BS.empty
  
readAtomFunc :: FilePath -> AtomFunctionName -> IO (AtomFunction)
readAtomFunc transDir funcName = do
  let atomFuncPath = atomFuncsDir transDir </> T.unpack funcName  
  _ <- BS.readFile atomFuncPath
  return undefined
  
writeIncDep :: FilePath -> (IncDepName, InclusionDependency) -> IO ()  
writeIncDep transDir (incDepName, incDep) = do
  BS.writeFile (incDepsDir transDir </> T.unpack incDepName) $ B.encode incDep
  
writeIncDeps :: FilePath -> M.Map IncDepName InclusionDependency -> IO ()  
writeIncDeps transDir incdeps = mapM_ (writeIncDep transDir) $ M.toList incdeps 
  
readIncDep :: FilePath -> IncDepName -> IO (IncDepName, InclusionDependency)
readIncDep transDir incdepName = do
  let incDepPath = incDepsDir transDir </> T.unpack incdepName
  incDepData <- BS.readFile incDepPath
  return $ (incdepName, B.decode incDepData)
  
readIncDeps :: FilePath -> IO (M.Map IncDepName InclusionDependency)  
readIncDeps transDir = do
  let incDepsPath = incDepsDir transDir
  incDepNames <- getDirectoryContents incDepsPath
  incDeps <- mapM (readIncDep transDir) (map T.pack incDepNames)
  return $ M.fromList incDeps