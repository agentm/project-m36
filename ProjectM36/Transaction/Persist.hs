module ProjectM36.Transaction.Persist where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Persist (writeBSFileSync, DiskSync, renameSync)
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.UUID as U
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import System.FilePath
import System.Directory
import qualified Data.Text as T
import Control.Monad
import ProjectM36.AtomFunctions.Basic (basicAtomFunctions)

getDirectoryNames :: FilePath -> IO [FilePath]
getDirectoryNames path = do
  subpaths <- getDirectoryContents path
  return $ filter (\n -> n `notElem` ["..", "."]) subpaths

tempTransactionDir :: FilePath -> U.UUID -> FilePath
tempTransactionDir dbdir transUUID = dbdir </> "." ++ U.toString transUUID

transactionDir :: FilePath -> U.UUID -> FilePath
transactionDir dbdir transUUID = dbdir </> U.toString transUUID

transactionInfoPath :: FilePath -> FilePath
transactionInfoPath transdir = transdir </> "info"

relvarsDir :: FilePath -> FilePath        
relvarsDir transdir = transdir </> "relvars"

incDepsDir :: FilePath -> FilePath
incDepsDir transdir = transdir </> "incdeps"

atomFuncsDir :: FilePath -> FilePath
atomFuncsDir transdir = transdir </> "atomfuncs"

atomTypesPath :: FilePath -> FilePath
atomTypesPath transdir = transdir </> "atomtypes"

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
    atomtypes <- readAtomTypes transDir
    --atomFuncs <- readAtomFuncs transDir -- not yet supported since there is no bytecode to serialize yet
    let atomFuncs = basicAtomFunctions
    let newContext = DatabaseContext { inclusionDependencies = incDeps,
                                       relationVariables = relvars,
                                       atomTypes = atomtypes,
                                       notifications = M.empty,
                                       atomFunctions = atomFuncs }
    
    return $ Right $ Transaction transUUID transInfo newContext
        
writeTransaction :: DiskSync -> FilePath -> Transaction -> IO ()
writeTransaction sync dbdir trans = do
  let tempTransDir = tempTransactionDir dbdir (transactionUUID trans)
      finalTransDir = transactionDir dbdir (transactionUUID trans)
      context = transactionContext trans
  transDirExists <- doesDirectoryExist finalTransDir      
  if not transDirExists then do
    --create sub directories
    mapM_ createDirectory [tempTransDir, relvarsDir tempTransDir, incDepsDir tempTransDir, atomFuncsDir tempTransDir]
    writeRelVars sync tempTransDir (relationVariables context)
    writeIncDeps sync tempTransDir (inclusionDependencies context)
    writeAtomFuncs sync tempTransDir (atomFunctions context)
    writeAtomTypes sync tempTransDir (atomTypes context)
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
readAtomFuncs :: FilePath -> IO (AtomFunctions)
readAtomFuncs transDir = do
  funcNames <- getDirectoryNames (atomFuncsDir transDir)
  funcs <- mapM (readAtomFunc transDir) (map T.pack funcNames)
  return $ HS.fromList funcs
  
--to write the atom functions, we really some bytecode to write (GHCi bytecode?)
writeAtomFunc :: DiskSync -> FilePath -> AtomFunction -> IO ()
writeAtomFunc sync transDir func = do
  let atomFuncPath = atomFuncsDir transDir </> T.unpack (atomFuncName func)
  writeBSFileSync sync atomFuncPath BS.empty
  
readAtomFunc :: FilePath -> AtomFunctionName -> IO (AtomFunction)
readAtomFunc transDir funcName = do
  let atomFuncPath = atomFuncsDir transDir </> T.unpack funcName  
  _ <- BS.readFile atomFuncPath
  return undefined
  
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
  
writeAtomTypes :: DiskSync -> FilePath -> AtomTypes -> IO ()  
writeAtomTypes sync path aTypes = writeBSFileSync sync path $ B.encode aTypes

readAtomTypes :: FilePath -> IO (AtomTypes)
readAtomTypes path = do
  let atPath = atomTypesPath path
  liftM B.decode (BS.readFile atPath)