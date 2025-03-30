{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#ifdef PM36_HASKELL_SCRIPTING
{-# LANGUAGE TypeApplications #-}
#endif
module ProjectM36.Transaction.Persist where
import ProjectM36.Trace
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Transaction.Types
import ProjectM36.DatabaseContext
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.DatabaseContextFunctions.Basic
import ProjectM36.ChangeTrackingDatabaseContext
import ProjectM36.AtomFunction
import ProjectM36.Persist (DiskSync, renameSync, writeSerialiseSync)
import ProjectM36.Function
import qualified Data.Map as M
import qualified Data.HashSet as HS
import System.FilePath
import System.Directory
import qualified Data.Text as T
import Data.Foldable (toList)
import Control.Monad
import ProjectM36.ScriptSession
import ProjectM36.AtomFunctions.Basic (precompiledAtomFunctions)
import Codec.Winery
import Control.Concurrent.Async
import GHC.Generics
import qualified Data.Text.Encoding as TE
import Optics.Core
import qualified Data.Set as S


#if defined(__APPLE__) || defined(linux_HOST_OS)
#define USE_LINUX_XATTRS 1
#endif

#ifdef USE_LINUX_XATTRS
import System.Linux.XAttr
#endif

#ifdef PM36_HASKELL_SCRIPTING
import GHC
import Control.Exception
import GHC.Paths
#endif

xattrName :: String
xattrName = "project-m36.relvarName"

getDirectoryNames :: FilePath -> IO [FilePath]
getDirectoryNames path =
  filter (\ n -> n `notElem` ["..", "."]) <$> getDirectoryContents path


tempTransactionDir :: FilePath -> TransactionId -> FilePath
tempTransactionDir dbdir transId = dbdir </> "." ++ show transId

transactionDir :: FilePath -> TransactionId -> FilePath
transactionDir dbdir transId = dbdir </> show transId

transactionInfoPath :: FilePath -> FilePath
transactionInfoPath transdir = transdir </> "info"

relvarsDir :: FilePath -> FilePath        
relvarsDir transdir = transdir </> "relvars"

relvarsSimplePath :: FilePath -> FilePath
relvarsSimplePath transdir = relvarsDir transdir </> "index"

notificationsPath :: FilePath -> FilePath
notificationsPath transdir = transdir </> "notifs"

incDepsDir :: FilePath -> FilePath
incDepsDir transdir = transdir </> "incdeps"

atomFuncsPath :: FilePath -> FilePath
atomFuncsPath transdir = transdir </> "atomfuncs"

dbcFuncsPath :: FilePath -> FilePath
dbcFuncsPath transdir = transdir </> "dbcfuncs"

typeConsPath :: FilePath -> FilePath
typeConsPath transdir = transdir </> "typecons"

subschemasPath :: FilePath -> FilePath
subschemasPath transdir = transdir </> "schemas"

registeredQueriesPath :: FilePath -> FilePath
registeredQueriesPath transdir = transdir </> "registered_queries"

aggregateFunctionsPath :: FilePath -> FilePath
aggregateFunctionsPath transdir = transdir </> "aggregateFunctions"

-- | where compiled modules are stored within the database directory
objectFilesPath :: FilePath -> FilePath
objectFilesPath transdir = transdir </> ".." </> "compiled_modules"

-- note that some database context function elements don't change between transactions
unchangedElementsPath :: FilePath -> FilePath
unchangedElementsPath transdir = transdir </> "unchanged"

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
    notifs <- readNotifications transDir
    dbcFuncs <- readFuncs transDir (dbcFuncsPath transDir) basicDatabaseContextFunctions mScriptSession
    atomFuncs <- readFuncs transDir (atomFuncsPath transDir) precompiledAtomFunctions mScriptSession
    registeredQs <- readRegisteredQueries transDir
    let newContext = DatabaseContext { _inclusionDependencies = incDeps,
                                       _relationVariables = relvars,
                                       _typeConstructorMapping = typeCons,
                                       _notifications = notifs,
                                       _atomFunctions = atomFuncs,
                                       _dbcFunctions = dbcFuncs,
                                       _registeredQueries = registeredQs }
        newSchemas = Schemas newContext sschemas
    return $ Right $ Transaction transId transInfo newSchemas

-- | Transactions are always written write-once-read-many
writeTransaction :: DiskSync -> FilePath -> UncommittedTransaction -> IO ()
writeTransaction sync dbdir trans = do
  let tempTransDir = tempTransactionDir dbdir (transactionId trans)
      finalTransDir = transactionDir dbdir (transactionId trans)
      context = concreteDatabaseContext trans
  transDirExists <- doesDirectoryExist finalTransDir
  unless transDirExists $ do
    --create sub directories if necessary
    mapM_ createDirectory [tempTransDir, incDepsDir tempTransDir]
    writeUnchangedTransactionMarkers sync tempTransDir context

    writeRelVars sync tempTransDir (_ctrelationVariables context)
    writeIncDeps sync tempTransDir (context ^. inclusionDependencies)
    writeFuncs sync (atomFuncsPath tempTransDir) (HS.toList (context ^. atomFunctions))
    writeFuncs sync (dbcFuncsPath tempTransDir) (HS.toList (context ^. dbcFunctions ))
    writeNotifications sync tempTransDir (context ^. notifications)
    writeTypeConstructorMapping sync tempTransDir (context ^. typeConstructorMapping)
    writeSubschemas sync tempTransDir (subschemas trans)
    writeRegisteredQueries sync tempTransDir (context ^. registeredQueries)
    writeFileSerialise (transactionInfoPath tempTransDir) (transactionInfo trans)
    --move the temp directory to final location
    renameSync sync tempTransDir finalTransDir


-- local data structure for serialization, simple relvars (those which merely reference other relvars) are written to one file, wherease everything else (altered relvars) are written to individual files, potentially in parallel
data SingleFileRelationVariables = SingleFileRelationVariables
  {
    simpleRelVars :: RelationVariables,
    complexRelVarNameMap :: M.Map RelVarName FilePath
  }
  deriving (Show, Generic, Eq)
  deriving Serialise via WineryRecord SingleFileRelationVariables

-- | ADT used to serialize fast-path, "unchanged" database context value relative to parent transaction.
data UnchangedDatabaseContextValues =
  UnchangedDatabaseContextValues {
     unchangedRelationVariables :: Maybe TransactionId,
     unchangedInclusionDependencies :: Maybe TransactionId,
     unchangedAtomFunctions :: Maybe TransactionId,
     unchangedDBCFunctions :: Maybe TransactionId,
     unchangedNotifications :: Maybe TransactionId,
     unchangedTypeConstructorMapping :: Maybe TransactionId,
     unchangedRegisteredQueries :: Maybe TransactionId
  }
  deriving (Generic, Show)
  deriving Serialise via WineryRecord UnchangedDatabaseContextValues

unwrittenTransactionCanUseDatabaseContextValuesFastPath :: UncommittedTransaction -> Bool
unwrittenTransactionCanUseDatabaseContextValuesFastPath trans =
  S.size (parentIds trans) == 1

-- | For database context values which have *not* changed since the parent, write into a single file.
writeUnchangedTransactionMarkers :: DiskSync -> FilePath -> ChangeTrackingDatabaseContext -> IO ()
writeUnchangedTransactionMarkers sync transDir ctdbc = do
  let wUnchanged = UnchangedDatabaseContextValues {
        unchangedRelationVariables = mTrans (_ctrelationVariables ctdbc),
        unchangedInclusionDependencies = mTrans (_ctinclusionDependencies ctdbc),
        unchangedAtomFunctions = mTrans (_ctatomFunctions ctdbc),
        unchangedDBCFunctions = mTrans (_ctdbcFunctions ctdbc),
        unchangedNotifications = mTrans (_ctnotifications ctdbc),
        unchangedTypeConstructorMapping = mTrans (_cttypeConstructorMapping ctdbc),
        unchangedRegisteredQueries = mTrans (_ctregisteredQueries ctdbc)
        }
      mTrans :: forall a. ChangedMarker a -> Maybe TransactionId
      mTrans (NotChangedMarker tid _) = Just tid
      mTrans ChangedMarker{} = Nothing
      unchangedPath = unchangedElementsPath transDir
  writeSerialiseSync sync unchangedPath wUnchanged

writeRelVars :: DiskSync -> FilePath -> ChangedMarker RelationVariables -> IO ()
writeRelVars _sync _transDir NotChangedMarker{} = pure ()
writeRelVars sync transDir (ChangedMarker relvars) = do
  let relvarsPath = relvarsDir transDir
      simpleInfoPath = relvarsSimplePath transDir
  --write unchanged relvars and file name mapping to "relvars" file
      (simplervs, complexrvs) = M.partition isSimple relvars
      isSimple (RelationVariable _ _) = True
      isSimple _ = False
      --add incrementing integer to use as file name
      writeRvMapExprs = snd $ M.mapAccum (\acc rexpr -> (acc + 1, (acc, rexpr))) (0 :: Int) complexrvs
      writeRvMap = M.map (show . fst) writeRvMapExprs
      simpleFileInfo = SingleFileRelationVariables {
        simpleRelVars = simplervs,
        complexRelVarNameMap = writeRvMap
        }
  --parallelization opportunity
  traceBlock "write relvars" $ do
    createDirectory relvarsPath
    let writeSimple = do
          writeSerialiseSync sync simpleInfoPath simpleFileInfo
        writeComplex = do
          forConcurrently_ (M.toList writeRvMapExprs) $ \(rvname, (rvnum,rvExpr)) -> do
            let rvpath = relvarsPath </> show rvnum
            writeSerialiseSync sync rvpath rvExpr
-- Project:M36 does not read these extended attributes, but they might be useful for debugging or database restoration            
#ifdef USE_LINUX_XATTRS
            createUserXAttr rvpath xattrName (TE.encodeUtf8 rvname)
#endif            
    concurrently_ writeSimple writeComplex

{-
-- | Optimized code path to read one relvar expression from disk instead of all of them for a full transaction- useful for streaming results. Throws exception if the relvar name cannot be found since this function expects the database files to be coherent.
readOneRelVar :: FilePath -> RelVarName -> IO GraphRefRelationalExpr
readOneRelVar transDir rvName = do
  let relvarsIndex = relvarsSimplePath transDir
  rvindex <- readFileDeserialise relvarsIndex
  case M.lookup rvName (simpleRelVars rvindex) of
    Just rvexpr -> pure rvexpr
    Nothing -> do --look in complex rvs
      case M.lookup rvName (complexRelVarNameMap rvindex) of
        Nothing -> error $ "failed to find " <> T.unpack rvName <> " in filesystem."
        Just rvnum ->
          readFileDeserialise (relvarsDir transDir </> show rvnum)
-}

readRelVars :: FilePath -> IO RelationVariables
readRelVars transDir = do
  let relvarsIndex = relvarsSimplePath transDir
  rvindex <- readFileDeserialise relvarsIndex
  complexRvAssocs <- forConcurrently (M.toList (complexRelVarNameMap rvindex)) $ \(rvname, rvpath) -> do
    rvExpr <- readFileDeserialise (relvarsDir transDir </> rvpath)
    pure (rvname, rvExpr)
  pure (simpleRelVars rvindex <> M.fromList complexRvAssocs)

writeFuncs :: Traversable t => DiskSync -> FilePath -> t (Function a) -> IO ()
writeFuncs sync funcWritePath funcs = traceBlock "write functions" $ do
  funcs' <- forM funcs $ \fun -> do
    case funcBody fun of
      FunctionScriptBody{} -> pure fun
      FunctionBuiltInBody{} -> pure fun
      FunctionObjectLoadedBody objPath a b c -> do
         let newFuncBody = FunctionObjectLoadedBody objPath a b c
         pure (fun { funcBody = newFuncBody })
  --write additional data for object-loaded functions (which are not built-in or scripted)
  let functionData f =
          (funcType f, funcName f, functionScript f, objInfo f)
      objInfo :: Function a -> Maybe ObjectFileInfo
      objInfo f =
        case funcBody f of
          FunctionObjectLoadedBody objPath modName entryFunc _ ->
            Just (ObjectFileInfo (objPath, modName, entryFunc))
          FunctionScriptBody{} -> Nothing
          FunctionBuiltInBody{} -> Nothing
  writeSerialiseSync sync funcWritePath (fmap functionData (toList funcs'))

readFuncs :: FilePath -> FilePath -> HS.HashSet (Function a) -> Maybe ScriptSession -> IO (HS.HashSet (Function a))
readFuncs transDir funcPath precompiledFunctions mScriptSession = do
  funcsList <- readFileDeserialise funcPath
  --we always return the pre-compiled functions
  --load object files and functions in objects (shared libraries or flat object files)
  let objFilesDir = objectFilesPath transDir
  funcs <- mapM (\(funcType', funcName', mFuncScript, mObjInfo) -> 
                    loadFunc objFilesDir precompiledFunctions mScriptSession funcName' funcType' mFuncScript mObjInfo) funcsList
  pure (HS.union precompiledFunctions (HS.fromList funcs))

newtype ObjectFileInfo = ObjectFileInfo { _unFileInfo :: (FilePath, String, String) }
 deriving (Show, Serialise)
-- deriving Serialise via WineryVariant ObjectFileInfo

loadFunc :: FilePath -> HS.HashSet (Function a) -> Maybe ScriptSession -> FunctionName -> [AtomType] -> Maybe FunctionBodyScript -> Maybe ObjectFileInfo -> IO (Function a)
loadFunc objFilesDir precompiledFuncs _mScriptSession funcName' _funcType mFuncScript mObjInfo = do
  case mObjInfo of
    --load from shared or static object library
    Just (ObjectFileInfo (path, modName, entryFunc)) -> do
      eFuncs <- loadFunctions modName entryFunc (Just objFilesDir) path
      case eFuncs of
        Left _ -> error $ "Failed to load " <> path
        Right funcs -> 
          case filter (\f -> funcName f == funcName'
                      ) funcs of
            [f] -> pure f
            [] -> error $ "Failed to find function \"" <> T.unpack funcName' <> "\" in " <> path
            _ -> error $ "impossible error in loading \"" <> T.unpack funcName' <> "\""
    Nothing -> 
      case mFuncScript of
        --handle pre-compiled case- pull it from the precompiled list
        Nothing -> case functionForName funcName' precompiledFuncs of
          --WARNING: possible landmine here if we remove a precompiled atom function in the future, then the transaction cannot be restored
          Left _ -> error ("expected precompiled atom function: " ++ T.unpack funcName')
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
                Right compiledScript -> pure Function { funcName = funcName',
                                                        funcType = _funcType,
                                                        funcBody = FunctionScriptBody _funcScript compiledScript }
#else
         error "Haskell scripting is disabled"
#endif                                    

--if the script session is enabled, compile the script, otherwise, hard error!  
  
readAtomFunc :: FilePath -> FunctionName -> Maybe ScriptSession -> AtomFunctions -> IO AtomFunction
#if !defined(PM36_HASKELL_SCRIPTING)
readAtomFunc _ _ _ _ = error "Haskell scripting is disabled"
#else
readAtomFunc transDir funcName' mScriptSession precompiledFuncs = do
  let atomFuncPath = atomFuncsPath transDir
  (funcType', mFuncScript) <- readFileDeserialise @([AtomType],Maybe T.Text) atomFuncPath
  case mFuncScript of
    --handle pre-compiled case- pull it from the precompiled list
    Nothing -> case atomFunctionForName funcName' precompiledFuncs of
      --WARNING: possible landmine here if we remove a precompiled atom function in the future, then the transaction cannot be restored
      Left _ -> error ("expected precompiled atom function: " ++ T.unpack funcName')
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
            Right compiledScript -> pure Function { funcName = funcName',
                                                    funcType = funcType',
                                                    funcBody = FunctionScriptBody funcScript compiledScript }
#endif

writeIncDep :: DiskSync -> FilePath -> (IncDepName, InclusionDependency) -> IO ()  
writeIncDep sync transDir (incDepName, incDep) = do
  writeSerialiseSync sync (incDepsDir transDir </> T.unpack incDepName) incDep
  
writeIncDeps :: DiskSync -> FilePath -> M.Map IncDepName InclusionDependency -> IO ()  
writeIncDeps sync transDir incdeps = 
  traceBlock "write incdeps" $ mapM_ (writeIncDep sync transDir) $ M.toList incdeps 
  
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
  traceBlock "write subschemas" $ writeSerialiseSync sync sschemasPath sschemas
  
writeTypeConstructorMapping :: DiskSync -> FilePath -> TypeConstructorMapping -> IO ()  
writeTypeConstructorMapping sync path types = do
  let atPath = typeConsPath path
  traceBlock "write tconsmap" $ writeSerialiseSync sync atPath types

readTypeConstructorMapping :: FilePath -> IO TypeConstructorMapping
readTypeConstructorMapping path = do
  let atPath = typeConsPath path
  readFileDeserialise atPath
  
readRegisteredQueries :: FilePath -> IO RegisteredQueries
readRegisteredQueries transDir = do
  let regQsPath = registeredQueriesPath transDir
  readFileDeserialise regQsPath

writeRegisteredQueries :: DiskSync -> FilePath -> RegisteredQueries -> IO ()
writeRegisteredQueries sync transDir regQs = do
  let regQsPath = registeredQueriesPath transDir
  traceBlock "write registered queries" $ writeSerialiseSync sync regQsPath regQs

readNotifications :: FilePath -> IO Notifications
readNotifications transDir = do
  let notifsPath = notificationsPath transDir
  readFileDeserialise notifsPath

writeNotifications :: DiskSync -> FilePath -> Notifications -> IO ()
writeNotifications sync transDir notifs = do
  let notifsPath = notificationsPath transDir
  traceBlock "write notifications" $ writeSerialiseSync sync notifsPath notifs



