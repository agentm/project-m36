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
import ProjectM36.ValueMarker
import ProjectM36.DatabaseContext.Types
import ProjectM36.IsomorphicSchema.Types hiding (concreteDatabaseContext, subschemas)
import ProjectM36.DatabaseContextFunctions.Basic
import ProjectM36.AtomFunction
import ProjectM36.Persist (DiskSync, renameSync, writeSerialiseSync, readDeserialise)
import ProjectM36.Function
import ProjectM36.AccessControlList
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
import qualified Data.Set as S
import Data.Default

#if defined(__APPLE__) || defined(linux_HOST_OS)
#define USE_LINUX_XATTRS 1
#endif

#ifdef USE_LINUX_XATTRS
import System.Linux.XAttr
import qualified Data.Text.Encoding as TE
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

notChangedSinceDatabaseContextPath :: FilePath -> FilePath
notChangedSinceDatabaseContextPath transdir = transdir </> "ncs"

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

aclPath :: FilePath -> FilePath
aclPath transdir = transdir </> "acl"

data NotChangedSinceDatabaseContext =
  NotChangedSinceDatabaseContext {
  ncsInclusionDependencies :: Maybe TransactionId,
  ncsRelationVariables :: Maybe TransactionId,
  ncsAtomFunctions :: Maybe TransactionId,
  ncsDbcFunctions :: Maybe TransactionId,
  ncsNotifications :: Maybe TransactionId,
  ncsTypeConstructorMapping :: Maybe TransactionId,
  ncsRegisteredQueries :: Maybe TransactionId,
  ncsSchemas :: Maybe TransactionId,
  ncsAcl :: Maybe TransactionId
  }
  deriving (Show, Generic)
  deriving Serialise via WineryRecord NotChangedSinceDatabaseContext

readTransaction :: FilePath -> TransactionId -> Maybe ScriptSession -> IO (Either PersistenceError Transaction)
readTransaction dbdir transId mScriptSession = do
  let transDir = transactionDir dbdir transId
  transDirExists <- doesDirectoryExist transDir
  if not transDirExists then    
    return $ Left $ MissingTransactionError transId
    else do
    ncs <- readNotChangedSinceDatabaseContext transDir
    let ncsRead :: forall a. (NotChangedSinceDatabaseContext -> Maybe TransactionId) ->
                   (FilePath -> IO a) -> IO (ValueMarker a)
        ncsRead f readIO' = do
          case f ncs of
            Nothing -> ValueMarker <$> readIO' transDir
            Just tid -> pure (NotChangedSinceMarker tid)

    transInfo <- readDeserialise (transactionInfoPath transDir)
    relvars <- ncsRead ncsRelationVariables readRelVars
    incDeps <- ncsRead ncsInclusionDependencies readIncDeps
    typeCons <- ncsRead ncsTypeConstructorMapping readTypeConstructorMapping
    acl' <- ncsRead ncsAcl readAcl
    sschemas <- case ncsSchemas ncs of
                  Nothing -> ValueMarker <$> readSubschemas transDir
                  Just tid -> pure $ NotChangedSinceMarker tid
    notifs <- ncsRead ncsNotifications readNotifications
    dbcFuncs <- ncsRead ncsDbcFunctions (\d -> readFuncs d (dbcFuncsPath transDir) basicDatabaseContextFunctions mScriptSession)
    atomFuncs <- ncsRead ncsAtomFunctions (\d -> readFuncs d (atomFuncsPath transDir) precompiledAtomFunctions mScriptSession)
    registeredQs <- ncsRead ncsRegisteredQueries readRegisteredQueries
    let newContext = DatabaseContext { inclusionDependencies = incDeps,
                                       relationVariables = relvars,
                                       typeConstructorMapping = typeCons,
                                       notifications = notifs,
                                       atomFunctions = atomFuncs,
                                       dbcFunctions = dbcFuncs,
                                       registeredQueries = registeredQs,
                                       acl = acl' }
        newSchemas = Schemas newContext sschemas
    return $ Right $ Transaction transId transInfo newSchemas

-- | Transactions are always written write-once-read-many
writeTransaction :: DiskSync -> FilePath -> UncommittedTransaction -> IO ()
writeTransaction sync dbdir (UncommittedTransaction trans) = do
  let tempTransDir = tempTransactionDir dbdir (transactionId trans)
      finalTransDir = transactionDir dbdir (transactionId trans)
      context = concreteDatabaseContext trans
  transDirExists <- doesDirectoryExist finalTransDir
  unless transDirExists $ do
    createDirectory tempTransDir
    let ncs = mkNotChangedSinceDatabaseContext context (subschemas trans)
    writeNotChangedSinceDatabaseContext sync tempTransDir ncs
    
    let ncsWrite :: forall a. (DatabaseContext -> ValueMarker a) -> FilePath -> (DiskSync -> FilePath -> a -> IO ()) -> IO ()
        ncsWrite f path writeIO =
          case f context of
            ValueMarker val -> do
              writeIO sync path val
            NotChangedSinceMarker{} -> pure ()
            
    ncsWrite relationVariables tempTransDir writeRelVars
    ncsWrite inclusionDependencies tempTransDir writeIncDeps
    ncsWrite atomFunctions (atomFuncsPath tempTransDir) (\ds fp v -> writeFuncs ds fp (HS.toList v))
    ncsWrite dbcFunctions (dbcFuncsPath tempTransDir) (\ds fp v -> writeFuncs ds fp (HS.toList v))
    ncsWrite notifications tempTransDir writeNotifications
    ncsWrite typeConstructorMapping tempTransDir writeTypeConstructorMapping
    ncsWrite registeredQueries tempTransDir writeRegisteredQueries
    ncsWrite acl tempTransDir writeAcl

    case subschemas trans of
      NotChangedSinceMarker{} -> pure ()
      ValueMarker sschemas -> 
        writeSubschemas sync tempTransDir sschemas
    
    writeSerialiseSync sync (transactionInfoPath tempTransDir) (transactionInfo trans)
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
unwrittenTransactionCanUseDatabaseContextValuesFastPath (UncommittedTransaction trans) =
  S.size (parentIds trans) == 1

-- | For database context values which have *not* changed since the parent, write into a single file.
{-
writeUnchangedTransactionMarkers :: DiskSync -> FilePath -> DatabaseContext -> IO ()
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
-}

writeRelVars :: DiskSync -> FilePath -> RelationVariables -> IO ()
writeRelVars sync transDir relvars = do
  let relvarsPath = relvarsDir transDir
      simpleInfoPath = relvarsSimplePath transDir
  --write unchanged relvars and file name mapping to "relvars" file
      (simplervs, complexrvs) = M.partition isSmallRelExpr relvars
      -- with the winery schema, one relational expression takes 28 KB to store, so we should do our best to compact expressions into one file
      isSmallRelExpr _ = True
--      isSmallRelExpr _ = False
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
          forConcurrently_ (M.toList writeRvMapExprs) $ \rvInfo -> do
            let rvpath = relvarsPath </> show rvnum
                rvExpr = snd (snd rvInfo)
                rvnum = fst (snd rvInfo)
            writeSerialiseSync sync rvpath rvExpr
-- Project:M36 does not read these extended attributes, but they might be useful for debugging or database restoration            
#ifdef USE_LINUX_XATTRS
            let rvname = fst rvInfo
            createUserXAttr rvpath xattrName (TE.encodeUtf8 rvname)
#endif            
    concurrently_ writeSimple writeComplex

{-
-- | Optimized code path to read one relvar expression from disk instead of all of them for a full transaction- useful for streaming results. Throws exception if the relvar name cannot be found since this function expects the database files to be coherent.
readOneRelVar :: FilePath -> RelVarName -> IO GraphRefRelationalExpr
readOneRelVar transDir rvName = do
  let relvarsIndex = relvarsSimplePath transDir
  rvindex <- readDeserialise relvarsIndex
  case M.lookup rvName (simpleRelVars rvindex) of
    Just rvexpr -> pure rvexpr
    Nothing -> do --look in complex rvs
      case M.lookup rvName (complexRelVarNameMap rvindex) of
        Nothing -> error $ "failed to find " <> T.unpack rvName <> " in filesystem."
        Just rvnum ->
          readDeserialise (relvarsDir transDir </> show rvnum)
-}

readNotChangedSinceDatabaseContext :: FilePath -> IO NotChangedSinceDatabaseContext
readNotChangedSinceDatabaseContext transDir = do
  let nscPath = notChangedSinceDatabaseContextPath transDir
  fExists <- doesFileExist nscPath
  if fExists then
    readDeserialise nscPath
  else
    pure $ NotChangedSinceDatabaseContext {
            ncsInclusionDependencies = Nothing,
            ncsRelationVariables = Nothing,
            ncsAtomFunctions = Nothing,
            ncsDbcFunctions = Nothing,
            ncsNotifications = Nothing,
            ncsTypeConstructorMapping = Nothing,
            ncsRegisteredQueries = Nothing,
            ncsSchemas = Nothing,
            ncsAcl = Nothing
    }

mkNotChangedSinceDatabaseContext :: DatabaseContext -> ValueMarker Subschemas -> NotChangedSinceDatabaseContext
mkNotChangedSinceDatabaseContext ctx mSubschemas =
  NotChangedSinceDatabaseContext {
  ncsInclusionDependencies = mkVal (inclusionDependencies ctx),
  ncsRelationVariables = mkVal (relationVariables ctx),
  ncsAtomFunctions = mkVal (atomFunctions ctx),
  ncsDbcFunctions = mkVal (dbcFunctions ctx),
  ncsNotifications = mkVal (notifications ctx),
  ncsTypeConstructorMapping = mkVal (typeConstructorMapping ctx),
  ncsRegisteredQueries = mkVal (registeredQueries ctx),
  ncsSchemas = mkVal mSubschemas,
  ncsAcl = mkVal (acl ctx)
  }
  where
    mkVal :: forall a. ValueMarker a -> Maybe TransactionId
    mkVal ValueMarker{} = Nothing
    mkVal (NotChangedSinceMarker tid) = Just tid
  
writeNotChangedSinceDatabaseContext :: DiskSync -> FilePath -> NotChangedSinceDatabaseContext -> IO ()
writeNotChangedSinceDatabaseContext diskSync transDir ncs = do
  let ncsPath = notChangedSinceDatabaseContextPath transDir
  writeSerialiseSync diskSync ncsPath ncs

readRelVars :: FilePath -> IO RelationVariables
readRelVars transDir = do
  let relvarsIndex = relvarsSimplePath transDir
  rvindex <- readDeserialise relvarsIndex
  complexRvAssocs <- forConcurrently (M.toList (complexRelVarNameMap rvindex)) $ \(rvname, rvpath) -> do
    rvExpr <- readDeserialise (relvarsDir transDir </> rvpath)
    pure (rvname, rvExpr)
  pure (simpleRelVars rvindex <> M.fromList complexRvAssocs)

writeFuncs :: Traversable t => DiskSync -> FilePath -> t (Function a acl) -> IO ()
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
      objInfo :: Function a acl -> Maybe ObjectFileInfo
      objInfo f =
        case funcBody f of
          FunctionObjectLoadedBody objPath modName entryFunc _ ->
            Just (ObjectFileInfo (objPath, modName, entryFunc))
          FunctionScriptBody{} -> Nothing
          FunctionBuiltInBody{} -> Nothing
  writeSerialiseSync sync funcWritePath (fmap functionData (toList funcs'))

readFuncs :: Default acl => FilePath -> FilePath -> HS.HashSet (Function a acl) -> Maybe ScriptSession -> IO (HS.HashSet (Function a acl))
readFuncs transDir funcPath precompiledFunctions mScriptSession = do
  funcsList <- readDeserialise funcPath
  --we always return the pre-compiled functions
  --load object files and functions in objects (shared libraries or flat object files)
  let objFilesDir = objectFilesPath transDir
  funcs <- mapM (\(funcType', funcName', mFuncScript, mObjInfo) -> 
                    loadFunc objFilesDir precompiledFunctions mScriptSession funcName' funcType' mFuncScript mObjInfo) funcsList
  pure (HS.union precompiledFunctions (HS.fromList funcs))

newtype ObjectFileInfo = ObjectFileInfo { _unFileInfo :: (FilePath, String, String) }
 deriving (Show, Serialise)
-- deriving Serialise via WineryVariant ObjectFileInfo

loadFunc :: Default acl => FilePath -> HS.HashSet (Function a acl) -> Maybe ScriptSession -> FunctionName -> [AtomType] -> Maybe FunctionBodyScript -> Maybe ObjectFileInfo -> IO (Function a acl)
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
                                                        funcBody = FunctionScriptBody _funcScript compiledScript,
                                                        funcACL = def}
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
  (funcType', mFuncScript) <- readDeserialise @([AtomType],Maybe T.Text) atomFuncPath
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
                                                    funcBody = FunctionScriptBody funcScript compiledScript,
                                                    funcACL = def }
#endif

writeIncDeps :: DiskSync -> FilePath -> M.Map IncDepName InclusionDependency -> IO ()  
writeIncDeps sync transDir incdeps = do
  traceBlock "write incdeps" $ writeSerialiseSync sync (incDepsDir transDir) incdeps
  
readIncDeps :: FilePath -> IO (M.Map IncDepName InclusionDependency)
readIncDeps transDir = do
  let incDepsPath = incDepsDir transDir
  readDeserialise incDepsPath
  
readSubschemas :: FilePath -> IO Subschemas
readSubschemas transDir = do
  let sschemasPath = subschemasPath transDir
  readDeserialise sschemasPath

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
  readDeserialise atPath
  
readRegisteredQueries :: FilePath -> IO RegisteredQueries
readRegisteredQueries transDir = do
  let regQsPath = registeredQueriesPath transDir
  readDeserialise regQsPath

writeRegisteredQueries :: DiskSync -> FilePath -> RegisteredQueries -> IO ()
writeRegisteredQueries sync transDir regQs = do
  let regQsPath = registeredQueriesPath transDir
  traceBlock "write registered queries" $ writeSerialiseSync sync regQsPath regQs

readNotifications :: FilePath -> IO Notifications
readNotifications transDir = do
  let notifsPath = notificationsPath transDir
  readDeserialise notifsPath

writeNotifications :: DiskSync -> FilePath -> Notifications -> IO ()
writeNotifications sync transDir notifs = do
  let notifsPath = notificationsPath transDir
  traceBlock "write notifications" $ writeSerialiseSync sync notifsPath notifs

readAcl :: FilePath -> IO DatabaseContextACL
readAcl transDir = do
  let aclPath' = aclPath transDir
  readDeserialise aclPath'

writeAcl :: DiskSync -> FilePath -> DatabaseContextACL -> IO ()
writeAcl sync transDir acl' = do
  let aclPath' = aclPath transDir
  traceBlock "write acl" $ writeSerialiseSync sync aclPath' acl'


