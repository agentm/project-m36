{-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.ProjectM36 where
import Database.Persist hiding (Assign, Update)
import qualified Database.Persist as DP
import ProjectM36.Base
import ProjectM36.Tuple
import qualified ProjectM36.Client as C
import qualified Data.UUID as U
import Data.UUID.V4 (nextRandom)
import qualified Data.Vector as V
import ProjectM36.Error
import ProjectM36.Relation
import qualified Control.Monad.IO.Class as Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ask, ReaderT)
import Control.Exception (throw, throwIO)
import qualified Data.Text as T
import ProjectM36.Atom 
import Data.Aeson (FromJSON(..), ToJSON(..), withText, withObject, (.:))
import qualified Data.Map as M
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Aeson.Types (modifyFailure)
import Control.Monad.Reader (runReaderT)
import Web.PathPieces (PathPiece (..))
import qualified Database.Persist.Sql as Sql
import Control.Monad.Trans.Either
import qualified Database.Persist.Types as DPT
import qualified Data.Set as S
import qualified Data.Conduit.List as CL
import Data.Typeable (typeRep, typeOf, Proxy(..))
import Data.Time.Calendar (Day)
import Data.ByteString (ByteString)
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..), parseUrlPieceWithPrefix, readTextData)

type ProjectM36Backend = (C.SessionId, C.Connection)

--convert a PersistEntity to a RelationTuple
recordAsTuple :: forall record. (PersistEntity record, PersistEntityBackend record ~ ProjectM36Backend)
            => Maybe U.UUID -> record -> Either RelationalError RelationTuple
recordAsTuple uuid record = do -- if the uuid is passed in, set the idDBName attribute
  let entInfo = zip entFields entValues
      entFields = embeddedFields $ toEmbedEntityDef (entityDef $ Just record)
      entValues = map toPersistValue $ toPersistFields record
  attrVec <- recordAttributes entInfo record
  atomVec <- recordAsAtoms uuid attrVec record
  return $ RelationTuple attrVec atomVec

recordAttributes :: forall record. (PersistEntity record, PersistEntityBackend record ~ ProjectM36Backend)
           => [(EmbedFieldDef, PersistValue)] -> record -> Either RelationalError Attributes
recordAttributes entInfo record = do
  let convertAttr = uncurry fieldDefAsAttribute
      idDBName = unDBName $ fieldDB $ entityId (entityDef $ Just record)
  attrList <- mapM convertAttr entInfo
  return $ V.fromList ([Attribute idDBName stringAtomType] ++ attrList)

fieldDefAsAttribute :: EmbedFieldDef -> PersistValue -> Either RelationalError Attribute
fieldDefAsAttribute fieldDef pVal = case persistValueAtomType pVal of
  Nothing -> Left $ AtomTypeNotSupported attrName
  Just fType -> Right $ Attribute attrName fType
  where
    attrName = unDBName $ emFieldDB fieldDef

persistValueAtomType :: PersistValue -> Maybe AtomType
persistValueAtomType val = do
  atom <- persistValueAtom val
  return $ atomTypeForAtom atom

persistValueAtom :: PersistValue -> Maybe Atom
persistValueAtom val = case val of
  PersistText v -> return $ Atom v
  PersistInt64 v -> return $ Atom ((fromIntegral v)::Int)
  PersistBool v -> return $ Atom v
  PersistDay v -> return $ Atom v
  _ -> Nothing

atomAsPersistValue :: Atom -> PersistValue
atomAsPersistValue atom@(Atom atomv) = if typeRep (Proxy :: Proxy Int) == typeOf atomv then
                                         PersistInt64 (fromIntegral ((unsafeCast atom) :: Int))
                                       else if typeRep (Proxy :: Proxy T.Text) == typeOf atomv then
                                              PersistText (unsafeCast atom)
                                            else if typeRep (Proxy :: Proxy Bool) == typeOf atomv then
                                                   PersistBool (unsafeCast atom)
                                                 else if typeRep (Proxy :: Proxy Day) == typeOf atomv then
                                                        PersistDay (unsafeCast atom)
                                                      else if typeRep (Proxy :: Proxy ByteString) == typeOf atomv then
                                                             PersistByteString (unsafeCast atom)
                                                           else
                                                             error "missing conversion"

recordAsAtoms :: forall record. (PersistEntity record, PersistEntityBackend record ~ ProjectM36Backend)
           => Maybe U.UUID -> Attributes -> record -> Either RelationalError (V.Vector Atom)
recordAsAtoms freshUUID _ record = do
  let pValues = map toPersistValue $ toPersistFields record
      valAtom val = case persistValueAtom val of
        Nothing -> Left $ AtomTypeNotSupported ""
        Just atom -> Right atom
  atoms <- mapM valAtom pValues
  return $ V.fromList $ case freshUUID of
    Nothing -> atoms
    Just uuid -> [Atom $ T.pack (U.toString uuid)] ++ atoms

--used by insert operations
recordsAsRelation :: forall record. (PersistEntity record, PersistEntityBackend record ~ ProjectM36Backend) =>
                     [(Maybe U.UUID, record)] -> Either RelationalError Relation
recordsAsRelation [] = Left EmptyTuplesError
recordsAsRelation recordZips = do
  tupleList <- mapM (uncurry recordAsTuple) recordZips
  let oneTuple = head tupleList
  mkRelation (tupleAttributes oneTuple) (RelationTupleSet tupleList)

keyFromValuesOrDie :: (Trans.MonadIO m,
                       PersistEntity record,
                       PersistEntityBackend record ~ ProjectM36Backend
                       ) => [PersistValue] -> m (Key record)
keyFromValuesOrDie val = case keyFromValues val of
  Right k -> return k
  Left _ -> Trans.liftIO $ throwIO $ PersistError "key pooped"

insertNewRecords :: (Trans.MonadIO m,
                     PersistEntity record,
                     PersistEntityBackend record ~ ProjectM36Backend) =>
                    [U.UUID] -> [record] -> ReaderT ProjectM36Backend m [Key record]
insertNewRecords uuids records = do
  let recordsZip = zip (map Just uuids) records
      insertExpr = Insert (relVarNameFromRecord $ head records)
  case recordsAsRelation recordsZip of
      Left err -> throw $ PersistError (T.pack $ show err)
      Right rel -> do
        (sessionId, conn) <- ask
        Trans.liftIO $ executeDatabaseContextExpr sessionId conn (insertExpr (ExistingRelation rel))
        mapM (\uuid -> keyFromValuesOrDie [PersistText $ T.pack (U.toString uuid)]) uuids

throwIOPersistError :: (Trans.MonadIO m) => String -> m a
throwIOPersistError msg = Trans.liftIO $ throwIO $ PersistError (T.pack msg)

lookupByKey :: forall record m.
               (Trans.MonadIO m,
                PersistEntity record,
                PersistEntityBackend record ~ ProjectM36Backend)
           => Key record -> ReaderT ProjectM36Backend m (Maybe (Entity record))
lookupByKey key = do
  (relVarName, restrictionPredicate) <- commonKeyQueryProperties key
  let query = Restrict restrictionPredicate (RelationVariable relVarName)
  (sessionId, conn) <- ask
  resultRel <- Trans.liftIO $ C.executeRelationalExpr sessionId conn query
  case resultRel of
    Left err -> throwIOPersistError ("executeRelationExpr error: " ++ show err)
    Right resultRel' -> case singletonTuple resultRel' of
      Nothing -> return Nothing --no match on key
      Just tuple -> do
        let entityInfo = entityDefFromKey key
        entity <- fromPersistValuesThrow entityInfo tuple --correct, one match
        return $ Just entity

fromPersistValuesThrow :: (Trans.MonadIO m,
                           PersistEntity record,
                           PersistEntityBackend record ~ ProjectM36Backend) => EntityDef -> RelationTuple -> m (Entity record)
fromPersistValuesThrow entDef tuple = do
  let body = fromPersistValues $ map getValue (entityFields entDef)
      getValue field = case atomForAttributeName (unDBName (fieldDB field)) tuple of
        Right atom -> atomAsPersistValue atom
        Left err -> throw $ PersistError ("missing field: " `T.append` (T.pack $ show err))
      keyAtom = atomForAttributeName (unDBName (fieldDB (entityId entDef))) tuple
  case keyAtom of
    Left err -> throwIOPersistError ("missing key atom" ++ show err)
    Right keyAtom' -> case keyFromValues [atomAsPersistValue keyAtom'] of
      Left err -> throw $ PersistError ("key failure" `T.append` (T.pack $ show err))
      Right key -> case body of
        Left err -> throwIOPersistError (show err)
        Right body' -> return $ Entity key body'

commonKeyQueryProperties :: (Trans.MonadIO m, PersistEntityBackend val ~ ProjectM36Backend, PersistEntity val) => Key val -> ReaderT ProjectM36Backend m (RelVarName, RestrictionPredicateExpr)
commonKeyQueryProperties key = do
  let matchUUID = keyToUUID key
      keyAttributeName = unDBName (fieldDB $ entityId entityInfo)
      relVarName = unDBName $ entityDB entityInfo
      matchUUIDText = T.pack $ U.toString matchUUID
      entityInfo = entityDefFromKey key
      restrictionPredicate = AttributeEqualityPredicate keyAttributeName (NakedAtomExpr (Atom matchUUIDText))
  return (relVarName, restrictionPredicate)

deleteByKey :: (Trans.MonadIO m, PersistEntityBackend val ~ ProjectM36Backend, PersistEntity val) => Key val -> ReaderT ProjectM36Backend m (Bool)
deleteByKey key = do
  (relVarName, restrictionPredicate) <- commonKeyQueryProperties key
  let query = Delete relVarName restrictionPredicate
  (sessionId, conn) <- ask
  maybeErr <- Trans.liftIO $ C.executeDatabaseContextExpr sessionId conn query
  return $ isJust maybeErr

--convert persistent update list to ProjectM36 Update map
updatesToUpdateMap :: (Trans.MonadIO m,
                       PersistEntityBackend val ~ ProjectM36Backend,
                       PersistEntity val) =>
                      [DP.Update val] -> ReaderT ProjectM36Backend m (M.Map AttributeName Atom)
updatesToUpdateMap updates = do
  let convertMap upd = case updateUpdate upd of
        DP.Assign -> let attrName = unDBName $ fieldDB $ updateFieldDef upd
                         newAttrValue = persistValueAtom (updatePersistValue upd) in
                     case newAttrValue of
                       Nothing -> Trans.liftIO $ throwIO $ PersistError "atom conversion failed"
                       Just newAttrValue' -> return (attrName, newAttrValue')
        _ -> Trans.liftIO $ throwIO $ PersistError "update type not supported"
  updateAtomList <- mapM convertMap updates
  return $ M.fromList updateAtomList

updateByKey :: (Trans.MonadIO m, PersistEntityBackend val ~ ProjectM36Backend, PersistEntity val) => Key val -> [DP.Update val] -> ReaderT ProjectM36Backend m ()
updateByKey key updates = do
  (relVarName, restrictionPredicate) <- commonKeyQueryProperties key
  updateMap <- updatesToUpdateMap updates
  let query = Update relVarName updateMap restrictionPredicate
  (sessionId, conn) <- ask
  maybeErr <- Trans.liftIO $ C.executeDatabaseContextExpr sessionId conn query
  case maybeErr of
    Just err -> throwIOPersistError ("executeDatabaseExpr error" ++ show err)
    Nothing -> return ()

--set the truth unconditionally
repsertByKey :: (Trans.MonadIO m, ProjectM36Backend ~ PersistEntityBackend val, PersistEntity val) => Key val -> val -> ReaderT ProjectM36Backend m ()
repsertByKey key val = do
  _ <- delete key
  insertKey key val

replaceByKey :: (Trans.MonadIO m, ProjectM36Backend ~ PersistEntityBackend val, PersistEntity val) => Key val -> val -> ReaderT ProjectM36Backend m ()
replaceByKey key val = do
  deletionSuccess <- deleteByKey key
  when (not deletionSuccess) (Trans.liftIO $ throwIO $ PersistError "entity missing during replace")
  insertKey key val

entityDefFromKey :: PersistEntity record => Key record -> EntityDef
entityDefFromKey = entityDef . Just . recordTypeFromKey

dummyFromKey :: Key record -> Maybe record
dummyFromKey = Just . recordTypeFromKey

recordTypeFromKey :: Key record -> record
recordTypeFromKey _ = error "dummyFromKey"

dummyFromUnique :: Unique v -> Maybe v
dummyFromUnique _ = Nothing

dummyFromFilter :: Filter v -> Maybe v
dummyFromFilter _ = Nothing

dummyFromFilters :: [Filter v] -> Maybe v
dummyFromFilters _ = Nothing

dummyFromUpdate :: DPT.Update v -> Maybe v
dummyFromUpdate _ = Nothing

keyToUUID:: (PersistEntity record) => Key record -> U.UUID
keyToUUID key = case keyToValues key of
  ([PersistText uuidText]) -> case U.fromString (T.unpack uuidText) of
    Nothing -> error "error reading uuid from text in key construction"
    Just uuid -> uuid
  _ -> error "unexpected persist value in key construction"

instance ToHttpApiData (BackendKey ProjectM36Backend) where
  toUrlPiece = T.pack . U.toString . unPM36Key
    
instance FromHttpApiData (BackendKey ProjectM36Backend) where    
  parseUrlPiece input = do
    s <- parseUrlPieceWithPrefix "o" input <!> return input
    ProjectM36Key <$> readTextData s
      where
        infixl 3 <!>
        Left _ <!> y = y
        x      <!> _ = x
    
instance PersistStore ProjectM36Backend where
  newtype BackendKey ProjectM36Backend = ProjectM36Key { unPM36Key :: U.UUID }
          deriving (Show, Read, Eq, Ord, PersistField)

  insertMany [] = return []
  insertMany records = do
    freshUUIDs <- Trans.liftIO $ mapM (\_-> nextRandom) records
    insertNewRecords freshUUIDs records

  insert record = do
   keys <- insertMany [record]
   return $ head keys

  --insertEntityMany [] = return []
  --insertKey :: (Trans.MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => Key val -> val -> ReaderT backend m ()
  insertKey key record = do
    let uuid = keyToUUID key
    _ <- insertNewRecords [uuid] [record]
    return ()

  --get :: (MonadIO m, backend ~ PersistEntityBackend val, PersistEntity val) => Key val -> ReaderT backend m (Maybe val)
  get key = do
    ent <- lookupByKey key
    case ent of
      Nothing -> return Nothing
      Just (Entity _ val) -> return $ Just val

  repsert = repsertByKey
    --delete followed by insert "set the record as the truth

  delete key = deleteByKey key >> return ()

  replace = replaceByKey

  --update :: (MonadIO m, PersistEntity val, backend ~ PersistEntityBackend val) => Key val -> [Update val] -> ReaderT backend m ()
  update = updateByKey

instance FromJSON (BackendKey ProjectM36Backend) where
  parseJSON = withText "ProjectM36Key" $ \t -> maybe (fail "Invalid UUID") (return . ProjectM36Key) (U.fromString (T.unpack t))

instance ToJSON (BackendKey ProjectM36Backend) where
  toJSON (ProjectM36Key uuid) = toJSON $ U.toString uuid

--wrapped version which throws exceptions
executeDatabaseContextExpr :: C.SessionId -> C.Connection -> DatabaseExpr -> IO ()
executeDatabaseContextExpr sessionId conn expr = do
  res <- C.executeDatabaseContextExpr sessionId conn expr
  case res of
    Nothing -> return ()
    Just err -> throwIO (PersistError $ T.pack (show err))

relVarNameFromRecord :: (PersistEntity record, PersistEntityBackend record ~ ProjectM36Backend)
               => record -> RelVarName
relVarNameFromRecord = unDBName . entityDB . entityDef . Just

{- unfortunately copy-pasted from Database.Persist.Sql.Orphan.PersistStore -}
updateFieldDef :: PersistEntity v => DP.Update v -> FieldDef
updateFieldDef (DP.Update f _ _) = persistFieldDef f
updateFieldDef (BackendUpdate {}) = error "updateFieldDef did not expect BackendUpdate"

updatePersistValue :: DP.Update v -> PersistValue
updatePersistValue (DP.Update _ v _) = toPersistValue v
updatePersistValue (BackendUpdate {}) = error "updatePersistValue did not expect BackendUpdate"

instance PersistField U.UUID where
  toPersistValue val = PersistText $ T.pack (U.toString val)
  fromPersistValue (PersistText uuidText) = case U.fromString $ T.unpack uuidText of
    Nothing -> Left "uuid text read failure"
    Just uuid -> Right uuid
  fromPersistValue _ = Left $ T.pack "expected PersistObjectId"

instance FromJSON C.ConnectionInfo where
         parseJSON v = modifyFailure ("Persistent: error loading ProjectM36 conf: " ++) $
           flip (withObject "ProjectM36Conf") v $ \o -> do
           ctype <- parseJSON =<< ( o .: "connectionType")
           return ctype

instance PersistConfig C.ConnectionInfo where
  --- | Hold onto a connection as well a default session.
         type PersistConfigBackend C.ConnectionInfo = ReaderT ProjectM36Backend
         type PersistConfigPool C.ConnectionInfo = ProjectM36Backend

         loadConfig = parseJSON
         applyEnv = return -- no environment variables are used
         createPoolConfig conf = do
           connErr <- C.connectProjectM36 conf
           case connErr of
               Left err -> throwIO $ PersistError ("Failed to create connection: " `T.append` (T.pack $ show err))
               Right conn -> do
                 eSession <- C.createSessionAtHead "master" conn
                 case eSession of
                   Left err -> throwIO $ PersistError ("Failed to create session: " `T.append` (T.pack $ show err))
                   Right sessionId -> pure (sessionId, conn)
         --runPool :: (MonadBaseControl IO m, MonadIO m) => c -> PersistConfigBackend c m a -> PersistConfigPool c -> m a
         runPool _ r = runReaderT r

withProjectM36Conn :: (Monad m, Trans.MonadIO m) => C.ConnectionInfo -> (ProjectM36Backend -> m a) -> m a
withProjectM36Conn conf connReader = do
    backend <- Trans.liftIO $ createPoolConfig conf
    connReader backend

runProjectM36Conn :: (Trans.MonadIO m) => ReaderT ProjectM36Backend m a -> (C.SessionId, C.Connection) -> m a
runProjectM36Conn m1 (sessionId, conn) = runReaderT m1 (sessionId, conn)

instance PathPiece (BackendKey ProjectM36Backend) where
    toPathPiece (ProjectM36Key uuid) = U.toText uuid
    fromPathPiece txt = do
      uuid <- U.fromText txt
      return $ ProjectM36Key uuid

instance Sql.PersistFieldSql U.UUID where
    sqlType _ = Sql.SqlOther "doesn't make much sense for ProjectM36"

instance Sql.PersistFieldSql (BackendKey ProjectM36Backend) where
    sqlType _ = Sql.SqlOther "doesn't make much sense for ProjectM36"

persistUniqueToRestrictionPredicate :: PersistEntity record => Unique record -> Either RelationalError RestrictionPredicateExpr
persistUniqueToRestrictionPredicate unique = do
    atoms <- mapM (\v -> maybe (Left $ AtomTypeNotSupported "") Right $ persistValueAtom v) $ persistUniqueToValues unique
    let attrNames = map (unDBName . snd) $ persistUniqueToFieldNames unique
        andify [] = TruePredicate
        andify ((attrName, atom):xs) = AndPredicate (AttributeEqualityPredicate attrName (NakedAtomExpr atom)) $ andify xs
    return $ andify (zip attrNames atoms)

instance PersistUnique ProjectM36Backend where
    getBy unique = do
        (sessionId, conn) <- ask
        let predicate = persistUniqueToRestrictionPredicate unique
            relVarName = unDBName $ entityDB entDef
            entDef = entityDef $ dummyFromUnique unique
            throwRelErr err = Trans.liftIO $ throwIO (PersistError (T.pack $ show err))
        case predicate of
            Left err -> throwRelErr err
            Right predicate' -> do
                let restrictExpr = Restrict predicate' (RelationVariable relVarName)
                singletonRel <- Trans.liftIO $ C.executeRelationalExpr sessionId conn restrictExpr
                case singletonRel of
                    Left err -> throwRelErr err
                    Right singletonRel' -> do
                        if cardinality singletonRel' == Finite 0 then
                            return Nothing
                            else if cardinality singletonRel' > Finite 1 then
                                Trans.liftIO $ throwIO (PersistError "getBy returned more than one tuple")
                                else -- exactly one tuple
                                    case singletonTuple singletonRel' of
                                        Nothing -> Trans.liftIO $ throwIO (PersistMarshalError "singletonTuple failure")
                                        Just tuple -> do
                                            newEnt <- Trans.liftIO $ fromPersistValuesThrow entDef tuple
                                            return $ Just newEnt
    deleteBy unique = do
        (sessionId, conn) <- ask
        let predicate = persistUniqueToRestrictionPredicate unique
            relVarName = unDBName $ entityDB entDef
            entDef = entityDef $ dummyFromUnique unique
            throwRelErr err = Trans.liftIO $ throwIO (PersistError (T.pack $ show err))
        case predicate of
            Left err -> throwRelErr err
            Right predicate' -> do
                let deleteExpr = Delete relVarName predicate'
                maybeErr <- Trans.liftIO $ C.executeDatabaseContextExpr sessionId conn deleteExpr
                case maybeErr of
                   Just err -> throwRelErr err
                   Nothing -> return ()

multiFilterAsRestrictionPredicate :: (PersistEntity val, PersistEntityBackend val ~ ProjectM36Backend) => Bool -> [Filter val] -> Either RelationalError RestrictionPredicateExpr
multiFilterAsRestrictionPredicate _ [] = Right $ TruePredicate
multiFilterAsRestrictionPredicate andOr (x:xs) = do
    let predicate = if andOr then AndPredicate else OrPredicate
    filtHead <- filterAsRestrictionPredicate x
    filtTail <- multiFilterAsRestrictionPredicate andOr xs
    return $ predicate filtHead filtTail

filterValueToPersistValues :: forall a. PersistField a => Either a [a] -> [PersistValue]
filterValueToPersistValues v = map toPersistValue $ either return id v

filterAsRestrictionPredicate :: (PersistEntity val, PersistEntityBackend val ~ ProjectM36Backend) => Filter val -> Either RelationalError RestrictionPredicateExpr
filterAsRestrictionPredicate filterIn = case filterIn of
    FilterAnd filters -> multiFilterAsRestrictionPredicate True filters
    FilterOr filters -> multiFilterAsRestrictionPredicate False filters
    BackendFilter _ -> error "BackendFilter not supported"
    Filter field value pfilter -> let attrName = unDBName $ fieldDB (persistFieldDef field) in
                                  case value of
                                      Left val -> let atom = persistValueAtom $ toPersistValue val in
                                        case pfilter of
                                          Eq -> case atom of
                                                    Nothing -> Left $ AtomTypeNotSupported attrName
                                                    Just atom' -> Right $ AttributeEqualityPredicate attrName (NakedAtomExpr atom')
                                          Ne -> case atom of
                                                    Nothing -> Left $ AtomTypeNotSupported attrName
                                                    Just atom' -> Right $ NotPredicate (AttributeEqualityPredicate attrName (NakedAtomExpr atom'))
                                          op -> Left $ AtomOperatorNotSupported $ T.pack (show op)
                                      Right _ -> Left $ AtomTypeNotSupported attrName

updateToUpdateTuple :: (PersistEntity val, PersistEntityBackend val ~ ProjectM36Backend) => DPT.Update val -> Either RelationalError (AttributeName, Atom)
updateToUpdateTuple (BackendUpdate _) = error "BackendUpdate not supported"
updateToUpdateTuple (DPT.Update field value op) = let attrName = unDBName $ fieldDB (persistFieldDef field)
                                                      atom = persistValueAtom $ toPersistValue value
                                                  in case op of
                                                       DPT.Assign -> case atom of
                                                         Nothing -> Left $ AtomTypeNotSupported attrName
                                                         Just atom' -> Right $ (attrName, atom')
                                                       op' -> Left $ AtomOperatorNotSupported $ T.pack (show op')

selectionFromRestriction :: (PersistEntity val, PersistEntityBackend val ~ backend, Trans.MonadIO m, backend ~ ProjectM36Backend) => [Filter val] -> ReaderT backend m (Either RelationalError RelationalExpr)
selectionFromRestriction filters = do
    runEitherT $ do
        restrictionPredicate <- hoistEither $ multiFilterAsRestrictionPredicate True filters
        let entDef = entityDef $ dummyFromFilters filters
            relVarName = unDBName $ entityDB entDef
        right $ Restrict restrictionPredicate (RelationVariable relVarName)


instance PersistQuery ProjectM36Backend where
         updateWhere _ [] = return ()
         updateWhere filters updates = do
             (sessionId, conn) <- ask
             e <- runEitherT $ do
                 restrictionPredicate <- hoistEither $ multiFilterAsRestrictionPredicate True filters
                 tuples <- hoistEither $ mapM updateToUpdateTuple updates
                 let updateMap = M.fromList tuples
                     entDef = entityDef $ dummyFromUpdate (head updates)
                     relVarName = unDBName $ entityDB entDef
                     updateExpr = Update relVarName updateMap restrictionPredicate
                 maybeErr <- Trans.liftIO $ C.executeDatabaseContextExpr sessionId conn updateExpr
                 case maybeErr of
                     Just err -> left err
                     Nothing -> right ()
             case e of
               Left err -> throwIOPersistError ("updateWhere failure: " ++ show err)
               Right () -> return ()

         deleteWhere filters = do
             (sessionId, conn) <- ask
             e <- runEitherT $ do
                 restrictionPredicate <- hoistEither $ multiFilterAsRestrictionPredicate True filters
                 let entDef = entityDef $ dummyFromFilters filters
                     relVarName = unDBName $ entityDB entDef
                     deleteExpr = Delete relVarName restrictionPredicate
                 maybeErr <- Trans.liftIO $ C.executeDatabaseContextExpr sessionId conn deleteExpr
                 case maybeErr of
                     Just err -> left err
                     Nothing -> right ()
             case e of
               Left err -> throwIOPersistError ("deleteWhere failure: " ++ show err)
               Right () -> return ()

         count filters = do
             (sessionId, conn) <- ask
             e <- runEitherT $ do
                 restrictionPredicate <- hoistEither $ multiFilterAsRestrictionPredicate True filters
                 let entDef = entityDef $ dummyFromFilters filters
                     relVarName = unDBName $ entityDB entDef
                     allAttrNamesList = map (unDBName . fieldDB) (entityFields entDef) ++ [unDBName (fieldDB (entityId entDef))]
                     allAttrNames = AttributeNames $ S.fromList allAttrNamesList
                     groupExpr = Group allAttrNames "persistcountrel" (Restrict restrictionPredicate (RelationVariable relVarName))
                     tupleExpr = AttributeTupleExpr "persistcount" (FunctionAtomExpr "count" [AttributeAtomExpr "persistcountrel"])
                     countExpr = Extend tupleExpr groupExpr
                 rel <- Trans.liftIO $ C.executeRelationalExpr sessionId conn countExpr
                 case rel of
                    Left err -> left err
                    Right rel' -> case singletonTuple rel' of
                          Nothing -> Trans.liftIO $ throwIO $ PersistError "failed to get count tuple"
                          Just tuple -> case atomForAttributeName "persistcount" tuple of
                             (Right c) -> return (castInt c)
                             Left err -> left err
             case e of
               Left err -> throwIOPersistError ("count failure: " ++ show err)
               Right c -> return c

         --no select options currently supported (sorting, limiting)
         selectSourceRes _ (_:_) = Trans.liftIO $ throwIO $ PersistError "select options not yet supported"
         selectSourceRes filters [] = do
             (sessionId, conn) <- ask
             entities <- runEitherT $ do
                 restrictionExpr <- (lift $ selectionFromRestriction filters) >>= hoistEither
                 --restrictionExpr' <- hoistEither restrictionExpr
                 let entDef = entityDef $ dummyFromFilters filters
                 let tupleMapper tuple = Trans.liftIO $ fromPersistValuesThrow entDef tuple
                 rel <- Trans.liftIO $ C.executeRelationalExpr sessionId conn restrictionExpr
                 case rel of
                     Left err -> Trans.liftIO $ throwIO $ PersistError (T.pack (show err))
                     Right (Relation _ tupleSet) -> mapM tupleMapper $ asList tupleSet --refactor to use some relation accessors- here we convert a relation to a matrix, effectively (especially to support future sorting)
             case entities of
                 Left err -> Trans.liftIO $ throwIO $ PersistError (T.pack $ show err)
                 Right entities' -> return $ return $ CL.sourceList entities'

         selectKeysRes _ (_:_) = Trans.liftIO $ throwIO $ PersistError "select options not yet supported"
         selectKeysRes filters [] = do
            (sessionId, conn) <- ask
            keys <- runEitherT $ do
               restrictionExpr <- (lift $ selectionFromRestriction filters) >>= hoistEither
               let keyAttrNames = ["id"] --no support for multi-attribute keys yet
                   keyExpr = Project (AttributeNames $ S.fromList keyAttrNames) restrictionExpr
               (Relation _ tupleSet) <- (Trans.liftIO $ C.executeRelationalExpr sessionId conn keyExpr) >>= hoistEither
               let keyMapper :: (PersistEntity record, Trans.MonadIO m) => RelationTuple -> EitherT RelationalError (ReaderT ProjectM36Backend m) (Key record)
                   keyMapper tuple = do
                                       atoms <- hoistEither (atomsForAttributeNames (V.fromList keyAttrNames) tuple)
                                       case keyFromValues (map atomAsPersistValue (V.toList atoms)) of
                                           Left err -> throwIOPersistError ("keyFromValues failure: " ++ show err)
                                           Right key -> right key
               mapM keyMapper $ asList tupleSet
            case keys of
                Left err -> throwIOPersistError ("keyFromValues failure2: " ++ show err)
                Right keys' -> return $ return (CL.sourceList keys')

