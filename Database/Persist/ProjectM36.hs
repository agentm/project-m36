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
import Control.Monad.Trans.Reader (ask, ReaderT)
import Control.Exception (throw, throwIO)
import qualified Data.Text as T
import ProjectM36.Atom
import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Control.Monad (when)
import Data.Maybe (isJust)
      
type ProjectM36Backend = C.Connection  
                         
--convert a PersistEntity to a RelationTuple
recordAsTuple :: forall record. (PersistEntity record, PersistEntityBackend record ~ C.Connection)
            => Maybe U.UUID -> record -> Either RelationalError RelationTuple
recordAsTuple uuid record = do -- if the uuid is passed in, set the idDBName attribute
  let entInfo = zip entFields entValues
      entFields = embeddedFields $ toEmbedEntityDef (entityDef $ Just record)
      entValues = map toPersistValue $ toPersistFields record
  attrVec <- recordAttributes entInfo record
  atomVec <- recordAsAtoms uuid attrVec record
  return $ RelationTuple attrVec atomVec
      
recordAttributes :: forall record. (PersistEntity record, PersistEntityBackend record ~ C.Connection)
           => [(EmbedFieldDef, PersistValue)] -> record -> Either RelationalError Attributes
recordAttributes entInfo record = do
  let convertAttr = uncurry fieldDefAsAttribute
      idDBName = unDBName $ fieldDB $ entityId (entityDef $ Just record)
  attrList <- mapM convertAttr entInfo
  return $ V.fromList ([Attribute idDBName StringAtomType] ++ attrList)
      
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
  PersistText v -> return $ StringAtom v
  PersistInt64 v -> return $ IntAtom (fromIntegral v)
  PersistBool v -> return $ BoolAtom v
  PersistDay v -> return $ DateAtom v
  _ -> Nothing
  
atomAsPersistValue :: Atom -> PersistValue  
atomAsPersistValue a = case a of 
  StringAtom v -> PersistText v
  IntAtom v -> PersistInt64 (fromIntegral v)
  BoolAtom v -> PersistBool v
  DateAtom v -> PersistDay v
  _ -> error "missing conversion"
  
recordAsAtoms :: forall record. (PersistEntity record, PersistEntityBackend record ~ C.Connection)
           => Maybe U.UUID -> Attributes -> record -> Either RelationalError (V.Vector Atom)
recordAsAtoms freshUUID _ record = do
  let pValues = map toPersistValue $ toPersistFields record
      valAtom val = case persistValueAtom val of
        Nothing -> Left $ AtomTypeNotSupported ""
        Just atom -> Right atom
  atoms <- mapM valAtom pValues
  return $ V.fromList $ case freshUUID of 
    Nothing -> atoms
    Just uuid -> [StringAtom $ T.pack (U.toString uuid)] ++ atoms
  
--used by insert operations
recordsAsRelation :: forall record. (PersistEntity record, PersistEntityBackend record ~ C.Connection) => 
                     [(Maybe U.UUID, record)] -> Either RelationalError Relation
recordsAsRelation [] = Left EmptyTuplesError
recordsAsRelation recordZips = do
  tupleList <- mapM (uncurry recordAsTuple) recordZips
  let oneTuple = head tupleList
  mkRelation (tupleAttributes oneTuple) (HS.fromList tupleList)
     
keyFromValuesOrDie :: (Trans.MonadIO m, 
                       PersistEntity record, 
                       PersistEntityBackend record ~ C.Connection
                       ) => [PersistValue] -> m (Key record)
keyFromValuesOrDie val = case keyFromValues val of
  Right k -> return k
  Left _ -> Trans.liftIO $ throwIO $ PersistError "key pooped"
  
insertNewRecords :: (Trans.MonadIO m,
                     PersistEntity record,
                     PersistEntityBackend record ~ C.Connection) =>
                    [U.UUID] -> [record] -> ReaderT C.Connection m [Key record]
insertNewRecords uuids records = do
  let recordsZip = zip (map Just uuids) records  
      insertExpr = Insert (relVarNameFromRecord $ head records) 
  case recordsAsRelation recordsZip of
      Left err -> throw $ PersistError (T.pack $ show err)
      Right rel -> do               
        conn <- ask
        Trans.liftIO $ executeDatabaseContextExpr conn (insertExpr (ExistingRelation rel))
        mapM (\uuid -> keyFromValuesOrDie [PersistText $ T.pack (U.toString uuid)]) uuids
        
lookupByKey :: forall record m.
               (Trans.MonadIO m, 
                PersistEntity record, 
                PersistEntityBackend record ~ C.Connection)
           => Key record -> ReaderT C.Connection m (Maybe (Entity record))
lookupByKey key = do
  (relVarName, restrictionPredicate) <- commonKeyQueryProperties key
  let query = Restrict restrictionPredicate (RelationVariable relVarName)
  conn <- ask
  resultRel <- Trans.liftIO $ C.executeRelationalExpr conn query
  case resultRel of
    Left err -> Trans.liftIO $ throwIO $ PersistError "executeRelationExpr error"
    Right resultRel' -> case singletonTuple resultRel' of
      Nothing -> return Nothing --no match on key
      Just tuple -> do 
        let entityInfo = entityDefFromKey key              
        entity <- fromPersistValuesThrow entityInfo tuple --correct, one match
        return $ Just entity
    
fromPersistValuesThrow :: (Trans.MonadIO m, 
                           PersistEntity record, 
                           PersistEntityBackend record ~ C.Connection) => EntityDef -> RelationTuple -> m (Entity record)
fromPersistValuesThrow entDef tuple = do
  let body = fromPersistValues $ map getValue (entityFields entDef)
      getValue field = case atomForAttributeName (unDBName (fieldDB field)) tuple of
        Right atom -> atomAsPersistValue atom
        Left err -> throw $ PersistError "missing field"
      keyAtom = atomForAttributeName (unDBName (fieldDB (entityId entDef))) tuple
  case keyAtom of
    Left err -> Trans.liftIO $ throwIO $ PersistError "missing key atom"
    Right keyAtom' -> case keyFromValues [atomAsPersistValue keyAtom'] of
      Left err -> throw $ PersistError "key failure"
      Right key -> case body of
        Left err -> Trans.liftIO $ throwIO $ PersistError err
        Right body' -> return $ Entity key body'
        
commonKeyQueryProperties :: (Trans.MonadIO m, PersistEntityBackend val ~ C.Connection, PersistEntity val) => Key val -> ReaderT C.Connection m (RelVarName, RestrictionPredicateExpr)
commonKeyQueryProperties key = do        
  let matchUUID = keyToUUID key
      keyAttributeName = unDBName (fieldDB $ entityId entityInfo)
      relVarName = unDBName $ entityDB entityInfo      
      matchUUIDText = T.pack $ U.toString matchUUID
      entityInfo = entityDefFromKey key      
      restrictionPredicate = AttributeEqualityPredicate keyAttributeName (NakedAtomExpr (StringAtom matchUUIDText))
  return (relVarName, restrictionPredicate)
        
deleteByKey :: (Trans.MonadIO m, PersistEntityBackend val ~ C.Connection, PersistEntity val) => Key val -> ReaderT C.Connection m (Bool)
deleteByKey key = do
  (relVarName, restrictionPredicate) <- commonKeyQueryProperties key
  let query = Delete relVarName restrictionPredicate
  conn <- ask
  maybeErr <- Trans.liftIO $ C.executeDatabaseContextExpr conn query
  return $ isJust maybeErr
    
--convert persistent update list to ProjectM36 Update map    
updatesToUpdateMap :: (Trans.MonadIO m, 
                       PersistEntityBackend val ~ C.Connection,
                       PersistEntity val) =>
                      [DP.Update val] -> ReaderT C.Connection m (M.Map AttributeName Atom)
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
      
updateByKey :: (Trans.MonadIO m, PersistEntityBackend val ~ C.Connection, PersistEntity val) => Key val -> [DP.Update val] -> ReaderT C.Connection m ()    
updateByKey key updates = do
  (relVarName, restrictionPredicate) <- commonKeyQueryProperties key
  updateMap <- updatesToUpdateMap updates
  let query = Update relVarName updateMap restrictionPredicate
  conn <- ask
  maybeErr <- Trans.liftIO $ C.executeDatabaseContextExpr conn query
  case maybeErr of
    Just err -> Trans.liftIO $ throwIO $ PersistError "executeDatabaseExpr error"
    Nothing -> return ()
    
--set the truth unconditionally    
repsertByKey :: (Trans.MonadIO m, C.Connection ~ PersistEntityBackend val, PersistEntity val) => Key val -> val -> ReaderT C.Connection m ()
repsertByKey key val = do
  _ <- delete key
  insertKey key val
    
replaceByKey :: (Trans.MonadIO m, C.Connection ~ PersistEntityBackend val, PersistEntity val) => Key val -> val -> ReaderT C.Connection m ()     
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

keyToUUID:: (PersistEntity record) => Key record -> U.UUID
keyToUUID key = case keyToValues key of
  ([PersistText uuidText]) -> case U.fromString (T.unpack uuidText) of
    Nothing -> error "error reading uuid from text in key construction"
    Just uuid -> uuid
  _ -> error "unexpected persist value in key construction"
  
instance PersistStore C.Connection where
  newtype BackendKey C.Connection = ProjectM36Key { unPM36Key :: U.UUID }
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
                  
instance FromJSON (BackendKey C.Connection) where                  
  parseJSON = withText "ProjectM36Key" $ \t -> maybe (fail "Invalid UUID") (return . ProjectM36Key) (U.fromString (T.unpack t))
  
instance ToJSON (BackendKey C.Connection) where
  toJSON (ProjectM36Key uuid) = toJSON $ U.toString uuid
    
--wrapped version which throws exceptions    
executeDatabaseContextExpr :: C.Connection -> DatabaseExpr -> IO ()
executeDatabaseContextExpr conn expr = do
  res <- C.executeDatabaseContextExpr conn expr
  case res of
    Nothing -> return ()
    Just err -> throwIO (PersistError $ T.pack (show err))
    
relVarNameFromRecord :: (PersistEntity record, PersistEntityBackend record ~ C.Connection)
               => record -> RelVarName
relVarNameFromRecord = unDBName . entityDB . entityDef . Just
        
instance PersistField U.UUID where
  toPersistValue val = PersistText $ T.pack (U.toString val)
  fromPersistValue (PersistText uuidText) = case U.fromString $ T.unpack uuidText of
    Nothing -> Left "uuid text read failure"
    Just uuid -> Right uuid
  fromPersistValue _ = Left $ T.pack "expected PersistObjectId"    
    
{- unfortunately copy-pasted from Database.Persist.Sql.Orphan.PersistStore -}
updateFieldDef :: PersistEntity v => DP.Update v -> FieldDef
updateFieldDef (DP.Update f _ _) = persistFieldDef f
updateFieldDef (BackendUpdate {}) = error "updateFieldDef did not expect BackendUpdate"
  
updatePersistValue :: DP.Update v -> PersistValue
updatePersistValue (DP.Update _ v _) = toPersistValue v
updatePersistValue (BackendUpdate {}) = error "updatePersistValue did not expect BackendUpdate"
