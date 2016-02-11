{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Sessions where
import Control.Concurrent.STM
import qualified STMContainers.Map as STMMap
import ListT
import ProjectM36.Attribute
import ProjectM36.Base
import ProjectM36.DataTypes.Primitive
import ProjectM36.Session
import ProjectM36.Atom
import ProjectM36.Relation
import ProjectM36.Error
import qualified Data.UUID as U

type Sessions = STMMap.Map SessionId Session

stmMapToList :: STMMap.Map k v -> STM [(k, v)]
stmMapToList = ListT.fold (\l -> return . (:l)) [] . STMMap.stream

uuidAtom :: U.UUID -> Atom
uuidAtom = textAtom . U.toText

sessionsAsRelation :: Sessions -> STM (Either RelationalError Relation)
sessionsAsRelation sessions = do
  sessionAssocs <- stmMapToList sessions
  let atomMatrix = map (\(sessionId, (Session (DisconnectedTransaction parentUUID _))) -> [uuidAtom sessionId, uuidAtom parentUUID]) sessionAssocs
  pure $ mkRelationFromList (attributesFromList [Attribute "sessionid" textAtomType,
                             Attribute "parentCommit" textAtomType]) atomMatrix
    
  
