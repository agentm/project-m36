{-# LANGUAGE PackageImports #-}
module ProjectM36.Sessions where
import Control.Concurrent.STM
import qualified StmContainers.Map as StmMap
import qualified StmContainers.Set as StmSet
import "list-t" ListT
import ProjectM36.Attribute
import ProjectM36.Base
import ProjectM36.Session
import ProjectM36.Relation
import ProjectM36.Error
import qualified Data.UUID as U
import qualified Control.Foldl as Foldl
import qualified DeferredFolds.UnfoldM as UnfoldM

type Sessions = StmMap.Map SessionId Session

--from https://github.com/nikita-volkov/stm-containers/blob/master/test/Main/MapTests.hs
stmMapToList :: StmMap.Map k v -> STM [(k, v)]
stmMapToList = UnfoldM.foldM (Foldl.generalize Foldl.list) . StmMap.unfoldM

stmSetToList :: StmSet.Set v -> STM [v]
stmSetToList = UnfoldM.foldM (Foldl.generalize Foldl.list) . StmSet.unfoldM

uuidAtom :: U.UUID -> Atom
uuidAtom = TextAtom . U.toText

sessionsAsRelation :: Sessions -> STM (Either RelationalError Relation)
sessionsAsRelation sessions = do
  sessionAssocs <- stmMapToList sessions
  let atomMatrix = map (\(sessionId, session) -> [uuidAtom sessionId, uuidAtom (parentId session)]) sessionAssocs
  pure $ mkRelationFromList (attributesFromList [Attribute "sessionid" TextAtomType,
                             Attribute "parentCommit" TextAtomType]) atomMatrix
    
  
