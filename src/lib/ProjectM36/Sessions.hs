{-# LANGUAGE PackageImports #-}
module ProjectM36.Sessions where
import Control.Concurrent.STM
#if MIN_VERSION_stm_containers(1,0,0)
import qualified StmContainers.Map as StmMap
import qualified StmContainers.Set as StmSet
#else
import qualified STMContainers.Map as StmMap
import qualified STMContainers.Set as StmSet
#endif 
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
#if MIN_VERSION_stm_containers(1,0,0)
stmMapToList = UnfoldM.foldM (Foldl.generalize Foldl.list) . StmMap.unfoldM
#else
stmMapToList = ListT.fold (\l -> return . (:l)) [] . StmMap.stream
#endif

stmSetToList :: StmSet.Set v -> STM [v]
#if MIN_VERSION_stm_containers(1,0,0)
stmSetToList = UnfoldM.foldM (Foldl.generalize Foldl.list) . StmSet.unfoldM
#else
stmSetToList = ListT.fold (\l -> return . (:l)) [] . StmSet.stream
#endif

uuidAtom :: U.UUID -> Atom
uuidAtom = TextAtom . U.toText

sessionsAsRelation :: Sessions -> STM (Either RelationalError Relation)
sessionsAsRelation sessions = do
  sessionAssocs <- stmMapToList sessions
  let atomMatrix = map (\(sessionId, session) -> [uuidAtom sessionId, uuidAtom (parentId session)]) sessionAssocs
  pure $ mkRelationFromList (attributesFromList [Attribute "sessionid" TextAtomType,
                             Attribute "parentCommit" TextAtomType]) atomMatrix
    
  
