module ProjectM36.DisconnectedTransaction where
import ProjectM36.Base
import Data.Map

concreteDatabaseContext :: DisconnectedTransaction -> DatabaseContext
concreteDatabaseContext (DisconnectedTransaction _ (Schemas context _) _) = context

schemas :: DisconnectedTransaction -> Schemas
schemas (DisconnectedTransaction _ s _) = s

loadGraphRefRelVarsOnly :: TransactionId -> Schemas -> Schemas
loadGraphRefRelVarsOnly commitId (Schemas concreteCtx subschemas) = 
  let f k _ = RelationVariable k (TransactionMarker commitId)
      ctx' = concreteCtx { relationVariables = mapWithKey f (relationVariables concreteCtx)}
  in Schemas ctx' subschemas



parentId :: DisconnectedTransaction -> TransactionId
parentId (DisconnectedTransaction pid _ _) = pid

isDirty :: DisconnectedTransaction -> Bool
isDirty (DisconnectedTransaction _ _ flag) = flag

freshTransaction :: TransactionId -> Schemas -> DisconnectedTransaction
freshTransaction tid schemas' = DisconnectedTransaction tid schemas' False
