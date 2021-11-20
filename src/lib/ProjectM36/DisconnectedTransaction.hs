module ProjectM36.DisconnectedTransaction where
import ProjectM36.Base
import Data.Map

concreteDatabaseContext :: DisconnectedTransaction -> DatabaseContext
concreteDatabaseContext (DisconnectedTransaction _ (Schemas context _) _) = context

schemas :: DisconnectedTransaction -> Schemas
schemas (DisconnectedTransaction _ s _) = s

loadGraphRefRelVarsOnly :: TransactionId -> Schemas -> Schemas
loadGraphRefRelVarsOnly commitId (Schemas ctx@(DatabaseContext _ rv _ _ _ _) subschemas) = 
  let f k _ = RelationVariable k (TransactionMarker commitId)
      ctx' = ctx { relationVariables = mapWithKey f rv}
  in Schemas ctx' subschemas



parentId :: DisconnectedTransaction -> TransactionId
parentId (DisconnectedTransaction pid _ _) = pid

isDirty :: DisconnectedTransaction -> Bool
isDirty (DisconnectedTransaction _ _ flag) = flag

freshTransaction :: TransactionId -> Schemas -> DisconnectedTransaction
freshTransaction tid schemas' = DisconnectedTransaction tid schemas' False
