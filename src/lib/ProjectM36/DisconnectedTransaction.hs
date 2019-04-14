module ProjectM36.DisconnectedTransaction where
import ProjectM36.Base

concreteDatabaseContext :: DisconnectedTransaction -> DatabaseContext
concreteDatabaseContext (DisconnectedTransaction _ (Schemas context _) _) = context

schemas :: DisconnectedTransaction -> Schemas
schemas (DisconnectedTransaction _ s _) = s

parentId :: DisconnectedTransaction -> TransactionId
parentId (DisconnectedTransaction pid _ _) = pid

isDirty :: DisconnectedTransaction -> Bool
isDirty (DisconnectedTransaction _ _ NoOperation) = False
isDirty _ = True

diffExpr :: DisconnectedTransaction -> DatabaseContextExpr
diffExpr (DisconnectedTransaction _ _ expr) = expr

freshTransaction :: TransactionId -> Schemas -> DisconnectedTransaction
freshTransaction tid schemas' = DisconnectedTransaction tid schemas' NoOperation
