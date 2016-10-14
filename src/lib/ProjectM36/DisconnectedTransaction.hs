module ProjectM36.DisconnectedTransaction where
import ProjectM36.Base

concreteDatabaseContext :: DisconnectedTransaction -> DatabaseContext
concreteDatabaseContext (DisconnectedTransaction _ (Schemas context _)) = context

schemas :: DisconnectedTransaction -> Schemas
schemas (DisconnectedTransaction _ s) = s

parentId :: DisconnectedTransaction -> TransactionId
parentId (DisconnectedTransaction pid _) = pid