module ProjectM36.Session where
import ProjectM36.Base
import Data.UUID

type SessionId = UUID

--the persistence of a session is as long as the life of the database (not serialized to disk)
-- sessions are not associated with connections and have separate lifetimes
data Session = Session DisconnectedTransaction

concreteDatabaseContext :: Session -> DatabaseContext
concreteDatabaseContext (Session (DisconnectedTransaction _ (Schemas context _))) = context

parentId :: Session -> TransactionId
parentId (Session (DisconnectedTransaction parentUUID _)) = parentUUID

subschemas :: Session -> Subschemas
subschemas (Session (DisconnectedTransaction _ (Schemas _ s))) = s

schemas :: Session -> Schemas
schemas (Session (DisconnectedTransaction _ s)) = s