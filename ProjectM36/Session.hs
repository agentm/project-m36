module ProjectM36.Session where
import ProjectM36.Base
import Data.UUID

type SessionId = UUID

--the persistence of a session is as long as the life of the database (not serialized to disk)
-- sessions are not associated with connections and have separate lifetimes
data Session = Session DisconnectedTransaction

sessionContext :: Session -> DatabaseContext
sessionContext (Session (DisconnectedTransaction _ context)) = context

sessionParentId :: Session -> TransactionId
sessionParentId (Session (DisconnectedTransaction parentUUID _)) = parentUUID

