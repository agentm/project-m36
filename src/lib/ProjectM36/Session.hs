module ProjectM36.Session where
import ProjectM36.Base
import Data.UUID
import qualified Data.Map as M
import ProjectM36.Error

type SessionId = UUID

--the persistence of a session is as long as the life of the database (not serialized to disk)
-- sessions are not associated with connections and have separate lifetimes
data Session = Session DisconnectedTransaction SchemaName

defaultSchemaName :: SchemaName
defaultSchemaName = "main"

disconnectedTransaction :: Session -> DisconnectedTransaction
disconnectedTransaction (Session discon _) = discon

concreteDatabaseContext :: Session -> DatabaseContext
concreteDatabaseContext (Session (DisconnectedTransaction _ (Schemas context _)) _) = context

parentId :: Session -> TransactionId
parentId (Session (DisconnectedTransaction parentUUID _) _) = parentUUID

subschemas :: Session -> Subschemas
subschemas (Session (DisconnectedTransaction _ (Schemas _ s)) _) = s

schemas :: Session -> Schemas
schemas (Session (DisconnectedTransaction _ s) _) = s

schemaName :: Session -> SchemaName
schemaName (Session _ s) = s

setSchemaName :: SchemaName -> Session -> Either RelationalError Session
setSchemaName sname session = if sname == defaultSchemaName || M.member sname (subschemas session) then
                                pure (Session (disconnectedTransaction session) sname)
                              else
                                Left (SubschemaNameNotInUseError sname)
  