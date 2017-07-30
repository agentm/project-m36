module ProjectM36.Session where
import ProjectM36.Base
import Data.UUID
import qualified Data.Map as M
import ProjectM36.Error

type SessionId = UUID

--the persistence of a session is as long as the life of the database (not serialized to disk)
-- sessions are not associated with connections and have separate lifetimes
-- | Represents a pointer into the database's transaction graph which the 'DatabaseContextExpr's can then modify subsequently be committed to extend the transaction graph. The session contains staged (uncommitted) database changes as well as the means to switch between isomorphic schemas.
data Session = Session DisconnectedTransaction SchemaName

defaultSchemaName :: SchemaName
defaultSchemaName = "main"

disconnectedTransaction :: Session -> DisconnectedTransaction
disconnectedTransaction (Session discon _) = discon

isDirty :: Session -> DirtyFlag
isDirty (Session (DisconnectedTransaction _ _ dirtyFlag) _) = dirtyFlag

concreteDatabaseContext :: Session -> DatabaseContext
concreteDatabaseContext (Session (DisconnectedTransaction _ (Schemas context _) _) _) = context

parentId :: Session -> TransactionId
parentId (Session (DisconnectedTransaction parentUUID _ _) _) = parentUUID

subschemas :: Session -> Subschemas
subschemas (Session (DisconnectedTransaction _ (Schemas _ s) _) _) = s

schemas :: Session -> Schemas
schemas (Session (DisconnectedTransaction _ s _) _) = s

schemaName :: Session -> SchemaName
schemaName (Session _ s) = s

setSchemaName :: SchemaName -> Session -> Either RelationalError Session
setSchemaName sname session = if sname == defaultSchemaName || M.member sname (subschemas session) then
                                pure (Session (disconnectedTransaction session) sname)
                              else
                                Left (SubschemaNameNotInUseError sname)
  