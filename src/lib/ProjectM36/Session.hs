module ProjectM36.Session where
import ProjectM36.Base
import ProjectM36.DatabaseContext.Types 
import Data.UUID
import qualified Data.Map as M
import ProjectM36.Error
import qualified ProjectM36.DisconnectedTransaction as Discon
import ProjectM36.DisconnectedTransaction (DisconnectedTransaction(..))
import qualified ProjectM36.IsomorphicSchema.Types as Schema
import ProjectM36.IsomorphicSchema.Types (SchemaName, Subschemas)

type SessionId = UUID

--the persistence of a session is as long as the life of the database (not serialized to disk)
-- sessions are not associated with connections and have separate lifetimes
-- | Represents a pointer into the database's transaction graph which the 'DatabaseContextExpr's can then modify subsequently be committed to extend the transaction graph. The session contains staged (uncommitted) database changes as well as the means to switch between isomorphic schemas.
data Session = Session DisconnectedTransaction SchemaName

defaultSchemaName :: SchemaName
defaultSchemaName = "main"

disconnectedTransaction :: Session -> DisconnectedTransaction
disconnectedTransaction (Session discon _) = discon

isUpdated :: Session -> DirtyFlag
isUpdated (Session discon _) = Discon.isUpdated discon

concreteDatabaseContext :: Session -> DatabaseContext
concreteDatabaseContext (Session discon _) = Discon.concreteDatabaseContext discon

parentId :: Session -> TransactionId
parentId (Session discon _) = Discon.parentId discon

subschemas :: Session -> Subschemas
subschemas (Session discon _) = Schema.subschemas (disconSchemas discon)

schemas :: Session -> Discon.TransactionRefSchemas
schemas (Session discon _) = disconSchemas discon

schemaName :: Session -> SchemaName
schemaName (Session _ s) = s

setSchemaName :: SchemaName -> Session -> Either RelationalError Session
setSchemaName sname session = if sname == defaultSchemaName || M.member sname (subschemas session) then
                                pure (Session (disconnectedTransaction session) sname)
                              else
                                Left (SubschemaNameNotInUseError sname)

