module ProjectM36.Session where
import ProjectM36.Base
import ProjectM36.DatabaseContext.Types
import ProjectM36.TransactionGraph.Types
import qualified ProjectM36.Transaction.Types as T
import ProjectM36.RelationalExpression
import Data.UUID
import qualified Data.Map as M
import ProjectM36.Error
import ProjectM36.ValueMarker
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

subschemas :: Session -> ValueMarker Subschemas
subschemas (Session discon _) = Schema.subschemas (disconSchemas discon)

resolveSubschemas :: Session -> TransactionGraph -> Either RelationalError Subschemas
resolveSubschemas session graph = do
  let resolveSubschemas' val = 
        case val of
          ValueMarker v -> pure v
          NotChangedSinceMarker tid -> do
            t <- transactionForId tid graph
            resolveSubschemas' (T.subschemas t)
  resolveSubschemas' (subschemas session)

schemas :: Session -> Discon.TransactionRefSchemas
schemas (Session discon _) = disconSchemas discon

schemaName :: Session -> SchemaName
schemaName (Session _ s) = s

setSchemaName :: SchemaName -> Session -> TransactionGraph -> Either RelationalError Session
setSchemaName sname session graph = do
  sSchemas <- resolveSubschemas session graph
  if sname == defaultSchemaName || M.member sname sSchemas then
    pure (Session (disconnectedTransaction session) sname)
  else
    Left (SubschemaNameNotInUseError sname)


