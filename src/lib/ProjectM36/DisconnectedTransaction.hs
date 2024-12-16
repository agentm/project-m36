module ProjectM36.DisconnectedTransaction where
import ProjectM36.Base
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.DatabaseContext as DBC
import ProjectM36.ChangeTrackingDatabaseContext as CTDBC
import Data.Map
import Optics.Core

-- | Every set of modifications made to the database are atomically committed to the transaction graph as a transaction.

-- | The disconnected transaction represents an in-progress workspace used by sessions before changes are committed. This is similar to git's "index". After a transaction is committed, it is "connected" in the transaction graph and can no longer be modified.

data DisconnectedTransaction = DisconnectedTransaction TransactionId ChangeTrackingSchemas

type ChangeTrackingSchemas = Schemas ChangeTrackingDatabaseContext

--the database context expression represents a difference between the disconnected transaction and its immutable parent transaction- is this diff expr used at all?

concreteDatabaseContext :: DisconnectedTransaction -> ChangeTrackingDatabaseContext
concreteDatabaseContext (DisconnectedTransaction _ (Schemas context _)) = context

schemas :: DisconnectedTransaction -> ChangeTrackingSchemas
schemas (DisconnectedTransaction _ s) = s

loadGraphRefRelVarsOnly :: IsDatabaseContext ctx => TransactionId -> Schemas ctx -> Schemas ctx
loadGraphRefRelVarsOnly commitId (Schemas concreteCtx subschemas) = 
  let f k _ = RelationVariable k (TransactionMarker commitId)
      ctx' = concreteCtx & relationVariables .~ mapWithKey f (concreteCtx ^. relationVariables)
  in Schemas ctx' subschemas



parentId :: DisconnectedTransaction -> TransactionId
parentId (DisconnectedTransaction pid _) = pid

isDirty :: DisconnectedTransaction -> Bool
isDirty disconTrans = CTDBC.isDirty (concreteDatabaseContext disconTrans)

-- | Create a fresh (unchanged marker-ed) disconnected transaction- used after a commit
freshTransaction :: TransactionId -> ChangeTrackingSchemas -> DisconnectedTransaction
freshTransaction tid (Schemas ctx subschemas') =
  DisconnectedTransaction tid (Schemas (makeClean ctx) subschemas')

freshTransaction' :: TransactionId -> Schemas DatabaseContext -> DisconnectedTransaction
freshTransaction' tid (Schemas parentContext parentSchemas) =
  DisconnectedTransaction tid schemas'
  where
    schemas' = Schemas (fromDatabaseContext parentContext) parentSchemas
