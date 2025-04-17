module ProjectM36.DisconnectedTransaction where
import ProjectM36.Base
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.DatabaseContext as DBC
import ProjectM36.DatabaseContext.Types as DBC
import ProjectM36.ChangeTrackingDatabaseContext as CTDBC
import Data.Map
import Optics.Core
import Data.Functor.Identity

-- | Every set of modifications made to the database are atomically committed to the transaction graph as a transaction.

-- | The disconnected transaction represents an in-progress workspace used by sessions before changes are committed. This is similar to git's "index". After a transaction is committed, it is "connected" in the transaction graph and can no longer be modified.

data DisconnectedTransaction = DisconnectedTransaction TransactionId (Schemas DatabaseContext)

type TransactionRefSchemas = Schemas DatabaseContext

--the database context expression represents a difference between the disconnected transaction and its immutable parent transaction- is this diff expr used at all?

concreteDatabaseContext :: DisconnectedTransaction -> DatabaseContext
concreteDatabaseContext (DisconnectedTransaction _ (Schemas context _)) = context

schemas :: DisconnectedTransaction -> TransactionRefSchemas
schemas (DisconnectedTransaction _ s) = s

{-
-- | Mark all database context values as NotChangedSince- used for about-to-be-committed transactions
loadGraphRefRelVarsOnly :: TransactionId -> TransactionRefSchemas -> TransactionRefSchemas 
loadGraphRefRelVarsOnly commitId (Schemas concreteCtx subschemas) = 
  let f k _ = RelationVariable k (TransactionMarker commitId)
      ctx' = concreteCtx { relationVariables = ValueMarker $ mapWithKey f (concreteCtx & relationVariables) }
  in Schemas ctx' subschemas
-}


parentId :: DisconnectedTransaction -> TransactionId
parentId (DisconnectedTransaction pid _) = pid

isUpdated :: DisconnectedTransaction -> Bool
isUpdated disconTrans = DBC.isUpdated (concreteDatabaseContext disconTrans)

-- | Create a fresh (unchanged marker-ed) disconnected transaction- used after a commit so all database context values should refer to previous transactions.
freshTransaction :: TransactionId -> TransactionRefSchemas -> DisconnectedTransaction
freshTransaction tid (Schemas ctx subschemas') =
  DisconnectedTransaction tid (Schemas (freshDatabaseContext tid ctx) subschemas')

freshTransaction' :: TransactionId -> Schemas DatabaseContext -> DisconnectedTransaction
freshTransaction' tid (Schemas parentContext parentSchemas) =
  DisconnectedTransaction tid schemas'
  where
    schemas' = Schemas (freshDatabaseContext tid parentContext) parentSchemas

