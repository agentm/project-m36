{-# LANGUAGE DeriveGeneric #-}
module ProjectM36.DisconnectedTransaction where
import ProjectM36.Base
import qualified ProjectM36.IsomorphicSchema.Types as Schema
import ProjectM36.IsomorphicSchema.Types (Schemas)
import ProjectM36.DatabaseContext as DBC
import ProjectM36.DatabaseContext.Types as DBC
import GHC.Generics

-- | Every set of modifications made to the database are atomically committed to the transaction graph as a transaction.

-- | The disconnected transaction represents an in-progress workspace used by sessions before changes are committed. This is similar to git's "index". After a transaction is committed, it is "connected" in the transaction graph and can no longer be modified.

data DisconnectedTransaction = DisconnectedTransaction {
  disconTransactionId :: TransactionId,
  disconSchemas :: Schemas DatabaseContext,
  disconCurrentHead :: CurrentHead
  }

data CurrentHead = CurrentHeadBranch HeadName | -- ^ track a branch
                   CurrentHeadTransactionId TransactionId -- ^ peg to a transaction which is not a head
                   deriving (Show, Eq, Generic)

type TransactionRefSchemas = Schemas DatabaseContext

--the database context expression represents a difference between the disconnected transaction and its immutable parent transaction- is this diff expr used at all?

concreteDatabaseContext :: DisconnectedTransaction -> DatabaseContext
concreteDatabaseContext discon = Schema.concreteDatabaseContext (disconSchemas discon)

parentId :: DisconnectedTransaction -> TransactionId
parentId = disconTransactionId

isUpdated :: DisconnectedTransaction -> Bool
isUpdated disconTrans = DBC.isUpdated (concreteDatabaseContext disconTrans)

-- | Create a fresh (unchanged marker-ed) disconnected transaction- used after a commit so all database context values should refer to previous transactions.
freshTransaction :: CurrentHead -> TransactionId -> TransactionRefSchemas -> DisconnectedTransaction
freshTransaction head' tid schemas =
  DisconnectedTransaction {
  disconTransactionId = tid,
  disconSchemas = schemas { Schema.concreteDatabaseContext = freshDatabaseContext tid (Schema.concreteDatabaseContext schemas) },
  disconCurrentHead = head'
  }


freshTransaction' :: CurrentHead -> TransactionId -> Schemas DatabaseContext -> DisconnectedTransaction
freshTransaction' head' tid schemas =
  DisconnectedTransaction {
  disconTransactionId = tid,
  disconSchemas = schemas',
  disconCurrentHead = head'
  }
  where
    schemas' = schemas { Schema.concreteDatabaseContext = freshDatabaseContext tid (Schema.concreteDatabaseContext schemas)
                       }

