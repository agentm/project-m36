module ProjectM36.Transaction where
import ProjectM36.Base
import qualified Data.Set as S
import qualified Data.UUID as U
import qualified Data.Map as M

transactionParentIds :: Transaction -> S.Set TransactionId
transactionParentIds (Transaction _ (TransactionInfo pId _) _) = S.singleton pId
transactionParentIds (Transaction _ (MergeTransactionInfo pId1 pId2 _) _) = S.fromList [pId1, pId2]

transactionChildIds :: Transaction -> S.Set TransactionId
transactionChildIds (Transaction _ (TransactionInfo _ children) _) = children
transactionChildIds (Transaction _ (MergeTransactionInfo _ _ children) _) = children

-- | Create a new transaction which is identical to the original except that a new set of child transaction ids is added.
transactionSetChildren :: Transaction -> S.Set TransactionId -> Transaction
transactionSetChildren t@(Transaction _ (TransactionInfo pId _) schemas') childIds = Transaction (transactionId t) (TransactionInfo pId childIds) schemas'
transactionSetChildren t@(Transaction _ (MergeTransactionInfo pId1 pId2 _) schemas') childIds =  Transaction (transactionId t) (MergeTransactionInfo pId1 pId2 childIds) schemas'

-- | Return the same transaction but referencing only the specific child transactions. This is useful when traversing a graph and returning a subgraph. This doesn't filter parent transactions because it assumes a head-to-root traversal.
filterTransactionInfoTransactions :: S.Set TransactionId -> TransactionInfo -> TransactionInfo
filterTransactionInfoTransactions filterIds (TransactionInfo parentId childIds) = TransactionInfo (filterParent parentId filterIds) (S.intersection childIds filterIds)
filterTransactionInfoTransactions filterIds (MergeTransactionInfo parentIdA parentIdB childIds) = MergeTransactionInfo (filterParent parentIdA filterIds) (filterParent parentIdB filterIds) (S.intersection childIds filterIds)

filterParent :: TransactionId -> S.Set TransactionId -> TransactionId
filterParent parentId validIds = if S.member parentId validIds then parentId else U.nil

-- | Remove any child or parent transaction references not in the valud UUID set.
filterTransaction :: S.Set TransactionId -> Transaction -> Transaction
filterTransaction filterIds (Transaction selfId tInfo context) = Transaction selfId (filterTransactionInfoTransactions filterIds tInfo) context

-- | Return the singular context which is not virtual.
concreteDatabaseContext :: Transaction -> DatabaseContext
concreteDatabaseContext (Transaction _ _ (Schemas context _)) = context

-- | Returns all schemas including the concrete schema.
schemas :: Transaction -> Schemas
schemas (Transaction _ _ schemas') = schemas'
    
-- | Returns all subschemas which are isomorphic or sub-isomorphic to the concrete schema.
subschemas :: Transaction -> Subschemas
subschemas (Transaction _ _ (Schemas _ sschemas)) = sschemas
