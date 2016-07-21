{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Transaction where
import ProjectM36.Base
import qualified Data.Set as S
import qualified Data.UUID as U

--create a disconnected transaction with a parent  
newDisconnectedTransaction :: TransactionId -> DatabaseContext -> DisconnectedTransaction
newDisconnectedTransaction parentId context = DisconnectedTransaction parentId context

transactionParentIds :: Transaction -> S.Set TransactionId
transactionParentIds (Transaction _ (TransactionInfo pId _) _) = S.singleton pId
transactionParentIds (Transaction _ (MergeTransactionInfo pId1 pId2 _) _) = S.fromList [pId1, pId2]

transactionChildIds :: Transaction -> S.Set TransactionId
transactionChildIds (Transaction _ (TransactionInfo _ children) _) = children
transactionChildIds (Transaction _ (MergeTransactionInfo _ _ children) _) = children

-- | Create a new transaction which is identical to the original except that a new set of child transaction ids is added.
transactionSetChildren :: Transaction -> S.Set TransactionId -> Transaction
transactionSetChildren t@(Transaction _ (TransactionInfo pId _) _) childIds = Transaction (transactionId t) (TransactionInfo pId childIds) (transactionContext t)
transactionSetChildren t@(Transaction _ (MergeTransactionInfo pId1 pId2 _) _) childIds =  Transaction (transactionId t) (MergeTransactionInfo pId1 pId2 childIds) (transactionContext t)

-- | Return the same transaction but referencing only the specific child transactions. This is useful when traversing a graph and returning a subgraph. This doesn't filter parent transactions because it assumes a head-to-root traversal.
filterTransactionInfoTransactions :: S.Set TransactionId -> TransactionInfo -> TransactionInfo
filterTransactionInfoTransactions filterIds (TransactionInfo parentId childIds) = TransactionInfo (filterParent parentId filterIds) (S.intersection childIds filterIds)
filterTransactionInfoTransactions filterIds (MergeTransactionInfo parentIdA parentIdB childIds) = MergeTransactionInfo (filterParent parentIdA filterIds) (filterParent parentIdB filterIds) (S.intersection childIds filterIds)

filterParent :: TransactionId -> S.Set TransactionId -> TransactionId
filterParent parentId validIds = if S.member parentId validIds then parentId else U.nil

-- | Remove any child or parent transaction references not in the valud UUID set.
filterTransaction :: S.Set TransactionId -> Transaction -> Transaction
filterTransaction filterIds (Transaction selfId tInfo context) = Transaction selfId (filterTransactionInfoTransactions filterIds tInfo) context

