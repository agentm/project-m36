{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Transaction where
import qualified Data.UUID as U
import ProjectM36.Base
import qualified Data.Set as S

--create a disconnected transaction with a parent  
newDisconnectedTransaction :: U.UUID -> DatabaseContext -> DisconnectedTransaction
newDisconnectedTransaction parentUUID context = DisconnectedTransaction parentUUID context

transactionParentIds :: Transaction -> S.Set U.UUID
transactionParentIds (Transaction _ (TransactionInfo pUUID _) _) = S.singleton pUUID
transactionParentIds (Transaction _ (MergeTransactionInfo pUUID1 pUUID2 _) _) = S.fromList [pUUID1, pUUID2]

transactionChildIds :: Transaction -> S.Set U.UUID
transactionChildIds (Transaction _ (TransactionInfo _ children) _) = children
transactionChildIds (Transaction _ (MergeTransactionInfo _ _ children) _) = children

-- | Create a new transaction which is identical to the original except that a new set of children UUIDs is added.
transactionSetChildren :: Transaction -> S.Set U.UUID -> Transaction
transactionSetChildren t@(Transaction _ (TransactionInfo pUUID _) _) childIds = Transaction (transactionUUID t) (TransactionInfo pUUID childIds) (transactionContext t)
transactionSetChildren t@(Transaction _ (MergeTransactionInfo pUUID1 pUUID2 _) _) childIds =  Transaction (transactionUUID t) (MergeTransactionInfo pUUID1 pUUID2 childIds) (transactionContext t)

-- | Return the same transaction but referencing only the specific child transactions. This is useful when traversing a graph and returning a subgraph. This doesn't filter parent transactions because it assumes a head-to-root traversal.
filterTransactionInfoTransactions :: S.Set U.UUID -> TransactionInfo -> TransactionInfo
filterTransactionInfoTransactions filterIds (TransactionInfo parentId childIds) = TransactionInfo (filterParent parentId filterIds) (S.intersection childIds filterIds)
filterTransactionInfoTransactions filterIds (MergeTransactionInfo parentIdA parentIdB childIds) = MergeTransactionInfo (filterParent parentIdA filterIds) (filterParent parentIdB filterIds) (S.intersection childIds filterIds)

filterParent :: U.UUID -> S.Set U.UUID -> U.UUID
filterParent parentId validIds = if S.member parentId validIds then parentId else U.nil

-- | Remove any child or parent transaction references not in the valud UUID set.
filterTransaction :: S.Set U.UUID -> Transaction -> Transaction
filterTransaction filterIds (Transaction selfId tInfo context) = Transaction selfId (filterTransactionInfoTransactions filterIds tInfo) context

