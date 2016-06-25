{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Transaction where
import qualified Data.UUID as U
import ProjectM36.Base
import qualified Data.Set as S

--create a disconnected transaction with a parent  
newDisconnectedTransaction :: U.UUID -> DatabaseContext -> DisconnectedTransaction
newDisconnectedTransaction parentUUID context = DisconnectedTransaction parentUUID context

transactionParentUUIDs :: Transaction -> S.Set U.UUID
transactionParentUUIDs (Transaction _ (TransactionInfo pUUID _) _) = S.singleton pUUID
transactionParentUUIDs (Transaction _ (MergeTransactionInfo pUUID1 pUUID2 _) _) = S.fromList [pUUID1, pUUID2]

transactionChildren :: Transaction -> S.Set U.UUID
transactionChildren (Transaction _ (TransactionInfo _ children) _) = children
transactionChildren (Transaction _ (MergeTransactionInfo _ _ children) _) = children

-- | Create a new transaction which is identical to the original except that a new set of children UUIDs is added.
transactionSetChildren :: Transaction -> S.Set U.UUID -> Transaction
transactionSetChildren t@(Transaction _ (TransactionInfo pUUID _) _) childIds = Transaction (transactionUUID t) (TransactionInfo pUUID childIds) (transactionContext t)
transactionSetChildren t@(Transaction _ (MergeTransactionInfo pUUID1 pUUID2 _) _) childIds =  Transaction (transactionUUID t) (MergeTransactionInfo pUUID1 pUUID2 childIds) (transactionContext t)

-- | Return the same transaction but referencing only the specific child transactions. This is useful when traversing a graph and returning a subgraph. This doesn't filter parent transactions because it assumes a head-to-root traversal.
filterChildrenFromTransactionInfo :: TransactionInfo -> S.Set U.UUID -> TransactionInfo
filterChildrenFromTransactionInfo (TransactionInfo parentId childIds) filterIds = TransactionInfo parentId (S.intersection childIds filterIds)
filterChildrenFromTransactionInfo (MergeTransactionInfo parentIdA parentIdB childIds) filterIds = MergeTransactionInfo parentIdA parentIdB (S.intersection childIds filterIds)

filterChildTransactions :: Transaction -> S.Set U.UUID -> Transaction
filterChildTransactions (Transaction selfId tInfo context) filterIds = Transaction selfId (filterChildrenFromTransactionInfo tInfo filterIds) context