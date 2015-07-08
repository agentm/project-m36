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

