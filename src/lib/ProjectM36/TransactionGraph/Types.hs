module ProjectM36.TransactionGraph.Types where
import ProjectM36.Base

transactions :: TransactionGraph -> S.Set Transaction
transactions (TransactionGraph _ t) = t

transactionHeads :: TransactionGraph -> TransactionHeads
transactionHeads (TransactionGraph heads _) = heads

transactionInfoDiffs :: TransactionInfo -> TransactionDiffs
transactionInfoDiffs (TransactionInfo _ d _) = d
transactionInfoDiffs (MergeTransactionInfo _ _ d _ ) = d
