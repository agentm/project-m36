{-# LANGUAGE DeriveGeneric #-}
module ProjectM36.TransactionGraph.Types where
import ProjectM36.Base
import ProjectM36.Transaction.Types
import qualified Data.Set as S
import qualified Data.Map as M
import GHC.Generics
import qualified Data.List.NonEmpty as NE

transactions :: TransactionGraph -> S.Set Transaction
transactions (TransactionGraph _ t) = t

transactionHeads :: TransactionGraph -> TransactionHeads
transactionHeads (TransactionGraph heads _) = heads


type TransactionHeads = M.Map HeadName Transaction

-- | The transaction graph is the global database's state which references every committed transaction.

data TransactionGraph = TransactionGraph TransactionHeads (S.Set Transaction)
  deriving Generic

type TransactionParents = NE.NonEmpty TransactionId

transactionHeadsForGraph :: TransactionGraph -> TransactionHeads
transactionHeadsForGraph (TransactionGraph hs _) = hs

transactionsForGraph :: TransactionGraph -> S.Set Transaction
transactionsForGraph (TransactionGraph _ ts) = ts

transactionIdsForGraph :: TransactionGraph -> S.Set TransactionId
transactionIdsForGraph = S.map transactionId . transactionsForGraph

