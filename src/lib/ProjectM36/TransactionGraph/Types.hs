{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia #-}
module ProjectM36.TransactionGraph.Types where
import ProjectM36.Base
import ProjectM36.Serialise.Base ()
import ProjectM36.Transaction.Types

import qualified Data.Set as S
import qualified Data.Map as M
import GHC.Generics
import qualified Data.List.NonEmpty as NE
import Codec.Winery
import Data.Time.Clock

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

-- | Used to track changes to child transactions relative to parent transactions as a write optimization. We can refer to the parent transaction for the data which has already been written, but we need to track then what actually changed.
data TransactionGraphIncrementalWriteInfo =
  TransactionGraphIncrementalWriteInfo {
  uncommittedTransactions :: S.Set UncommittedTransaction, -- the disconnected transaction holds the parent id, but the fst transaction id is the transaction id of the transaction in the "newGraph" which has not yet been flushed to permanent storage
  newGraph :: TransactionGraph
      }

affectedTransactionIds :: TransactionGraphIncrementalWriteInfo -> S.Set TransactionId
affectedTransactionIds winfo = S.map (\(UncommittedTransaction t) -> transactionId t) (uncommittedTransactions winfo)

data MergeHeadNames = MergeHeadNames { sourceHead :: HeadName,
                                       targetHead :: HeadName }
                      deriving (Eq, Show)

-- | Record a lookup for a specific transaction in the graph.
data TransactionIdLookup = TransactionIdLookup TransactionId |
                           TransactionIdHeadNameLookup HeadName [TransactionIdHeadBacktrack]
                           deriving (Show, Eq, Generic)
                           deriving Serialise via WineryVariant TransactionIdLookup

-- | Used for git-style head backtracking such as topic~3^2.
data TransactionIdHeadBacktrack = TransactionIdHeadParentBacktrack Int | -- ^ git equivalent of ~v: walk back n parents, arbitrarily choosing a parent when a choice must be made
                                  TransactionIdHeadBranchBacktrack Int | -- ^ git equivalent of ^: walk back one parent level to the nth arbitrarily-chosen parent
                                  TransactionStampHeadBacktrack UTCTime -- ^ git equivalent of 'git-rev-list -n 1 --before X' find the first transaction which was created before the timestamp
                                  deriving (Show, Eq, Generic)
                                  deriving Serialise via WineryVariant TransactionIdHeadBacktrack


data TransactionGraphExpr = JumpToHead HeadName  |
                            JumpToTransaction TransactionId |
                            WalkBackToTime UTCTime
                          deriving (Eq, Show, Generic)
  deriving Serialise via WineryVariant TransactionGraphExpr
                                
-- | Operators which manipulate a transaction graph and which transaction the current 'Session' is based upon.
data AlterTransactionGraphExpr = 
                                Branch HeadName |
                                DeleteBranch HeadName |
                                MergeTransactions MergeStrategy HeadName HeadName |
                                Commit |
                                Rollback
                              deriving (Eq, Show, Generic)
                              deriving Serialise via WineryVariant AlterTransactionGraphExpr
