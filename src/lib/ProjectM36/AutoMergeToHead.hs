module ProjectM36.AutoMergeToHead where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.TransactionGraph.Types
import ProjectM36.TransactionGraph
import ProjectM36.DisconnectedTransaction
import ProjectM36.AlterTransactionGraphExpr
import ProjectM36.RelationalExpression

import Data.Time.Clock (UTCTime)
import qualified Data.UUID as U
import qualified Data.Set as S
import Data.Maybe (catMaybes)

-- | Create a temporary branch for commit, merge the result to head, delete the temporary branch. This is useful to atomically commit a transaction, avoiding a TransactionIsNotHeadError but trading it for a potential MergeError.
--this is not a GraphOp because it combines multiple graph operations
autoMergeToHead :: UTCTime -> (TransactionId, TransactionId, TransactionId) -> DisconnectedTransaction -> HeadName -> MergeStrategy -> TransactionGraph -> Either RelationalError (DisconnectedTransaction, TransactionGraphIncrementalWriteInfo)
autoMergeToHead stamp' (tempBranchTransId, tempCommitTransId, mergeTransId) discon mergeToHeadName strat graph = do
  let tempBranchName = "mergebranch_" <> U.toText tempBranchTransId
  --create the temp branch, branching does not create a new transaction
  (discon', mtrans', graph') <- evalAlterTransactionGraphExpr stamp' tempBranchTransId discon checkConstraints graph (Branch tempBranchName)

  --commit to the new branch- possible future optimization: don't require fsync for this- create a temp commit type
  (discon'', mtrans'', graph'') <- evalAlterTransactionGraphExpr stamp' tempCommitTransId discon' checkConstraints graph' Commit

  --jump to merge head
  discon''' <- evalTransactionGraphExpr discon'' graph'' (JumpToHead mergeToHeadName)

  --create the merge
  (discon'''', mtrans'''', graph'''') <- evalAlterTransactionGraphExpr stamp' mergeTransId discon''' checkConstraints graph'' (MergeTransactions strat tempBranchName mergeToHeadName)


  --delete the temp branch
  (discon''''', mtrans''''', graph''''') <- evalAlterTransactionGraphExpr stamp' tempBranchTransId discon'''' checkConstraints graph'''' (DeleteBranch tempBranchName)
  {-
  let rel = runReader (evalRelationalExpr (RelationVariable "s" ())) (mkRelationalExprState $ D.concreteDatabaseContext discon'''')
  traceShowM rel
-}
  pure (discon''''',
        TransactionGraphIncrementalWriteInfo {
           uncommittedTransactions = S.fromList (catMaybes [mtrans',
                                                            mtrans'',
                                                            mtrans'''',
                                                            mtrans'''''
                                                           ]),
           newGraph = graph'''''
           })
