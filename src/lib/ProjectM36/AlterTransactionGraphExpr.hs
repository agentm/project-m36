module ProjectM36.AlterTransactionGraphExpr where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.DisconnectedTransaction as Discon
import ProjectM36.RelationalExpression.Types
import ProjectM36.TransactionGraph.Types
import ProjectM36.TransactionGraph.FindTransaction
import ProjectM36.Transaction.Types
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Merge

import Data.Time.Clock
import qualified Data.Map as M


evalAlterTransactionGraphExpr :: UTCTime -> TransactionId -> DisconnectedTransaction -> CheckConstraints -> TransactionGraph -> AlterTransactionGraphExpr -> Either RelationalError (DisconnectedTransaction, Maybe UncommittedTransaction, TransactionGraph)
evalAlterTransactionGraphExpr _stamp' _newId discon _checkConstraints graph@(TransactionGraph heads transSet) (Branch newBranchName) =
  if M.member newBranchName heads then
    Left (HeadNameAlreadyInUseError newBranchName)
    else do
    trans <- transactionForId (Discon.parentId discon) graph
    let discon' = discon { disconTransactionId = transactionId trans,
                           disconCurrentHead = CurrentHeadBranch newBranchName
                         }
    pure (discon', Nothing, TransactionGraph (M.insert newBranchName trans heads) transSet)
evalAlterTransactionGraphExpr _stamp' _newId discon _checkConstraints graph@(TransactionGraph graphHeads transSet) (DeleteBranch branchName) =
  case transactionForHead branchName graph of
    Nothing -> Left (NoSuchHeadNameError branchName)
    Just _ -> Right (discon, Nothing, TransactionGraph (M.delete branchName graphHeads) transSet)
evalAlterTransactionGraphExpr stamp' newId discon checkConstraints graph (MergeTransactions mergeStrategy headNameA headNameB) = do
  let env = freshGraphRefRelationalExprEnv Nothing graph
  (uTrans@(UncommittedTransaction trans), graph') <- runGraphRefRelationalExprM env $ mergeTransactions stamp' newId (Discon.parentId discon) checkConstraints mergeStrategy (MergeHeadNames { sourceHead = headNameA,
        targetHead = headNameB })
  let discon' = discon { disconSchemas = schemas trans,
                         disconCurrentHead = CurrentHeadBranch headNameB
                       }
  pure (discon', Just uTrans, graph')

evalAlterTransactionGraphExpr stamp' newTransId discon _checkConstraints graph Commit =
  case transactionForId (Discon.parentId discon) graph of
  Left err -> Left err
  Right parentTransaction ->
    case headNamesForTransaction parentTransaction graph of
      [] -> Left $ TransactionIsNotAHeadError (Discon.parentId discon)
      _ -> do
        let maybeUpdatedGraph = addDisconnectedTransaction stamp' newTransId discon graph
        case maybeUpdatedGraph of
          Left err -> Left err
          Right (uncommittedTrans, updatedGraph) -> do
                let newDisconnectedTrans = Discon.freshTransaction (disconCurrentHead discon) newTransId (disconSchemas discon)
                Right (newDisconnectedTrans, Just uncommittedTrans, updatedGraph)

evalAlterTransactionGraphExpr _stamp' _newId discon _checkConstraints graph Rollback =
  case transactionForId (disconTransactionId discon) graph of
    Left err -> Left err
    Right parentTransaction -> Right (newDiscon, Nothing, graph)
      where
        newDiscon = Discon.freshTransaction (disconCurrentHead discon) (disconTransactionId discon) (schemas parentTransaction)
