{-# LANGUAGE GADTs #-}
module TutorialD.Interpreter.TransactionGraphOperator where
import TutorialD.Interpreter.Base
import Text.Parsec
import Text.Parsec.String
import ProjectM36.Base
import qualified Data.UUID as U
import qualified Data.Text as T
import ProjectM36.Error
import ProjectM36.Transaction
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Show
import ProjectM36.Relation.Show.Term

--operators which manipulate a transaction graph
data TransactionGraphOperator where
  JumpToHead :: HeadName -> TransactionGraphOperator
  JumpToTransaction :: U.UUID -> TransactionGraphOperator
  Branch :: HeadName -> TransactionGraphOperator
  Commit :: TransactionGraphOperator
  Rollback :: TransactionGraphOperator
  ShowGraph :: TransactionGraphOperator

jumpToHeadP :: Parser TransactionGraphOperator
jumpToHeadP = do
  reservedOp ":jumphead"
  headid <- identifier
  return $ JumpToHead (T.pack headid)

jumpToTransactionP :: Parser TransactionGraphOperator
jumpToTransactionP = do
  reservedOp ":jump"
  uuid <- uuidP
  return $ JumpToTransaction uuid

branchTransactionP :: Parser TransactionGraphOperator
branchTransactionP = do
  reservedOp ":branch"
  branchName <- identifier
  return $ Branch (T.pack branchName)

commitTransactionP :: Parser TransactionGraphOperator
commitTransactionP = do
  reservedOp ":commit"
  return $ Commit

rollbackTransactionP :: Parser TransactionGraphOperator
rollbackTransactionP = do
  reservedOp ":rollback"
  return $ Rollback

showGraphP :: Parser TransactionGraphOperator
showGraphP = do
  reservedOp ":showgraph"
  return $ ShowGraph

transactionGraphOpP :: Parser TransactionGraphOperator
transactionGraphOpP = do
  jumpToHeadP
  <|> jumpToTransactionP
  <|> branchTransactionP
  <|> commitTransactionP
  <|> rollbackTransactionP
  <|> showGraphP

uuidP :: Parser U.UUID
uuidP = do
  uuidStr <- many (alphaNum <|> char '-')
  case U.fromString uuidStr of
    Nothing -> fail "Invalid uuid string"
    Just uuid -> return uuid

-- returns the new "current" transaction, updated graph, and tutorial d result
-- the current transaction is not part of the transaction graph until it is committed
evalGraphOp :: U.UUID -> DisconnectedTransaction -> TransactionGraph -> TransactionGraphOperator -> Either RelationalError (DisconnectedTransaction, TransactionGraph, TutorialDOperatorResult)

--affects only disconncted transaction
evalGraphOp _ _ graph (JumpToTransaction jumpUUID) = case transactionForUUID jumpUUID graph of
  Left err -> Left err
  Right parentTrans -> Right (newTrans, graph, QuietSuccessResult)
    where
      newTrans = DisconnectedTransaction jumpUUID (transactionContext parentTrans)

-- switch from one head to another
-- affects only disconnectedtransaction
evalGraphOp _ _ graph (JumpToHead headName) =
  case transactionForHead headName graph of
    Just newHeadTransaction -> let disconnectedTrans = newDisconnectedTransaction (transactionUUID newHeadTransaction) (transactionContext newHeadTransaction) in
      Right (disconnectedTrans, graph, QuietSuccessResult)
    Nothing -> Left $ NoSuchHeadNameError headName
-- add new head pointing to branchPoint
-- repoint the disconnected transaction to the new branch commit (with a potentially different disconnected context)
-- affects transactiongraph and the disconnectedtransaction is recreated based off the branch
    {-
evalGraphOp newUUID discon@(DisconnectedTransaction parentUUID disconContext) graph (Branch newBranchName) = case transactionForUUID parentUUID graph of
  Nothing -> (discon, graph, DisplayErrorResult "Failed to find parent transaction.")
  Just parentTrans -> case addBranch newBranchName parentTrans graph of
    Nothing -> (discon, graph, DisplayErrorResult "Failed to add branch.")
    Just newGraph -> (newDiscon, newGraph, DisplayResult "Branched.")
     where
       newDiscon = DisconnectedTransaction (transactionUUID parentTrans) disconContext
-}

-- create a new commit and add it to the heads
-- technically, the new head could be added to an existing commit, but by adding a new commit, the new head is unambiguously linked to a new commit (with a context indentical to its parent)
evalGraphOp newUUID (DisconnectedTransaction parentUUID disconContext) graph (Branch newBranchName) = case transactionForUUID parentUUID graph of
  Left err -> Left err
  Right parentTrans -> case addBranch newUUID newBranchName parentTrans graph of
    Left err -> Left err
    Right (_, newGraph) -> Right (newDiscon, newGraph, QuietSuccessResult)
     where
      newDiscon = DisconnectedTransaction newUUID disconContext

-- add the disconnected transaction to the graph
-- affects graph and disconnectedtransaction- the new disconnectedtransaction's parent is the freshly committed transaction
evalGraphOp newTransUUID discon@(DisconnectedTransaction parentUUID context) graph Commit = case transactionForUUID parentUUID graph of
  Left err -> Left err
  Right parentTransaction -> case headNameForTransaction parentTransaction graph of
    Nothing -> Left $ TransactionIsNotAHeadError parentUUID
    Just headName -> case maybeUpdatedGraph of
      Left err-> Left err
      Right (_, updatedGraph) -> Right (newDisconnectedTrans, updatedGraph, QuietSuccessResult)
      where
        newDisconnectedTrans = newDisconnectedTransaction newTransUUID context
        maybeUpdatedGraph = addDisconnectedTransaction newTransUUID headName discon graph

-- refresh the disconnected transaction, return the same graph
evalGraphOp _ (DisconnectedTransaction parentUUID _) graph Rollback = case transactionForUUID parentUUID graph of
  Left err -> Left err
  Right parentTransaction -> Right (newDiscon, graph, QuietSuccessResult)
    where
      newDiscon = newDisconnectedTransaction parentUUID (transactionContext parentTransaction)

--display transaction graph as relation
evalGraphOp _ discon graph ShowGraph = do
  graphStr <- graphAsRelation discon graph
  return (discon, graph, DisplayResult $ showRelation graphStr)

{-
-- for interpreter-specific operations
interpretOps :: U.UUID -> DisconnectedTransaction -> TransactionGraph -> String -> (DisconnectedTransaction, TransactionGraph, TutorialDOperatorResult)
interpretOps newUUID trans@(DisconnectedTransaction _ context) transGraph instring = case parse interpreterOps "" instring of
  Left _ -> (trans, transGraph, NoActionResult)
  Right ops -> case ops of
    Left contextOp -> (trans, transGraph, (evalContextOp context contextOp))
    Right graphOp -> case evalGraphOp newUUID trans transGraph graphOp of
      Left err -> (trans, transGraph, DisplayErrorResult $ T.pack (show err))
      Right (newDiscon, newGraph, result) -> (newDiscon, newGraph, result)
-}
