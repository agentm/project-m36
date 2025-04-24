{-# LANGUAGE GADTs #-}
module TutorialD.Interpreter.TransactionGraphOperator where
import TutorialD.Interpreter.Base
import ProjectM36.TransactionGraph hiding (autoMergeToHead)
import ProjectM36.Client as C
import ProjectM36.Interpreter
import ProjectM36.Base
import ProjectM36.Relation (relationTrue)
import Data.Functor

data ConvenienceTransactionGraphOperator = AutoMergeToHead MergeStrategy HeadName
                                         deriving (Show)

convenienceTransactionGraphOpP :: Parser ConvenienceTransactionGraphOperator
convenienceTransactionGraphOpP = autoMergeToHeadP

autoMergeToHeadP :: Parser ConvenienceTransactionGraphOperator
autoMergeToHeadP = do
  reserved ":automergetohead"
  AutoMergeToHead <$> mergeTransactionStrategyP <*> identifierP

jumpToHeadP :: Parser TransactionGraphExpr
jumpToHeadP = do
  reservedOp ":jumphead"
  JumpToHead <$> identifierP

jumpToTransactionP :: Parser TransactionGraphExpr
jumpToTransactionP = do
  reservedOp ":jump"
  JumpToTransaction <$> uuidP
  
walkBackToTimeP :: Parser TransactionGraphExpr 
walkBackToTimeP = do
  reservedOp ":walkbacktotime"
  WalkBackToTime <$> utcTimeP

branchTransactionP :: Parser AlterTransactionGraphExpr
branchTransactionP = do
  reservedOp ":branch"
  Branch <$> identifierP

deleteBranchP :: Parser AlterTransactionGraphExpr
deleteBranchP = do
  reserved ":deletebranch"
  DeleteBranch <$> identifierP

commitTransactionP :: Parser AlterTransactionGraphExpr
commitTransactionP = do
  reservedOp ":commit"
  pure Commit 

rollbackTransactionP :: Parser AlterTransactionGraphExpr
rollbackTransactionP = do
  reservedOp ":rollback"
  return Rollback

showGraphP :: Parser ROTransactionGraphOperator
showGraphP = do
  reservedOp ":showgraph"
  return ShowGraph
  
mergeTransactionStrategyP :: Parser MergeStrategy
mergeTransactionStrategyP = (reserved "union" $> UnionMergeStrategy) <|>
                            (do
                                reserved "selectedbranch"
                                SelectedBranchMergeStrategy <$> identifierP) <|>
                            (do
                                reserved "unionpreferbranch"
                                UnionPreferMergeStrategy <$> identifierP)
  
mergeTransactionsP :: Parser AlterTransactionGraphExpr
mergeTransactionsP = do
  reservedOp ":mergetrans"
  MergeTransactions <$> mergeTransactionStrategyP <*> identifierP <*> identifierP

validateMerkleHashesP :: Parser ROTransactionGraphOperator
validateMerkleHashesP = reservedOp ":validatemerklehashes" $> ValidateMerkleHashes

transactionGraphOpP :: Parser TransactionGraphExpr
transactionGraphOpP = 
  jumpToHeadP
  <|> jumpToTransactionP
  <|> walkBackToTimeP

alterTransactionGraphOpP :: Parser AlterTransactionGraphExpr
alterTransactionGraphOpP =
  branchTransactionP
  <|> deleteBranchP
  <|> commitTransactionP
  <|> rollbackTransactionP
  <|> mergeTransactionsP

roTransactionGraphOpP :: Parser ROTransactionGraphOperator
roTransactionGraphOpP = showGraphP <|> validateMerkleHashesP



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

evalROGraphOp :: SessionId -> Connection -> ROTransactionGraphOperator -> IO (Either RelationalError Relation)
evalROGraphOp sessionId conn ShowGraph = transactionGraphAsRelation sessionId conn
evalROGraphOp sessionId conn ValidateMerkleHashes = do
  eVal <- C.validateMerkleHashes sessionId conn
  case eVal of
    Left err -> pure (Left err)
    Right _ -> pure (Right relationTrue)

evalConvenienceGraphOp :: SessionId -> Connection -> ConvenienceTransactionGraphOperator -> IO (Either RelationalError ())
evalConvenienceGraphOp sessionId conn (AutoMergeToHead strat head') = autoMergeToHead sessionId conn strat head'
