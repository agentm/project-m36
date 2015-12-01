{-# LANGUAGE GADTs #-}
module TutorialD.Interpreter.TransactionGraphOperator where
import TutorialD.Interpreter.Base
import Text.Parsec
import Text.Parsec.String
import qualified Data.UUID as U
import qualified Data.Text as T
import ProjectM36.TransactionGraph
import ProjectM36.Client
import ProjectM36.Error
import ProjectM36.Base

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

showGraphP :: Parser ROTransactionGraphOperator
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

roTransactionGraphOpP :: Parser ROTransactionGraphOperator
roTransactionGraphOpP = showGraphP

uuidP :: Parser U.UUID
uuidP = do
  uuidStr <- many (alphaNum <|> char '-')
  case U.fromString uuidStr of
    Nothing -> fail "Invalid uuid string"
    Just uuid -> return uuid


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
