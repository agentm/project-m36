module SQL.Interpreter.TransactionGraphOperator where
import ProjectM36.Interpreter
import SQL.Interpreter.Base
import Control.Applicative

data TransactionGraphOperator = Commit | Rollback
  deriving (Show, Eq)

transactionGraphOperatorP :: Parser TransactionGraphOperator
transactionGraphOperatorP = commitP <|> rollbackP
  
commitP :: Parser TransactionGraphOperator
commitP = reserved "commit" *> pure Commit

rollbackP :: Parser TransactionGraphOperator
rollbackP = reserved "rollback" *> pure Rollback
