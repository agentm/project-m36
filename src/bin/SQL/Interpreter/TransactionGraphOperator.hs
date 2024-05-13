module SQL.Interpreter.TransactionGraphOperator where
import ProjectM36.Interpreter
import SQL.Interpreter.Base
import Control.Applicative
import Data.Functor (($>))

data TransactionGraphOperator = Commit | Rollback
  deriving (Show, Eq)

transactionGraphOperatorP :: Parser TransactionGraphOperator
transactionGraphOperatorP = commitP <|> rollbackP
  
commitP :: Parser TransactionGraphOperator
commitP = reserved "commit" $> Commit

rollbackP :: Parser TransactionGraphOperator
rollbackP = reserved "rollback" $> Rollback
