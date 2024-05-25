module SQL.Interpreter.TransactionGraphOperator where
import ProjectM36.Interpreter
import SQL.Interpreter.Base
import Control.Applicative
import Data.Functor (($>))

data TransactionGraphOperator = Begin | Commit | Rollback
  deriving (Show, Eq)

transactionGraphOperatorP :: Parser TransactionGraphOperator
transactionGraphOperatorP = beginP <|> commitP <|> rollbackP

beginP :: Parser TransactionGraphOperator
beginP = reserved "begin" $> Begin
  
commitP :: Parser TransactionGraphOperator
commitP = reserved "commit" $> Commit

rollbackP :: Parser TransactionGraphOperator
rollbackP = reserved "rollback" $> Rollback
