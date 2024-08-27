module SQL.Interpreter.Info where
import ProjectM36.Interpreter
import SQL.Interpreter.Base
import Data.Functor
  
data InfoOperator = HelpOperator deriving Show

infoP :: Parser InfoOperator
infoP = reserved "help" $> HelpOperator
