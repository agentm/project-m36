module SQL.Interpreter.Delete where
import SQL.Interpreter.Select
import ProjectM36.SQL.Delete
import ProjectM36.SQL.Select
import SQL.Interpreter.Base
import ProjectM36.Interpreter
import Control.Applicative

deleteP :: Parser Delete
deleteP = do
  reserveds "delete from"
  tname <- tableNameP
  restrictExpr <- whereP <|> pure (RestrictionExpr (BooleanLiteral True))
  pure $ Delete { target = tname,
                  restriction = restrictExpr }
