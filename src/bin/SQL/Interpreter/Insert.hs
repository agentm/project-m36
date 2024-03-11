module SQL.Interpreter.Insert where
import SQL.Interpreter.Select
import ProjectM36.SQL.Insert
import SQL.Interpreter.Base
import ProjectM36.Interpreter

insertP :: Parser Insert
insertP = do
  reserveds "insert into"
  tname <- tableNameP
  colNames <- parens (sepByComma1 unqualifiedColumnNameP)
  q <- queryP
  pure (Insert { target = tname,
                 targetColumns = colNames,
                 source = q })
