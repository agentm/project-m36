module SQL.Interpreter.DropTable where
import ProjectM36.SQL.DropTable
import SQL.Interpreter.Select
import SQL.Interpreter.Base
import ProjectM36.Interpreter

dropTableP :: Parser DropTable
dropTableP = do
  reserveds "drop table"
  DropTable <$> tableNameP
