module SQL.Interpreter.DBUpdate where
import ProjectM36.Interpreter
import ProjectM36.SQL.DBUpdate
import SQL.Interpreter.Update
import SQL.Interpreter.Insert
import SQL.Interpreter.Delete
import SQL.Interpreter.CreateTable
import SQL.Interpreter.DropTable
import Text.Megaparsec

dbUpdatesP :: Parser [DBUpdate]
dbUpdatesP = some dbUpdateP

dbUpdateP :: Parser DBUpdate
dbUpdateP = (UpdateUpdate <$> updateP) <|>
            (UpdateInsert <$> insertP) <|>
            (UpdateDelete <$> deleteP) <|>
            (UpdateCreateTable <$> createTableP) <|>
            (UpdateDropTable <$> dropTableP)
  
