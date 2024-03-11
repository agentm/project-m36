module SQL.Interpreter.DBUpdate where
import ProjectM36.Interpreter
import ProjectM36.SQL.DBUpdate
import SQL.Interpreter.Update
import SQL.Interpreter.Insert
import SQL.Interpreter.Delete
import SQL.Interpreter.Base
import Text.Megaparsec

dbUpdatesP :: Parser [DBUpdate]
dbUpdatesP = some dbUpdateP

dbUpdateP :: Parser DBUpdate
dbUpdateP = (UpdateUpdate <$> updateP <* semi) <|>
            (UpdateInsert <$> insertP <* semi) <|>
            (UpdateDelete <$> deleteP <* semi)
  
