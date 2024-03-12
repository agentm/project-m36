module SQL.Interpreter.CreateTable where
import SQL.Interpreter.Select
import ProjectM36.SQL.Select
import ProjectM36.SQL.CreateTable
import SQL.Interpreter.Base
import ProjectM36.Interpreter
import Text.Megaparsec

createTableP :: Parser CreateTable
createTableP = do
  reserveds "create table"
  tname <- tableNameP
  colsAndTypes <- parens columnNamesAndTypesP
  pure $ CreateTable { target = tname,
                       targetColumns = colsAndTypes
                       }

columnNamesAndTypesP :: Parser [(UnqualifiedColumnName, ColumnType, PerColumnConstraints)]
columnNamesAndTypesP =
  sepByComma $ do
    colName <- unqualifiedColumnNameP
    colType <- columnTypeP
    perColConstraints <- perColConstraintsP
    pure (colName, colType, perColConstraints) 

columnTypeP :: Parser ColumnType
columnTypeP = choice (map (\(nam, typ) -> reserved nam *> pure typ) types)
  where
    types = [("integer", IntegerColumnType),
             ("int", IntegerColumnType),
             ("text", TextColumnType),
             ("bool", BoolColumnType),
             ("double", DoubleColumnType),
             ("datetime", DateTimeColumnType)]

perColConstraintsP :: Parser PerColumnConstraints
perColConstraintsP = do
  let baseConstraints = PerColumnConstraints { notNullConstraint = False }
  (try (reserveds "not null" *> pure (baseConstraints { notNullConstraint = True}))) <|> pure baseConstraints
