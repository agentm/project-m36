module SQL.Interpreter.CreateTable where
import SQL.Interpreter.Select
import ProjectM36.SQL.Select
import ProjectM36.SQL.CreateTable
import SQL.Interpreter.Base
import ProjectM36.Interpreter
import Text.Megaparsec
import Control.Monad.Permutations

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

data PerColumnConstraintsParse =
  PerColumnConstraintsParse { parse_notNullConstraint :: Bool,
                              parse_uniquenessConstraint :: Bool,
                              parse_primaryKeyConstraint :: Bool,
                              parse_references :: Maybe (TableName, UnqualifiedColumnName)
                              }

referencesP :: Parser (TableName, UnqualifiedColumnName)
referencesP = do
  reserved "references"
  (,) <$> tableNameP <*> parens unqualifiedColumnNameP 
  
perColConstraintsP :: Parser PerColumnConstraints
perColConstraintsP = do
  parsed <- runPermutation $
    PerColumnConstraintsParse <$>
      toPermutationWithDefault False (try (reserveds "not null" *> pure True)) <*>
      toPermutationWithDefault False (reserved "unique" *> pure True) <*>
      toPermutationWithDefault False (reserved "primary key" *> pure True) <*>
      toPermutationWithDefault Nothing (Just <$> referencesP)
  pure (PerColumnConstraints { notNullConstraint = (parse_notNullConstraint parsed) || (parse_primaryKeyConstraint parsed),
                               uniquenessConstraint = (parse_uniquenessConstraint parsed) || (parse_primaryKeyConstraint parsed),
                               references = parse_references parsed })

