module SQL.Interpreter where
import ProjectM36.Base
import ProjectM36.Interpreter
import SQL.Interpreter.Base
import SQL.Interpreter.Select
import SQL.Interpreter.Convert
import qualified Data.Text as T
import qualified ProjectM36.Client as C
import Text.Megaparsec
import Text.Megaparsec.Error
import Data.Void
import Text.Megaparsec.Char 
import Data.Text (Text)

data ImportBasicExampleOperator = ImportBasicExampleOperator T.Text
  deriving (Show)

data SQLCommand = RODatabaseContextOp Select | -- SELECT
                  DatabaseContextExprOp DatabaseContextExpr | -- UPDATE, DELETE, INSERT
                  ImportBasicExampleOp ImportBasicExampleOperator  -- IMPORT EXAMPLE cjdate
                deriving (Show)
  
parseSQLUserInput :: T.Text -> Either ParserError SQLCommand
parseSQLUserInput = parse parseRODatabaseContextOp "" -- <|> parseDatabaseContextExprOp <|> parseImportBasicExampleOp)

parseRODatabaseContextOp :: Parser SQLCommand
parseRODatabaseContextOp = RODatabaseContextOp <$> queryExprP

parseDatabaseContextExprOp :: Parser SQLCommand
parseDatabaseContextExprOp = undefined

evalSQLInteractive :: C.SessionId -> C.Connection -> SafeEvaluationFlag -> InteractiveConsole -> SQLCommand -> IO ConsoleResult
evalSQLInteractive sesssionId conn safeFlag interactiveConsole command =
  case command of
    RODatabaseContextOp sel -> do
      --get relvars to build conversion context
      eRelExpr <- C.convertSQL sessionId conn sel
      case eRelExpr of
        Left err -> pure $ DisplayRelationalErrorResult err
        Right relExpr -> do
          eRel <- C.executeRelationalExpr sessionId conn relExpr
          case eRel of
            Left err -> pure $ DisplayRelationalErrorResult err
            Right rel -> pure $ DisplayRelationResult rel
          


-- relIn has attributes "attributes"::relation {attribute::Text,type::Text} and "name"::Text
{-
mkConversionTableContextFromRelation :: Relation -> TableContext
mkConversionTableContextFromRelation relIn =
  TableContext $ relFold folder mempty relIn
  where
    folder tup acc =
      case atomForAttributeName "name" tup of
        Left err -> pure acc
        Right rvname ->
          case atomForAttributeName "attributes" tup of
            Left err -> pure acc
            Right (RelationAtom attrsRel) ->
              let attrs = attributesFromList $ relFold attrsFolder [] attrsRel
      M.insert name (RelationVariable name (), attrs, mempty)
    attrsFolder tup acc =
      case atomForAttributeName "attribute" tup of
        Left err -> pure acc
        Right (TextAtom attrName) ->
          case atomForAttributeName "type" tup of
            Left err -> pure acc
            Right (TextAtom typeName) ->
              --convert typeName into AtomType
              case readMaybe typeName of
                Nothing -> pure acc
                Just atomType -> Attribute attrName atomType : acc

-}
