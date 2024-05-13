module SQL.Interpreter where
import ProjectM36.Base
import ProjectM36.Interpreter
import ProjectM36.SQL.Select
import ProjectM36.DatabaseContext
import ProjectM36.DateExamples
import ProjectM36.Error
import TutorialD.Printer
import SQL.Interpreter.ImportBasicExample
import SQL.Interpreter.TransactionGraphOperator
import SQL.Interpreter.Select
import SQL.Interpreter.DBUpdate
import ProjectM36.SQL.DBUpdate
import qualified Data.Text as T
import qualified ProjectM36.Client as C
import Text.Megaparsec
import SQL.Interpreter.Base

data SQLCommand = RODatabaseContextOp Query | -- SELECT
                  DatabaseContextExprOp DatabaseContextExpr |
                  DBUpdateOp [DBUpdate] | -- INSERT, UPDATE, DELETE, CREATE TABLE, DROP TABLE
                  ImportBasicExampleOp ImportBasicExampleOperator |  -- IMPORT EXAMPLE cjdate
                  TransactionGraphOp TransactionGraphOperator -- COMMIT, ROLLBACK
                deriving (Show)
  
parseSQLUserInput :: T.Text -> Either ParserError SQLCommand
parseSQLUserInput = parse ((parseRODatabaseContextOp <* semi) <|>
                           parseDatabaseContextExprOp <|>
                           (parseTransactionGraphOp <* semi) <|>
                           (parseImportBasicExampleOp <* semi)) ""

parseRODatabaseContextOp :: Parser SQLCommand
parseRODatabaseContextOp = RODatabaseContextOp <$> queryP

parseImportBasicExampleOp :: Parser SQLCommand
parseImportBasicExampleOp = ImportBasicExampleOp <$> importBasicExampleP

parseTransactionGraphOp :: Parser SQLCommand
parseTransactionGraphOp = TransactionGraphOp <$> transactionGraphOperatorP

parseDatabaseContextExprOp :: Parser SQLCommand
parseDatabaseContextExprOp = DBUpdateOp <$> dbUpdatesP

evalSQLInteractive :: C.SessionId -> C.Connection -> SafeEvaluationFlag -> InteractiveConsole -> SQLCommand -> IO ConsoleResult
evalSQLInteractive sessionId conn safeFlag interactiveConsole command =
  case command of
    RODatabaseContextOp query -> do
      --get relvars to build conversion context
      eDFExpr <- C.convertSQLQuery sessionId conn query
      case eDFExpr of
        Left err -> pure $ DisplayRelationalErrorResult err
        Right dfExpr -> do
          let hint = renderPretty dfExpr
          eDF <- C.executeDataFrameExpr sessionId conn dfExpr
          case eDF of
            Left err -> pure $ DisplayRelationalErrorResult err
            Right df -> pure $ DisplayHintWith ("[Equivalent TutorialD] " <> hint) (DisplayDataFrameResult df)
    ImportBasicExampleOp (ImportBasicExampleOperator exampleName) -> do
      if exampleName == "cjdate" then
        evalSQLInteractive sessionId conn safeFlag interactiveConsole (DatabaseContextExprOp (databaseContextAsDatabaseContextExpr dateExamples))
        else
          pure (DisplayErrorResult ("No such example: " <> exampleName))
    DatabaseContextExprOp dbcExpr -> do
      eHandler $ C.executeDatabaseContextExpr sessionId conn dbcExpr
    DBUpdateOp updates -> do
      eDBCExpr <- C.convertSQLDBUpdates sessionId conn updates
      case eDBCExpr of
        Left err -> pure $ DisplayRelationalErrorResult err
        Right dbcExpr -> do
          let hint = renderPretty dbcExpr
          ret <- C.executeDatabaseContextExpr sessionId conn dbcExpr
          case ret of
            Left err -> barf err
            Right () -> pure $ DisplayHintWith ("Equivalent TutorialD: " <> hint) QuietSuccessResult
    TransactionGraphOp Commit -> do
      eHandler $ C.commit sessionId conn
    TransactionGraphOp Rollback -> do
      eHandler $ C.rollback sessionId conn
  where
    eHandler io = do
      eErr <- io
      case eErr of
        Left err -> barf err
        Right () -> return QuietSuccessResult
    barf :: C.RelationalError -> IO ConsoleResult
    barf (C.ScriptError (OtherScriptCompilationError errStr)) = pure (DisplayErrorResult (T.pack errStr))
    barf (C.ParseError err) = pure (DisplayErrorResult err)
    barf err = return $ DisplayErrorResult (T.pack (show err))
      
