module SQL.Interpreter where
import ProjectM36.Base
import ProjectM36.Interpreter
import ProjectM36.SQL.Select
import ProjectM36.SQL.Update
import ProjectM36.DatabaseContext
import ProjectM36.DateExamples
import ProjectM36.Error
import SQL.Interpreter.ImportBasicExample
import SQL.Interpreter.TransactionGraphOperator
import SQL.Interpreter.Select
import SQL.Interpreter.Update
import qualified Data.Text as T
import qualified ProjectM36.Client as C
import Text.Megaparsec
import SQL.Interpreter.Base

data SQLCommand = RODatabaseContextOp Select | -- SELECT
                  DatabaseContextExprOp DatabaseContextExpr |
                  UpdateOp Update | -- UPDATE, DELETE, INSERT
--                  InsertOp Insert |
--                  DeleteOp Delete |
                  ImportBasicExampleOp ImportBasicExampleOperator |  -- IMPORT EXAMPLE cjdate
                  TransactionGraphOp TransactionGraphOperator -- COMMIT, ROLLBACK
                deriving (Show)
  
parseSQLUserInput :: T.Text -> Either ParserError SQLCommand
parseSQLUserInput = parse ((parseRODatabaseContextOp <|>
                           parseDatabaseContextExprOp <|>
                           parseTransactionGraphOp <|>
                           parseImportBasicExampleOp) <* semi) ""

parseRODatabaseContextOp :: Parser SQLCommand
parseRODatabaseContextOp = RODatabaseContextOp <$> queryExprP

parseImportBasicExampleOp :: Parser SQLCommand
parseImportBasicExampleOp = ImportBasicExampleOp <$> importBasicExampleP

parseTransactionGraphOp :: Parser SQLCommand
parseTransactionGraphOp = TransactionGraphOp <$> transactionGraphOperatorP

parseDatabaseContextExprOp :: Parser SQLCommand
parseDatabaseContextExprOp = UpdateOp <$> updateP  -- <|> insertP)

evalSQLInteractive :: C.SessionId -> C.Connection -> SafeEvaluationFlag -> InteractiveConsole -> SQLCommand -> IO ConsoleResult
evalSQLInteractive sessionId conn safeFlag interactiveConsole command =
  case command of
    RODatabaseContextOp sel -> do
      --get relvars to build conversion context
      eDFExpr <- C.convertSQLSelect sessionId conn sel
      case eDFExpr of
        Left err -> pure $ DisplayRelationalErrorResult err
        Right dfExpr -> do
          eDF <- C.executeDataFrameExpr sessionId conn dfExpr
          case eDF of
            Left err -> pure $ DisplayRelationalErrorResult err
            Right df -> pure $ DisplayDataFrameResult df
    ImportBasicExampleOp (ImportBasicExampleOperator exampleName) -> do
      if exampleName == "cjdate" then
        evalSQLInteractive sessionId conn safeFlag interactiveConsole (DatabaseContextExprOp (databaseContextAsDatabaseContextExpr dateExamples))
        else
          pure (DisplayErrorResult ("No such example: " <> exampleName))
    DatabaseContextExprOp dbcExpr -> do
        eHandler $ C.executeDatabaseContextExpr sessionId conn dbcExpr
    UpdateOp up -> do
      eDBCExpr <- C.convertSQLUpdate sessionId conn up
      case eDBCExpr of
        Left err -> pure $ DisplayRelationalErrorResult err
        Right dbcExpr -> 
          evalSQLInteractive sessionId conn safeFlag interactiveConsole (DatabaseContextExprOp dbcExpr)
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
      
