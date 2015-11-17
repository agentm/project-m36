{-# LANGUAGE GADTs,OverloadedStrings #-}
module TutorialD.Interpreter where
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RODatabaseContextOperator
import TutorialD.Interpreter.DatabaseExpr
import TutorialD.Interpreter.TransactionGraphOperator

import TutorialD.Interpreter.Import.CSV
import TutorialD.Interpreter.Import.TutorialD
import TutorialD.Interpreter.Import.Base

import TutorialD.Interpreter.Export.CSV
import TutorialD.Interpreter.Export.Base

import ProjectM36.Base
import ProjectM36.Relation.Show.Term
import ProjectM36.TransactionGraph
import qualified ProjectM36.Client as C
import ProjectM36.Relation (attributes)

import Text.Parsec
import Text.Parsec.String
import Control.Monad.State
import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

{-
context ops are read-only operations which only operate on the database context (relvars and constraints)
database ops are read-write operations which change the database context (such as relvar assignment)
graph ops are read-write operations which change the transaction graph
-}
data ParsedOperation = RODatabaseContextOp RODatabaseContextOperator |
                       DatabaseExprOp DatabaseExpr |
                       GraphOp TransactionGraphOperator |
                       ROGraphOp ROTransactionGraphOperator |
                       ImportRelVarOp RelVarDataImportOperator |
                       ImportDBContextOp DatabaseContextDataImportOperator |
                       RelVarExportOp RelVarDataExportOperator

interpreterParserP :: Parser ParsedOperation
interpreterParserP = liftM RODatabaseContextOp (roDatabaseContextOperatorP <* eof) <|>
                     liftM GraphOp (transactionGraphOpP <* eof) <|>
                     liftM ROGraphOp (roTransactionGraphOpP <* eof) <|>
                     liftM DatabaseExprOp (databaseExprOpP <* eof) <|>
                     liftM ImportRelVarOp (importCSVP <* eof) <|>
                     liftM ImportDBContextOp (tutdImportP <* eof) <|>
                     liftM RelVarExportOp (exportCSVP <* eof)

promptText :: Maybe HeadName -> StringType
promptText mHeadName = "TutorialD (" `T.append` transInfo `T.append` "): "
  where
    transInfo = fromMaybe "<unknown>" mHeadName
          
parseTutorialD :: String -> Either ParseError ParsedOperation
parseTutorialD inputString = parse interpreterParserP "" inputString

--execute the operation and display result
evalTutorialD :: C.Connection -> ParsedOperation -> IO (TutorialDOperatorResult)
evalTutorialD conn expr = case expr of
  
  --this does not pass through the ProjectM36.Client library because the operations
  --are specific to the interpreter, though some operations may be of general use in the future
  (RODatabaseContextOp execOp) -> do
    evalRODatabaseContextOp conn execOp
    
  (DatabaseExprOp execOp) -> do 
    maybeErr <- C.executeDatabaseContextExpr conn execOp 
    case maybeErr of
      Just err -> barf err
      Nothing -> return QuietSuccessResult
    
  (GraphOp execOp) -> do
    maybeErr <- C.executeGraphExpr conn execOp
    case maybeErr of
      Just err -> barf err
      Nothing -> return QuietSuccessResult

  (ROGraphOp execOp) -> do
    opResult <- evalROGraphOp conn execOp
    case opResult of 
      Left err -> barf err
      Right rel -> return $ DisplayResult $ showRelation rel
      
  (ImportRelVarOp execOp@(RelVarDataImportOperator relVarName _ _)) -> do
    -- collect attributes from relvar name
    -- is there a race condition here? The attributes of the relvar may have since changed, no?
    eImportType <- C.typeForRelationalExpr conn (RelationVariable relVarName)
    case eImportType of
      Left err -> barf err
      Right importType -> do
        exprErr <- evalRelVarDataImportOperator execOp (attributes importType)
        case exprErr of
          Left err -> barf err
          Right dbexpr -> evalTutorialD conn (DatabaseExprOp dbexpr)
  
  (ImportDBContextOp execOp) -> do
    mDbexprs <- evalDatabaseContextDataImportOperator execOp
    case mDbexprs of 
      Left err -> barf err
      Right dbexprs -> evalTutorialD conn (DatabaseExprOp dbexprs)
      
  (RelVarExportOp execOp@(RelVarDataExportOperator relExpr _ _)) -> do
    --eval relexpr to relation and pass to export function
    eRel <- C.executeRelationalExpr conn relExpr
    case eRel of
      Left err -> barf err
      Right rel -> do
        exportResult <- evalRelVarDataExportOperator execOp rel
        case exportResult of
          Just err -> barf err
          Nothing -> pure QuietSuccessResult
  where
    barf err = return $ DisplayErrorResult (T.pack (show err))
  
data InterpreterConfig = LocalInterpreterConfig PersistenceStrategy |
                         RemoteInterpreterConfig C.NodeId C.DatabaseName

reprLoop :: InterpreterConfig -> C.Connection -> IO ()
reprLoop config conn = do
  homeDirectory <- getHomeDirectory
  let settings = defaultSettings {historyFile = Just (homeDirectory ++ "/.tutd_history")}
  mHeadName <- C.headName conn
  maybeLine <- runInputT settings $ getInputLine (T.unpack (promptText mHeadName))
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      case parseTutorialD line of
        Left err -> do
          displayOpResult $ DisplayErrorResult (T.pack (show err))
        Right parsed -> do 
          evald <- evalTutorialD conn parsed
          displayOpResult evald
      reprLoop config conn
