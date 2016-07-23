{-# LANGUAGE GADTs #-}
module TutorialD.Interpreter where
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RODatabaseContextOperator
import TutorialD.Interpreter.DatabaseContextExpr
import TutorialD.Interpreter.TransactionGraphOperator
import TutorialD.Interpreter.InformationOperator

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
import System.IO (hPutStrLn, stderr)

{-
context ops are read-only operations which only operate on the database context (relvars and constraints)
database ops are read-write operations which change the database context (such as relvar assignment)
graph ops are read-write operations which change the transaction graph
-}
data ParsedOperation = RODatabaseContextOp RODatabaseContextOperator |
                       DatabaseContextExprOp DatabaseContextExpr |
                       InfoOp InformationOperator |
                       GraphOp TransactionGraphOperator |
                       ROGraphOp ROTransactionGraphOperator |
                       ImportRelVarOp RelVarDataImportOperator |
                       ImportDBContextOp DatabaseContextDataImportOperator |
                       RelVarExportOp RelVarDataExportOperator

interpreterParserP :: Parser ParsedOperation
interpreterParserP = liftM RODatabaseContextOp (roDatabaseContextOperatorP <* eof) <|>
                     liftM InfoOp (infoOpP <* eof) <|>
                     liftM GraphOp (transactionGraphOpP <* eof) <|>
                     liftM ROGraphOp (roTransactionGraphOpP <* eof) <|>
                     liftM DatabaseContextExprOp (databaseExprOpP <* eof) <|>
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
evalTutorialD :: C.SessionId -> C.Connection -> ParsedOperation -> IO (TutorialDOperatorResult)
evalTutorialD sessionId conn expr = case expr of
  
  --this does not pass through the ProjectM36.Client library because the operations
  --are specific to the interpreter, though some operations may be of general use in the future
  (RODatabaseContextOp execOp) -> do
    evalRODatabaseContextOp sessionId conn execOp
    
  (DatabaseContextExprOp execOp) -> do 
    maybeErr <- C.executeDatabaseContextExpr sessionId conn execOp 
    case maybeErr of
      Just err -> barf err
      Nothing -> return QuietSuccessResult
    
  (GraphOp execOp) -> do
    maybeErr <- C.executeGraphExpr sessionId conn execOp
    case maybeErr of
      Just err -> barf err
      Nothing -> return QuietSuccessResult

  (ROGraphOp execOp) -> do
    opResult <- evalROGraphOp sessionId conn execOp
    case opResult of 
      Left err -> barf err
      Right rel -> return $ DisplayResult $ showRelation rel
      
  (ImportRelVarOp execOp@(RelVarDataImportOperator relVarName _ _)) -> do
    -- collect attributes from relvar name
    -- is there a race condition here? The attributes of the relvar may have since changed, no?
    eImportType <- C.typeForRelationalExpr sessionId conn (RelationVariable relVarName)
    case eImportType of
      Left err -> barf err
      Right importType -> do
        exprErr <- evalRelVarDataImportOperator execOp (attributes importType)
        case exprErr of
          Left err -> barf err
          Right dbexpr -> evalTutorialD sessionId conn (DatabaseContextExprOp dbexpr)
  
  (ImportDBContextOp execOp) -> do
    mDbexprs <- evalDatabaseContextDataImportOperator execOp
    case mDbexprs of 
      Left err -> barf err
      Right dbexprs -> evalTutorialD sessionId conn (DatabaseContextExprOp dbexprs)
      
  (InfoOp execOp) -> do
    case evalInformationOperator execOp of
      Left err -> barf err
      Right info -> pure (DisplayResult info)
      
  (RelVarExportOp execOp@(RelVarDataExportOperator relExpr _ _)) -> do
    --eval relexpr to relation and pass to export function
    eRel <- C.executeRelationalExpr sessionId conn relExpr
    case eRel of
      Left err -> barf err
      Right rel -> do
        exportResult <- evalRelVarDataExportOperator execOp rel
        case exportResult of
          Just err -> barf err
          Nothing -> pure QuietSuccessResult
  where
    barf err = return $ DisplayErrorResult (T.pack (show err))
  
data InterpreterConfig = LocalInterpreterConfig PersistenceStrategy HeadName|
                         RemoteInterpreterConfig C.NodeId C.DatabaseName HeadName

outputNotificationCallback :: C.NotificationCallback
outputNotificationCallback notName evaldNot = hPutStrLn stderr $ "Notification received " ++ show notName ++ ":\n" ++ show (reportExpr (C.notification evaldNot)) ++ "\n" ++ prettyEvaluatedNotification evaldNot

prettyEvaluatedNotification :: C.EvaluatedNotification -> String
prettyEvaluatedNotification eNotif = case C.reportRelation eNotif of
  Left err -> show err
  Right reportRel -> T.unpack (showRelation reportRel)

reprLoop :: InterpreterConfig -> C.SessionId -> C.Connection -> IO ()
reprLoop config sessionId conn = do
  homeDirectory <- getHomeDirectory
  let settings = defaultSettings {historyFile = Just (homeDirectory ++ "/.tutd_history")}
  mHeadName <- C.headName sessionId conn
  maybeLine <- runInputT settings $ getInputLine (T.unpack (promptText mHeadName))
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      case parseTutorialD line of
        Left err -> do
          displayOpResult $ DisplayErrorResult (T.pack (show err))
        Right parsed -> do 
          evald <- evalTutorialD sessionId conn parsed
          displayOpResult evald
      reprLoop config sessionId conn
