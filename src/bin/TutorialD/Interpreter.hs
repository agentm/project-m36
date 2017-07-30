{-# LANGUAGE GADTs #-}
module TutorialD.Interpreter where
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RODatabaseContextOperator
import TutorialD.Interpreter.DatabaseContextExpr
import TutorialD.Interpreter.TransactionGraphOperator
import TutorialD.Interpreter.InformationOperator
import TutorialD.Interpreter.DatabaseContextIOOperator
import TutorialD.Interpreter.TransGraphRelationalOperator
import TutorialD.Interpreter.SchemaOperator

import TutorialD.Interpreter.Import.CSV
import TutorialD.Interpreter.Import.TutorialD
import TutorialD.Interpreter.Import.BasicExamples
import TutorialD.Interpreter.Import.Base

import TutorialD.Interpreter.Export.CSV
import TutorialD.Interpreter.Export.Base

import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Relation.Show.Term
import ProjectM36.TransactionGraph
import qualified ProjectM36.Client as C
import ProjectM36.Relation (attributes)

import Text.Megaparsec
import Text.Megaparsec.Text
import Control.Monad.State
import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)
import Data.Monoid
import Control.Exception

{-
context ops are read-only operations which only operate on the database context (relvars and constraints)
database ops are read-write operations which change the database context (such as relvar assignment)
graph ops are read-write operations which change the transaction graph
-}
data ParsedOperation = RODatabaseContextOp RODatabaseContextOperator |
                       DatabaseContextExprOp DatabaseContextExpr |
                       DatabaseContextIOExprOp DatabaseContextIOExpr |
                       InfoOp InformationOperator |
                       GraphOp TransactionGraphOperator |
                       ROGraphOp ROTransactionGraphOperator |
                       ImportRelVarOp RelVarDataImportOperator |
                       ImportDBContextOp DatabaseContextDataImportOperator |
                       ImportBasicExampleOp ImportBasicExampleOperator |
                       RelVarExportOp RelVarDataExportOperator |
                       TransGraphRelationalOp TransGraphRelationalOperator |
                       SchemaOp SchemaOperator
                       deriving (Show)

interpreterParserP :: Parser ParsedOperation
interpreterParserP = safeInterpreterParserP <|>
                     liftM ImportRelVarOp (importCSVP <* eof) <|>
                     liftM ImportDBContextOp (tutdImportP <* eof) <|>
                     liftM RelVarExportOp (exportCSVP <* eof) <|>
                     liftM DatabaseContextIOExprOp (dbContextIOExprP <* eof)
                     
-- the safe interpreter never reads or writes the file system
safeInterpreterParserP :: Parser ParsedOperation
safeInterpreterParserP = liftM RODatabaseContextOp (roDatabaseContextOperatorP <* eof) <|>
                         liftM InfoOp (infoOpP <* eof) <|>
                         liftM GraphOp (transactionGraphOpP <* eof) <|>
                         liftM ROGraphOp (roTransactionGraphOpP <* eof) <|>
                         liftM DatabaseContextExprOp (databaseExprOpP <* eof) <|>
                         liftM ImportBasicExampleOp (importBasicExampleOperatorP <* eof) <|>
                         liftM TransGraphRelationalOp (transGraphRelationalOpP <* eof) <|>
                         liftM SchemaOp (schemaOperatorP <* eof)

promptText :: Either RelationalError HeadName -> Either RelationalError SchemaName -> StringType
promptText eHeadName eSchemaName = "TutorialD (" <> transInfo <> "): "
  where
    transInfo = either (const "<unknown>") id eHeadName <> "/" <> either (const "<no schema>") id eSchemaName
          
parseTutorialD :: T.Text -> Either (ParseError Char Dec) ParsedOperation
parseTutorialD inputString = parse interpreterParserP "" inputString

--only parse tutoriald which doesn't result in file I/O
safeParseTutorialD :: T.Text -> Either (ParseError Char Dec) ParsedOperation
safeParseTutorialD inputString = parse safeInterpreterParserP "" inputString

data SafeEvaluationFlag = SafeEvaluation | UnsafeEvaluation deriving (Eq)

--execute the operation and display result
evalTutorialD :: C.SessionId -> C.Connection -> SafeEvaluationFlag -> ParsedOperation -> IO (TutorialDOperatorResult)
evalTutorialD sessionId conn safe expr = case expr of
  
  --this does not pass through the ProjectM36.Client library because the operations
  --are specific to the interpreter, though some operations may be of general use in the future
  (RODatabaseContextOp execOp) -> do
    evalRODatabaseContextOp sessionId conn execOp
    
  (DatabaseContextExprOp execOp) -> do 
    maybeErr <- C.executeDatabaseContextExpr sessionId conn execOp 
    case maybeErr of
      Left err -> barf err
      Right () -> return QuietSuccessResult
      
  (DatabaseContextIOExprOp execOp) -> do
    if needsSafe then
      unsafeError
      else do
      mErr <- C.executeDatabaseContextIOExpr sessionId conn execOp
      case mErr of
        Just err -> barf err
        Nothing -> pure QuietSuccessResult
    
  (GraphOp execOp) -> do
    maybeErr <- C.executeGraphExpr sessionId conn execOp
    case maybeErr of
      Left err -> barf err
      Right () -> return QuietSuccessResult

  (ROGraphOp execOp) -> do
    opResult <- evalROGraphOp sessionId conn execOp
    case opResult of 
      Left err -> barf err
      Right rel -> pure (DisplayRelationResult rel)
      
  (SchemaOp execOp) -> do
    opResult <- evalSchemaOperator sessionId conn execOp
    case opResult of
      Left err -> barf err
      Right () -> pure QuietSuccessResult
      
  (ImportRelVarOp execOp@(RelVarDataImportOperator relVarName _ _)) -> do
    if needsSafe then
      unsafeError
      else do
      -- collect attributes from relvar name
      -- is there a race condition here? The attributes of the relvar may have since changed, no?
      eImportType <- C.typeForRelationalExpr sessionId conn (RelationVariable relVarName ())
      case eImportType of
        Left err -> barf err
        Right importType -> do
          exprErr <- evalRelVarDataImportOperator execOp (attributes importType)
          case exprErr of
            Left err -> barf err
            Right dbexpr -> evalTutorialD sessionId conn safe (DatabaseContextExprOp dbexpr)
  
  (ImportDBContextOp execOp) -> do
    if needsSafe then
      unsafeError
      else do
      mDbexprs <- evalDatabaseContextDataImportOperator execOp
      case mDbexprs of 
        Left err -> barf err
        Right dbexprs -> evalTutorialD sessionId conn safe (DatabaseContextExprOp dbexprs)
      
  (InfoOp execOp) -> do
    if needsSafe then
      unsafeError
      else
      case evalInformationOperator execOp of
        Left err -> barf err
        Right info -> pure (DisplayResult info)
      
  (RelVarExportOp execOp@(RelVarDataExportOperator relExpr _ _)) -> do
    --eval relexpr to relation and pass to export function
    if needsSafe then
      unsafeError
      else do
        eRel <- C.executeRelationalExpr sessionId conn relExpr
        case eRel of
          Left err -> barf err
          Right rel -> do
            exportResult <- evalRelVarDataExportOperator execOp rel
            case exportResult of
              Just err -> barf err
              Nothing -> pure QuietSuccessResult
  (ImportBasicExampleOp execOp) -> do
    let dbcontextexpr = evalImportBasicExampleOperator execOp
    evalTutorialD sessionId conn safe (DatabaseContextExprOp dbcontextexpr)
  (TransGraphRelationalOp execOp) -> do
    evalTransGraphRelationalOp sessionId conn execOp
  where
    needsSafe = safe == SafeEvaluation
    unsafeError = pure $ DisplayErrorResult "File I/O operation prohibited."
    barf err = return $ DisplayErrorResult (T.pack (show err))
  
type GhcPkgPath = String  
data InterpreterConfig = LocalInterpreterConfig PersistenceStrategy HeadName [GhcPkgPath] |
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
  eHeadName <- C.headName sessionId conn
  eSchemaName <- C.currentSchemaName sessionId conn
  let prompt = promptText eHeadName eSchemaName
  maybeLine <- runInputT settings $ getInputLine (T.unpack prompt)
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      case parseTutorialD (T.pack line) of
        Left err -> do
          displayOpResult $ DisplayParseErrorResult (T.length prompt) err
        Right parsed -> do 
          catchJust (\exc -> if exc == C.RequestTimeoutException then Just exc else Nothing) (do
            evald <- evalTutorialD sessionId conn UnsafeEvaluation parsed
            displayOpResult evald)
            (\_ -> displayOpResult (DisplayErrorResult "Request timed out."))
      reprLoop config sessionId conn
