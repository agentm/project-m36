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
                       ConvenienceGraphOp ConvenienceTransactionGraphOperator |
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
                     fmap ImportRelVarOp (importCSVP <* eof) <|>
                     fmap ImportDBContextOp (tutdImportP <* eof) <|>
                     fmap RelVarExportOp (exportCSVP <* eof) <|>
                     fmap DatabaseContextIOExprOp (dbContextIOExprP <* eof)
                     
-- the safe interpreter never reads or writes the file system
safeInterpreterParserP :: Parser ParsedOperation
safeInterpreterParserP = fmap RODatabaseContextOp (roDatabaseContextOperatorP <* eof) <|>
                         fmap InfoOp (infoOpP <* eof) <|>
                         fmap GraphOp (transactionGraphOpP <* eof) <|>
                         fmap ConvenienceGraphOp (convenienceTransactionGraphOpP <* eof) <|>
                         fmap ROGraphOp (roTransactionGraphOpP <* eof) <|>
                         fmap DatabaseContextExprOp (databaseExprOpP <* eof) <|>
                         fmap ImportBasicExampleOp (importBasicExampleOperatorP <* eof) <|>
                         fmap TransGraphRelationalOp (transGraphRelationalOpP <* eof) <|>
                         fmap SchemaOp (schemaOperatorP <* eof)

promptText :: Either RelationalError HeadName -> Either RelationalError SchemaName -> StringType
promptText eHeadName eSchemaName = "TutorialD (" <> transInfo <> "): "
  where
    transInfo = either (const "<unknown>") id eHeadName <> "/" <> either (const "<no schema>") id eSchemaName
          
parseTutorialD :: T.Text -> Either (ParseError Char Dec) ParsedOperation
parseTutorialD = parse interpreterParserP ""

--only parse tutoriald which doesn't result in file I/O
safeParseTutorialD :: T.Text -> Either (ParseError Char Dec) ParsedOperation
safeParseTutorialD = parse safeInterpreterParserP ""

data SafeEvaluationFlag = SafeEvaluation | UnsafeEvaluation deriving (Eq)

--execute the operation and display result
evalTutorialD :: C.SessionId -> C.Connection -> SafeEvaluationFlag -> ParsedOperation -> IO TutorialDOperatorResult
evalTutorialD sessionId conn safe expr = case expr of
  --this does not pass through the ProjectM36.Client library because the operations
  --are specific to the interpreter, though some operations may be of general use in the future
  (RODatabaseContextOp execOp) -> 
    evalRODatabaseContextOp sessionId conn execOp
    
  (DatabaseContextExprOp execOp) -> 
    eHandler $ C.executeDatabaseContextExpr sessionId conn execOp 
      
  (DatabaseContextIOExprOp execOp) -> 
    if needsSafe then
      unsafeError
      else
      eHandler $ C.executeDatabaseContextIOExpr sessionId conn execOp
    
  (GraphOp execOp) -> 
    eHandler $ C.executeGraphExpr sessionId conn execOp
    
  (ConvenienceGraphOp execOp) ->
    eHandler $ evalConvenienceGraphOp sessionId conn execOp

  (ROGraphOp execOp) -> do
    opResult <- evalROGraphOp sessionId conn execOp
    case opResult of 
      Left err -> barf err
      Right rel -> pure (DisplayRelationResult rel)
      
  (SchemaOp execOp) ->
    eHandler $ evalSchemaOperator sessionId conn execOp
      
  (ImportRelVarOp execOp@(RelVarDataImportOperator relVarName _ _)) -> 
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
  
  (ImportDBContextOp execOp) -> 
    if needsSafe then
      unsafeError
      else do
      eErr <- evalDatabaseContextDataImportOperator execOp
      case eErr of
        Left err -> barf err
        Right dbexprs -> evalTutorialD sessionId conn safe (DatabaseContextExprOp dbexprs)
      
  (InfoOp execOp) -> 
    if needsSafe then
      unsafeError
      else
      case evalInformationOperator execOp of
        Left err -> pure (DisplayErrorResult err)
        Right info -> pure (DisplayResult info)
      
  (RelVarExportOp execOp@(RelVarDataExportOperator relExpr _ _)) ->
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
  (TransGraphRelationalOp execOp) ->
    evalTransGraphRelationalOp sessionId conn execOp
  where
    needsSafe = safe == SafeEvaluation
    unsafeError = pure $ DisplayErrorResult "File I/O operation prohibited."
    barf :: RelationalError -> IO TutorialDOperatorResult
    barf (ScriptError (OtherScriptCompilationError errStr)) = pure (DisplayErrorResult (T.pack errStr))
    barf err = return $ DisplayErrorResult (T.pack (show err))
    eHandler io = do
      eErr <- io
      case eErr of
        Left err -> barf err
        Right () -> return QuietSuccessResult
      
type GhcPkgPath = String  
type TutorialDExec = String
  
data InterpreterConfig = LocalInterpreterConfig PersistenceStrategy HeadName (Maybe TutorialDExec) [GhcPkgPath] |
                         RemoteInterpreterConfig C.NodeId C.DatabaseName HeadName (Maybe TutorialDExec)

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
      runTutorialD sessionId conn (T.pack line)
      reprLoop config sessionId conn

runTutorialD :: C.SessionId -> C.Connection -> T.Text -> IO ()
runTutorialD sessionId conn tutd = 
  case parseTutorialD tutd of
    Left err -> 
      displayOpResult $ DisplayParseErrorResult 0 err
    Right parsed ->
      catchJust (\exc -> if exc == C.RequestTimeoutException then Just exc else Nothing) (do
        evald <- evalTutorialD sessionId conn UnsafeEvaluation parsed
        displayOpResult evald)
        (\_ -> displayOpResult (DisplayErrorResult "Request timed out."))
