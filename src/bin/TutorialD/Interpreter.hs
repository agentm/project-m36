{-# LANGUAGE GADTs, LambdaCase, CPP #-}
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

import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.List (isPrefixOf)
import Control.Exception
import System.Exit
import Data.Either (fromRight)

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
    transInfo = fromRight "<unknown>" eHeadName <> "/" <> fromRight "<no schema>" eSchemaName

parseTutorialD :: T.Text -> Either ParserError ParsedOperation
parseTutorialD = parse interpreterParserP ""

--only parse tutoriald which doesn't result in file I/O
safeParseTutorialD :: T.Text -> Either ParserError ParsedOperation
safeParseTutorialD = parse safeInterpreterParserP ""

data SafeEvaluationFlag = SafeEvaluation | UnsafeEvaluation deriving (Eq)

type InteractiveConsole = Bool

evalTutorialD :: C.SessionId -> C.Connection -> SafeEvaluationFlag -> ParsedOperation -> IO TutorialDOperatorResult
evalTutorialD sessionId conn safe = evalTutorialDInteractive sessionId conn safe False

--execute the operation and display result
evalTutorialDInteractive :: C.SessionId -> C.Connection -> SafeEvaluationFlag -> InteractiveConsole -> ParsedOperation -> IO TutorialDOperatorResult
evalTutorialDInteractive sessionId conn safe interactive expr = case expr of
  --this does not pass through the ProjectM36.Client library because the operations
  --are specific to the interpreter, though some operations may be of general use in the future
  (RODatabaseContextOp execOp) -> do
    res <- evalRODatabaseContextOp sessionId conn execOp
    case res of
      QuitResult -> if safe == UnsafeEvaluation && interactive then
                      putStrLn "Goodbye." >> exitSuccess
                    else
                      pure res
      _ -> pure res

  (DatabaseContextExprOp execOp) ->
    eHandler $ C.executeDatabaseContextExpr sessionId conn execOp

  (DatabaseContextIOExprOp execOp) ->
    if needsSafe then
      unsafeError
      else
      eHandler $ C.executeDatabaseContextIOExpr sessionId conn execOp

  (GraphOp execOp) -> do
    -- warn if the graph op could cause uncommited changes to be discarded
    eIsDirty <- C.disconnectedTransactionIsDirty sessionId conn
    let runGraphOp = eHandler $ C.executeGraphExpr sessionId conn execOp
        settings = Settings {complete = noCompletion,
                             historyFile = Nothing,
                             autoAddHistory = False}
    case eIsDirty of
      Left err -> barf err
      Right False -> runGraphOp
      Right True -> do
        cancel <- runInputT settings $ do
          let promptDiscardChanges = do
                isatty <- haveTerminalUI
                if isatty && interactive && execOp /= Commit && execOp /= Rollback then do
                  mYesOrNo <- getInputLine "The current transaction has uncommitted changes. If you continue, the changes will be lost. Continue? (Y/n): "
                  case mYesOrNo of
                    Nothing -> promptDiscardChanges
                    Just "" -> promptDiscardChanges
                    Just yesOrNo -> pure (not ("Y" `isPrefixOf` yesOrNo))
                  else
                  pure False
          promptDiscardChanges
        if cancel then
          pure (DisplayErrorResult "Graph operation cancelled.")
          else
          runGraphOp

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
          eTConsMap <- C.typeConstructorMapping sessionId conn
          case eTConsMap of
            Left err -> barf err
            Right tConsMap -> do
              exprErr <- evalRelVarDataImportOperator execOp tConsMap (attributes importType)
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
    barf (ParseError err) = pure (DisplayErrorResult err)
    barf err = return $ DisplayErrorResult (T.pack (show err))
    eHandler io = do
      eErr <- io
      case eErr of
        Left err -> barf err
        Right () -> return QuietSuccessResult

type GhcPkgPath = String
type TutorialDExec = String
type CheckFS = Bool

data InterpreterConfig = LocalInterpreterConfig PersistenceStrategy HeadName (Maybe TutorialDExec) [GhcPkgPath] CheckFS |
                         RemoteInterpreterConfig C.Hostname C.Port C.DatabaseName HeadName (Maybe TutorialDExec) CheckFS

outputNotificationCallback :: C.NotificationCallback
outputNotificationCallback notName evaldNot = hPutStrLn stderr $ "Notification received " ++ show notName ++ ":\n" ++ "\n" ++ prettyEvaluatedNotification evaldNot

prettyEvaluatedNotification :: C.EvaluatedNotification -> String
prettyEvaluatedNotification eNotif = let eRelShow eRel = case eRel of
                                           Left err -> show err
                                           Right reportRel -> T.unpack (showRelation reportRel) in
  eRelShow (C.reportOldRelation eNotif) <> "\n" <> eRelShow (C.reportNewRelation eNotif)

reprLoop :: InterpreterConfig -> C.SessionId -> C.Connection -> IO ()
reprLoop config sessionId conn = do
  homeDirectory <- getHomeDirectory
  let settings = defaultSettings {historyFile = Just (homeDirectory ++ "/.tutd_history")}
  eHeadName <- C.headName sessionId conn
  eSchemaName <- C.currentSchemaName sessionId conn
  let prompt = promptText eHeadName eSchemaName
      catchInterrupt = handleJust (\case
                                      UserInterrupt -> Just Nothing
                                      _ -> Nothing) (\_ -> do
                                                        hPutStrLn stderr "Statement cancelled. Use \":quit\" to exit tutd."
                                                        pure (Just ""))
  maybeLine <- catchInterrupt $ runInputT settings $ getInputLine (T.unpack prompt)
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      runTutorialD sessionId conn (Just (T.length prompt)) (T.pack line)
      reprLoop config sessionId conn


runTutorialD :: C.SessionId -> C.Connection -> Maybe PromptLength -> T.Text -> IO ()
runTutorialD sessionId conn mPromptLength tutd =
  case parseTutorialD tutd of
    Left err ->
      displayOpResult $ DisplayParseErrorResult mPromptLength err
    Right parsed ->
      catchJust (\exc -> if exc == C.RequestTimeoutException then Just exc else Nothing) (do
        evald <- evalTutorialDInteractive sessionId conn UnsafeEvaluation True parsed
        displayOpResult evald)
        (\_ -> displayOpResult (DisplayErrorResult "Request timed out."))
