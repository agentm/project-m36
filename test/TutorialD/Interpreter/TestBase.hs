module TutorialD.Interpreter.TestBase where
import ProjectM36.Client
import ProjectM36.Interpreter as I
import TutorialD.Interpreter
import ProjectM36.DateExamples
import ProjectM36.DatabaseContextExpr
import Test.HUnit
import Data.Text

import Debug.Trace

dateExamplesConnection :: NotificationCallback -> IO (SessionId, Connection)
dateExamplesConnection callback = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence callback [] basicDatabaseContext adminRoleName)
  case dbconn of 
    Left err -> error (show err)
    Right conn -> do
      eSessionId <- createSessionAtHead conn "master"
      case eSessionId of
        Left err -> error (show err)
        Right sessionId -> do
          executeDatabaseContextExpr sessionId conn (resolvedDatabaseContextAsDatabaseContextExpr dateExamples) >>= eitherFail
      --skipping atom functions for now- there are no atom function manipulation operators yet
          commit sessionId conn >>= eitherFail
          pure (sessionId, conn)

executeTutorialD :: SessionId -> Connection -> Text -> IO ()
executeTutorialD sessionId conn tutd = case parseTutorialD tutd of
    Left err -> assertFailure (show tutd ++ ": " ++ show err)
    Right parsed -> do 
      result <- evalTutorialD sessionId conn UnsafeEvaluation parsed
      case result of
        QuitResult -> assertFailure "quit?"
        DisplayResult x -> assertFailure ("display: " <> show x)
        DisplayIOResult _ -> assertFailure "displayIO?"
        DisplayRelationResult _ -> assertFailure "displayrelation?"
        DisplayDataFrameResult _ -> assertFailure "displaydataframe?"
        DisplayParseErrorResult _ _ -> assertFailure "displayparseerrorresult?"
        DisplayErrorResult err -> assertFailure (show tutd ++ ": " ++ show err)        
        I.QuietSuccessResult -> pure ()
        DisplayRelationalErrorResult err -> assertFailure ("DisplayRelationalErrorResult: " <> show err)
        DisplayHintWith _ _ -> pure ()
        
expectTutorialDErr :: SessionId -> Connection -> (Text -> Bool) -> Text -> IO ()        
expectTutorialDErr sessionId conn matchFunc tutd = case parseTutorialD tutd of
    Left err -> assertFailure (show tutd ++ ": " ++ show err)  
    Right parsed -> do
      result <- evalTutorialD sessionId conn UnsafeEvaluation parsed      
      case result of
        QuitResult -> assertFailure "quit?"
        DisplayResult x -> traceShow ("expecterr"::String, x) $ assertFailure "display?"
        DisplayIOResult _ -> assertFailure "displayIO?"
        DisplayRelationResult _ -> assertFailure "displayrelation?"
        DisplayDataFrameResult _ -> assertFailure "displaydataframe?"
        DisplayParseErrorResult _ _ -> assertFailure "displayparseerrorresult?"
        DisplayErrorResult err -> assertBool (unpack tutd ++ " match error on: " ++ unpack err) (matchFunc err)
        I.QuietSuccessResult -> pure ()
        DisplayRelationalErrorResult err -> assertFailure ("DisplayRelationalErrorResult: " <> show err)
        DisplayHintWith{} -> pure ()

expectTutorialDRelationalError :: SessionId -> Connection -> RelationalError -> Text -> IO ()
expectTutorialDRelationalError sessionId conn matchErr tutd = case parseTutorialD tutd of
    Left err -> assertFailure (show tutd ++ ": " ++ show err)  
    Right parsed -> do
      result <- evalTutorialD sessionId conn UnsafeEvaluation parsed      
      case result of
        QuitResult -> assertFailure "quit?"
        DisplayResult _ -> assertFailure "display?"
        DisplayIOResult _ -> assertFailure "displayIO?"
        DisplayRelationResult _ -> assertFailure "displayrelation?"
        DisplayDataFrameResult _ -> assertFailure "displaydataframe?"
        DisplayParseErrorResult _ _ -> assertFailure "displayparseerrorresult?"
        DisplayErrorResult err -> assertFailure (unpack err)
        I.QuietSuccessResult -> assertFailure "quietsuccess?"
        DisplayRelationalErrorResult err -> assertEqual "relational error" matchErr err
        DisplayHintWith{} -> assertFailure "displayhintwith?"

        
eitherFail :: Either RelationalError a -> IO ()
eitherFail (Left err) = assertFailure (show err)
eitherFail (Right _) = pure ()
