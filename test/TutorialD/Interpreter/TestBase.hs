module TutorialD.Interpreter.TestBase where
import ProjectM36.Client
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import ProjectM36.DateExamples
import ProjectM36.DatabaseContext
import Test.HUnit
import Data.Text

dateExamplesConnection :: NotificationCallback -> IO (SessionId, Connection)
dateExamplesConnection callback = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence callback [])
  case dbconn of 
    Left err -> error (show err)
    Right conn -> do
      eSessionId <- createSessionAtHead conn "master"
      case eSessionId of
        Left err -> error (show err)
        Right sessionId -> do
          executeDatabaseContextExpr sessionId conn (databaseContextAsDatabaseContextExpr dateExamples) >>= eitherFail
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
        DisplayResult _ -> assertFailure "display?"
        DisplayIOResult _ -> assertFailure "displayIO?"
        DisplayRelationResult _ -> assertFailure "displayrelation?"
        DisplayDataFrameResult _ -> assertFailure "displaydataframe?"
        DisplayParseErrorResult _ _ -> assertFailure "displayparseerrorresult?"
        DisplayErrorResult err -> assertFailure (show tutd ++ ": " ++ show err)        
        QuietSuccessResult -> pure ()
        
expectTutorialDErr :: SessionId -> Connection -> (Text -> Bool) -> Text -> IO ()        
expectTutorialDErr sessionId conn matchFunc tutd = case parseTutorialD tutd of
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
        DisplayErrorResult err -> assertBool (unpack tutd ++ " match error on: " ++ unpack err) (matchFunc err)
        QuietSuccessResult -> pure ()
        
eitherFail :: Either RelationalError a -> IO ()
eitherFail (Left err) = assertFailure (show err)
eitherFail (Right _) = pure ()
