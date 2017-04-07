module TutorialD.Interpreter.TestBase where
import ProjectM36.Client
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import qualified ProjectM36.Base as Base
import ProjectM36.DateExamples
import Test.HUnit
import qualified Data.Map as M
import Data.Text

dateExamplesConnection :: NotificationCallback -> IO (SessionId, Connection)
dateExamplesConnection callback = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence callback [])
  let incDeps = Base.inclusionDependencies dateExamples
  case dbconn of 
    Left err -> error (show err)
    Right conn -> do
      eSessionId <- createSessionAtHead "master" conn
      case eSessionId of
        Left err -> error (show err)
        Right sessionId -> do
          mapM_ (\(rvName,rvRel) -> executeDatabaseContextExpr sessionId conn (Assign rvName (Base.ExistingRelation rvRel))) (M.toList (Base.relationVariables dateExamples))
          mapM_ (\(idName,incDep) -> executeDatabaseContextExpr sessionId conn (AddInclusionDependency idName incDep)) (M.toList incDeps)
      --skipping atom functions for now- there are no atom function manipulation operators yet
          commit sessionId conn >>= maybeFail
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
        DisplayParseErrorResult _ _ -> assertFailure "displayparseerrorresult?"
        DisplayErrorResult err -> assertFailure (show err)        
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
        DisplayParseErrorResult _ _ -> assertFailure "displayparseerrorresult?"
        DisplayErrorResult err -> assertBool ("match error on: " ++ unpack err) (matchFunc err)
        QuietSuccessResult -> pure ()
        
maybeFail :: (Show a) => Maybe a -> IO ()
maybeFail (Just err) = assertFailure (show err)
maybeFail Nothing = return ()
