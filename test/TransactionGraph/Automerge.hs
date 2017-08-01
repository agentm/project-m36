import Test.HUnit
import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.Error
import qualified Data.Set as S
import TutorialD.Interpreter.TestBase

import System.Exit

import ProjectM36.Relation.Show.Term
import Data.Text

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  
testList :: Test
testList = TestList [testAutomergeSuccess,
                     testAutomergeFailure]
           
checkEither :: IO (Either RelationalError a) -> IO a
checkEither io = do
  ret <- io
  case ret of
    Left err -> assertFailure (show err)
    Right a -> pure a

testAutomergeSuccess :: Test
testAutomergeSuccess = TestCase $ do
  --create two sessions, diverge from the head, and automerge back
  (sessionId, conn) <- dateExamplesConnection emptyNotificationCallback
  let headn = "master"
  sessionPastId <- checkEither $ createSessionAtHead conn headn
  executeTutorialD sessionId conn "insert s relation{tuple{city \"New City\", s# \"S6\", sname \"Samuels\", status 50}}"
  checkEither $ commit sessionId conn
  
  executeTutorialD sessionPastId conn "insert s relation{tuple{city \"Merge City\", s# \"S7\", sname \"Mr. Merge\", status 60}}"
  
  checkEither $ autoMergeToHead sessionPastId conn UnionMergeStrategy headn
  
  --validate that both tuples are now in the head transaction
  
  let predi key = eqAttr key
      eqAttr key = AttributeEqualityPredicate "s#" (NakedAtomExpr (TextAtom key))
  result <- checkEither $ executeRelationalExpr sessionPastId conn (Project (AttributeNames S.empty) (Restrict (predi "S6") (RelationVariable "s" ())))
  assertEqual "new S6" relationTrue result

  result' <- checkEither $ executeRelationalExpr sessionPastId conn (Project (AttributeNames S.empty) (Restrict (predi "S7") (RelationVariable "s" ())))
  
  assertEqual "new S7" relationTrue result'
  
testAutomergeFailure :: Test  
testAutomergeFailure = TestCase $ do
  --create two sessions, diverge from the head, but create a union merge strategy failure
  (sessionId, conn) <- dateExamplesConnection emptyNotificationCallback
  let headn = "master"
  sessionPastId <- checkEither $ createSessionAtHead conn headn
  executeTutorialD sessionId conn "insert s relation{tuple{city \"New City\", s# \"S6\", sname \"Samuels\", status 50}}"
  checkEither $ commit sessionId conn
  
  --reuse the same id, violating the uniqueness constraint
  executeTutorialD sessionPastId conn "insert s relation{tuple{city \"Merge City\", s# \"S6\", sname \"Mr. Merge\", status 60}}"
  
  --validate that both tuples are now in the head transaction
  --should fail
  mergeRes <- autoMergeToHead sessionPastId conn UnionMergeStrategy headn
  
  p <- checkEither $ executeRelationalExpr sessionPastId conn (RelationVariable "s" ())
  
  putStrLn (unpack (showRelation p))
  
  assertEqual "merge failure" (Left (InclusionDependencyCheckError "s_pkey")) mergeRes
  
  
  
