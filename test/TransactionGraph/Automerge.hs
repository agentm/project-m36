import Test.HUnit
import ProjectM36.Client
import ProjectM36.Relation
import qualified Data.Set as S
import TutorialD.Interpreter.TestBase

import System.Exit
import System.IO.Temp
import System.FilePath
import Control.Exception.Base

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  
testList :: Test
testList = TestList [testAutomergeSuccess,
                     testAutomergeFailure,
                     testAutomergeReconnect]
           
checkEither :: IO (Either RelationalError a) -> IO a
checkEither io = do
  ret <- io
  case ret of
    Left err -> assertFailure (show err) >> undefined
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
  
  let predi = eqAttr
      eqAttr key = AttributeEqualityPredicate "s#" (NakedAtomExpr (TextAtom key))
  result <- checkEither $ executeRelationalExpr sessionPastId conn (Project (AttributeNames S.empty) (Restrict (predi "S6") (RelationVariable "s" ())))
  assertEqual "new S6" relationTrue result

  result' <- checkEither $ executeRelationalExpr sessionPastId conn (Project (AttributeNames S.empty) (Restrict (predi "S7") (RelationVariable "s" ())))
  
  assertEqual "new S7" relationTrue result'
  
  eHeadName <- headName sessionPastId conn
  assertEqual "back on master" (Right "master") eHeadName
  
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
  
  _ <- checkEither $ executeRelationalExpr sessionPastId conn (RelationVariable "s" ())
  
  assertEqual "merge failure" (Left (InclusionDependencyCheckError "s_pkey")) mergeRes
  
--reported as #128
testAutomergeReconnect :: Test
testAutomergeReconnect = TestCase $ withSystemTempDirectory "m36testdb" $ \tempdir -> do
  let repro = do
          conn <- unsafeLeftCrash =<< connectProjectM36 (InProcessConnectionInfo (CrashSafePersistence (tempdir </> "test.db")) emptyNotificationCallback [])
          sess <- unsafeLeftCrash =<< createSessionAtHead conn "master"
          autoMergeToHead sess conn UnionMergeStrategy "master"
        -- commit sess conn
                  
      unsafeLeftCrash :: Show e => Either e a -> IO a
      unsafeLeftCrash = either (throwIO . userError . show) pure
  _ <- repro 
  _ <- repro
  pure ()
    
  
  
  
