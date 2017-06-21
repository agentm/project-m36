--tests which cover multi-process access to the same database directory
import Test.HUnit
import ProjectM36.Client
import ProjectM36.Error

import System.IO.Temp
import System.Exit
import System.FilePath

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess  
  
assertIOEither :: (Show a) => IO (Either a b) -> IO b
assertIOEither x = do
  ret <- x
  case ret of
    Left err -> assertFailure (show err) >> undefined
    Right val -> pure val
  
assertIONothing :: (Show a) => IO (Maybe a) -> IO ()
assertIONothing x = do
  ret <- x
  case ret of
    Nothing -> pure ()
    Just err -> assertFailure (show err)
  
testList :: Test
testList = TestList [testMultipleProcessAccess]

testMultipleProcessAccess :: Test
testMultipleProcessAccess = TestCase $ do
  withSystemTempDirectory "pm36" $ \tmpdir -> do
    let connInfo = InProcessConnectionInfo (MinimalPersistence dbdir) emptyNotificationCallback []
        master = "master"
        dudExpr = Assign "x" (RelationVariable "true" ())
        dbdir = tmpdir </> "db"
    conn1 <- assertIOEither $ connectProjectM36 connInfo
    conn2 <- assertIOEither $ connectProjectM36 connInfo
    session1 <- assertIOEither (createSessionAtHead master conn1)
    session2 <- assertIOEither (createSessionAtHead master conn2)
    --add a commit on conn1 which conn2 doesn't know about
    assertIONothing $ executeDatabaseContextExpr session1 conn1 dudExpr
    assertIONothing $ commit session1 conn1 ForbidEmptyCommitOption
    
    assertIONothing $ executeDatabaseContextExpr session2 conn2 dudExpr
    mHeadId <- headTransactionId session2 conn2
    headId <- case mHeadId of
      Nothing -> assertFailure "headTransactionId failed" >> undefined
      Just x -> pure x
    res <- commit session2 conn2 ForbidEmptyCommitOption
    assertEqual "commit should fail" (Just (TransactionIsNotAHeadError headId)) res