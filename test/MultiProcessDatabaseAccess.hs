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
  
testList :: Test
testList = TestList [testMultipleProcessAccess]

testMultipleProcessAccess :: Test
testMultipleProcessAccess = TestCase $ 
  withSystemTempDirectory "pm36" $ \tmpdir -> do
    let connInfo = InProcessConnectionInfo (MinimalPersistence dbdir) emptyNotificationCallback []
        master = "master"
        dudExpr = Assign "x" (RelationVariable "true" ())
        dbdir = tmpdir </> "db"
    conn1 <- assertIOEither $ connectProjectM36 connInfo
    conn2 <- assertIOEither $ connectProjectM36 connInfo
    session1 <- assertIOEither (createSessionAtHead conn1 master)
    session2 <- assertIOEither (createSessionAtHead conn2 master)
    --add a commit on conn1 which conn2 doesn't know about
    assertIOEither $ executeDatabaseContextExpr session1 conn1 dudExpr
    assertIOEither $ commit session1 conn1
    
    assertIOEither $ executeDatabaseContextExpr session2 conn2 dudExpr
    eHeadId <- headTransactionId session2 conn2
    headId <- case eHeadId of
      Left err -> assertFailure ("headTransactionId failed: " ++ show err) >> undefined
      Right x -> pure x
    res <- commit session2 conn2 
    assertEqual "commit should fail" (Left (TransactionIsNotAHeadError headId)) res