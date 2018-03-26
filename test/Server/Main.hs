{-# LANGUAGE CPP #-}
{-
test client/server interaction
-}
import Test.HUnit
import ProjectM36.Client
import qualified ProjectM36.Client as C
import ProjectM36.Server.EntryPoints
import ProjectM36.Server
import ProjectM36.Server.Config
import ProjectM36.Relation
import ProjectM36.TupleSet
import ProjectM36.IsomorphicSchema
import ProjectM36.Base

import System.Exit

import Control.Concurrent
import Network.Transport (EndPointAddress)
import Network.Transport.TCP (encodeEndPointAddress, decodeEndPointAddress)
import Data.Either (isRight)
import Control.Exception
import System.IO.Temp
import System.FilePath
#if defined(linux_HOST_OS)
import System.Directory
#endif

testList :: SessionId -> Connection -> MVar () -> Test
testList sessionId conn notificationTestMVar = TestList $ serverTests ++ sessionTests
  where
    sessionTests = map (\t -> t sessionId conn) [
      testRelationalExpr,
      testSchemaExpr,
      testTypeForRelationalExpr,  
      testDatabaseContextExpr,
      testGraphExpr,
      testPlanForDatabaseContextExpr,
      testTransactionGraphAsRelation,
      testHeadTransactionId,
      testHeadName,
      testSession,
      testRelationVariableSummary,
      testNotification notificationTestMVar
      ] 
    serverTests = [testRequestTimeout, testFileDescriptorCount]

main :: IO ()
main = do
  (serverAddress, _) <- launchTestServer 0
  notificationTestMVar <- newEmptyMVar 
  eTestConn <- testConnection serverAddress notificationTestMVar
  case eTestConn of
    Left err -> print err >> exitFailure
    Right (session, testConn) -> do
      tcounts <- runTestTT (testList session testConn notificationTestMVar)
      if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

{-main = do
    tcounts <- runTestTT (TestList [testRequestTimeout])
    if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess-}
                                                                     
testDatabaseName :: DatabaseName
testDatabaseName = "test"

testConnection :: EndPointAddress -> MVar () -> IO (Either ConnectionError (SessionId, Connection))
testConnection serverAddress mvar = do
  let Just (host, service, _) = decodeEndPointAddress serverAddress
      serverAddress' = encodeEndPointAddress host service 1  
  let connInfo = RemoteProcessConnectionInfo testDatabaseName (NodeId serverAddress') (testNotificationCallback mvar)
  --putStrLn ("testConnection: " ++ show serverAddress)
  eConn <- connectProjectM36 connInfo
  case eConn of 
    Left err -> pure $ Left err
    Right conn -> do
      eSessionId <- createSessionAtHead conn defaultHeadName
      case eSessionId of
        Left _ -> error "failed to create session"
        Right sessionId -> pure $ Right (sessionId, conn)

-- | A version of 'launchServer' which returns the port on which the server is listening on a secondary thread
launchTestServer :: Timeout -> IO (EndPointAddress, ThreadId)
launchTestServer ti = do
  addressMVar <- newEmptyMVar
  tid <- forkIO $ 
    withSystemTempDirectory "projectm36test" $ \tempdir -> do
      let config = defaultServerConfig { databaseName = testDatabaseName, 
                                         persistenceStrategy = CrashSafePersistence (tempdir </> "db"),
                                         perRequestTimeout = ti,
                                         testMode = True,
                                         bindPort = 0
                                       }
    
      launchServer config (Just addressMVar) >> pure ()
  endPointAddress <- takeMVar addressMVar
  --liftIO $ putStrLn ("launched server on " ++ show endPointAddress)
  pure (endPointAddress, tid)
  
testRelationalExpr :: SessionId -> Connection -> Test  
testRelationalExpr sessionId conn = TestCase $ do
  relResult <- executeRelationalExpr sessionId conn (RelationVariable "true" ())
  assertEqual "invalid relation result" (Right relationTrue) relResult
  
eitherFail :: (Show e) => Either e a -> IO ()
eitherFail (Left err) = assertFailure (show err)
eitherFail (Right _) = pure ()
  
-- test adding an removing a schema against true/false relations  
testSchemaExpr :: SessionId -> Connection -> Test
testSchemaExpr sessionId conn = TestCase $ do
  result <- executeSchemaExpr sessionId conn (AddSubschema "test-schema" [IsoRename "table_dee" "true", IsoRename "table_dum" "false"])
  assertEqual "executeSchemaExpr" (Right ()) result
  result' <- executeSchemaExpr sessionId conn (RemoveSubschema "test-schema")
  assertEqual "executeSchemaExpr2" (Right ()) result'  
  
testDatabaseContextExpr :: SessionId -> Connection -> Test
testDatabaseContextExpr sessionId conn = TestCase $ do 
  let attrExprs = [AttributeAndTypeNameExpr "x" (PrimitiveTypeConstructor "Text" TextAtomType) ()]
      attrs = attributesFromList [Attribute "x" TextAtomType]
      testrv = "testrv"
  executeDatabaseContextExpr sessionId conn (Define testrv attrExprs) >>= eitherFail
  eRel <- executeRelationalExpr sessionId conn (RelationVariable testrv ())
  let expected = mkRelation attrs emptyTupleSet
  case eRel of
    Left err -> assertFailure (show err)
    Right rel -> assertEqual "dbcontext definition failed" expected (Right rel)
        
testGraphExpr :: SessionId -> Connection -> Test        
testGraphExpr sessionId conn = TestCase (executeGraphExpr sessionId conn (JumpToHead "master") >>= eitherFail)
    
testTypeForRelationalExpr :: SessionId -> Connection -> Test
testTypeForRelationalExpr sessionId conn = TestCase $ do
  relResult <- typeForRelationalExpr sessionId conn (RelationVariable "true" ())
  case relResult of
    Left err -> assertFailure (show err)
    Right rel -> assertEqual "typeForRelationalExpr failure" relationFalse rel
    
testPlanForDatabaseContextExpr :: SessionId -> Connection -> Test    
testPlanForDatabaseContextExpr sessionId conn = TestCase $ do
  let attrExprs = [AttributeAndTypeNameExpr "x" (PrimitiveTypeConstructor "Int" IntAtomType) ()]
      testrv = "testrv"
      dbExpr = Define testrv attrExprs
  planResult <- planForDatabaseContextExpr sessionId conn dbExpr
  case planResult of
    Left err -> assertFailure (show err)
    Right plan -> assertEqual "planForDatabaseContextExpr failure" dbExpr plan
        
testTransactionGraphAsRelation :: SessionId -> Connection -> Test    
testTransactionGraphAsRelation sessionId conn = TestCase $ do
  eGraph <- transactionGraphAsRelation sessionId conn
  case eGraph of
    Left err -> assertFailure (show err)
    Right _ -> pure ()
    
testHeadTransactionId :: SessionId -> Connection -> Test    
testHeadTransactionId sessionId conn = TestCase $ do
  uuid <- headTransactionId sessionId conn
  assertBool "invalid head transaction uuid" (isRight uuid)
  pure ()
  
testHeadName :: SessionId -> Connection -> Test
testHeadName sessionId conn = TestCase $ do
  eHeadName <- headName sessionId conn
  assertEqual "headName failure" (Right "master") eHeadName
  
testRelationVariableSummary :: SessionId -> Connection -> Test  
testRelationVariableSummary sessionId conn = TestCase $ do
  eRel <- C.relationVariablesAsRelation sessionId conn
  case eRel of 
    Left err -> assertFailure ("relvar summary failed " ++ show err)
    Right rel -> assertBool "invalid tuple count in relvar summary" (cardinality rel == Finite 2)
  
testSession :: SessionId -> Connection -> Test
testSession _ conn = TestCase $ do
  -- create and close a new session using AtHead and AtCommit
  eSessionId1 <- createSessionAtHead conn defaultHeadName
  case eSessionId1 of
    Left _ -> assertFailure "invalid session" 
    Right sessionId1 -> do
      eHeadId <- headTransactionId sessionId1 conn
      case eHeadId of
        Left err -> assertFailure ("invalid head id: " ++ show err)
        Right headId -> do
          eSessionId2 <- createSessionAtCommit conn headId
          assertBool ("invalid session: " ++ show eSessionId2) (isRight eSessionId2)
          closeSession sessionId1 conn

testNotificationCallback :: MVar () -> NotificationCallback
testNotificationCallback mvar _ _ = putMVar mvar ()

-- create a relvar x, add a notification on x, update x and wait for the notification
testNotification :: MVar () -> SessionId -> Connection -> Test
testNotification mvar sess conn = TestCase $ do
  let relvarx = RelationVariable "x" ()
  executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationTrue)) >>= eitherFail
  executeDatabaseContextExpr sess conn (AddNotification "test notification" relvarx relvarx relvarx) >>= eitherFail
  commit sess conn >>= eitherFail
  executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationFalse)) >>= eitherFail
  commit sess conn >>= eitherFail
  takeMVar mvar

testRequestTimeout :: Test
testRequestTimeout = TestCase $ do
  (serverAddress, serverTid) <- launchTestServer 1000
  unusedMVar <- newEmptyMVar       
  eTestConn <- testConnection serverAddress unusedMVar  
  --eTestConn <- testConnection (encodeEndPointAddress "127.0.0.1" "10000" 1) unusedMVar
  case eTestConn of
    Left err -> putStrLn ("failed to connect: " ++ show err) >> exitFailure
    Right (session, testConn) -> do
      res <- catchJust (\exc -> if exc == RequestTimeoutException then Just exc else Nothing) (callTestTimeout_ session testConn) (const (pure False))
      assertBool "exception was not thrown" (not res)
      killThread serverTid
      
testFileDescriptorCount :: Test
#if defined(linux_HOST_OS)
--validate that creating a server, connecting a client, and then disconnecting doesn't leak file descriptors
testFileDescriptorCount = TestCase $ do
  (serverAddress, serverTid) <- launchTestServer 1000
  unusedMVar <- newEmptyMVar
  startCount <- fdCount  
  Right (sess, testConn) <- testConnection serverAddress unusedMVar
  --add a test commit to trigger the fsync machinery
  executeDatabaseContextExpr sess testConn (Assign "x" (ExistingRelation relationFalse)) >>= eitherFail  
  commit sess testConn >>= eitherFail
  close testConn
  endCount <- fdCount
  let fd_diff = endCount - startCount
  assertBool ("fd leak: " ++ show fd_diff) (fd_diff <= 0)
  killThread serverTid

  
-- returns the number of open file descriptors -- linux only /proc usage
fdCount :: IO Int
fdCount = do
  fds <- getDirectoryContents "/proc/self/fd"
  pure (length fds)
#else 
--pass on non-linux platforms
testFileDescriptorCount = TestCase (pure ())
#endif