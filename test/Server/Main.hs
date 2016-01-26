{-# LANGUAGE OverloadedStrings #-}
{-
test client/server interaction
-}
import Test.HUnit
import ProjectM36.Client
import ProjectM36.Server
import ProjectM36.Server.Config
import ProjectM36.Relation
import ProjectM36.Atom
import ProjectM36.TupleSet

import System.Exit
import Control.Concurrent
import Data.Either (isRight)
import Data.Maybe (isJust)

testList :: SessionId -> Connection -> MVar () -> Test
testList sessionId conn notificationTestMVar = TestList $ map (\t -> t sessionId conn) [
  testRelationalExpr,
  testDatabaseContextExpr,
  testGraphExpr,
  testTypeForRelationalExpr,
  testPlanForDatabaseContextExpr,
  testTransactionGraphAsRelation,
  testHeadTransactionUUID,
  testHeadName,
  testSession,
  testNotification notificationTestMVar  
  ]
           
main :: IO ()
main = do
  port <- launchTestServer
  notificationTestMVar <- newEmptyMVar 
  eTestConn <- testConnection port notificationTestMVar
  case eTestConn of
    Left err -> putStrLn (show err) >> exitFailure
    Right (session, testConn) -> do
      tcounts <- runTestTT (testList session testConn notificationTestMVar)
      if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testDatabaseName :: DatabaseName
testDatabaseName = "test"

testConnection :: Port -> MVar () -> IO (Either ConnectionError (SessionId, Connection))
testConnection port mvar = do
  let connInfo = RemoteProcessConnectionInfo testDatabaseName (createNodeId "127.0.0.1" port) (testNotificationCallback mvar)
  eConn <- connectProjectM36 connInfo
  case eConn of 
    Left err -> pure $ Left err
    Right conn -> do
      eSessionId <- createSessionAtHead defaultHeadName conn
      case eSessionId of
        Left _ -> error "failed to create session"
        Right sessionId -> pure $ Right (sessionId, conn)

-- | A version of 'launchServer' which returns the port on which the server is listening on a secondary thread
launchTestServer :: IO (Port)
launchTestServer = do
  let config = defaultServerConfig { databaseName = testDatabaseName }
  portVar <- newEmptyMVar
  _ <- forkIO $ launchServer config (Just portVar) >> pure ()
  takeMVar portVar
  
testRelationalExpr :: SessionId -> Connection -> Test  
testRelationalExpr sessionId conn = TestCase $ do
  relResult <- executeRelationalExpr sessionId conn (RelationVariable "true")
  assertEqual "invalid relation result" (Right relationTrue) relResult
  
testDatabaseContextExpr :: SessionId -> Connection -> Test
testDatabaseContextExpr sessionId conn = TestCase $ do 
  let attrs = attributesFromList [Attribute "x" intAtomType]
      testrv = "testrv"
  dbResult <- executeDatabaseContextExpr sessionId conn (Define testrv attrs)
  case dbResult of
    Just err -> assertFailure (show err)
    Nothing -> do
      eRel <- executeRelationalExpr sessionId conn (RelationVariable testrv)
      let expected = mkRelation attrs emptyTupleSet
      case eRel of
        Left err -> assertFailure (show err)
        Right rel -> assertEqual "dbcontext definition failed" expected (Right rel)
        
testGraphExpr :: SessionId -> Connection -> Test        
testGraphExpr sessionId conn = TestCase $ do
  graphResult <- executeGraphExpr sessionId conn (JumpToHead "master")
  case graphResult of
    Just err -> assertFailure (show err)
    Nothing -> pure ()
    
testTypeForRelationalExpr :: SessionId -> Connection -> Test
testTypeForRelationalExpr sessionId conn = TestCase $ do
  relResult <- typeForRelationalExpr sessionId conn (RelationVariable "true")
  case relResult of
    Left err -> assertFailure (show err)
    Right rel -> assertEqual "typeForRelationalExpr failure" relationFalse rel
    
testPlanForDatabaseContextExpr :: SessionId -> Connection -> Test    
testPlanForDatabaseContextExpr sessionId conn = TestCase $ do
  let attrs = attributesFromList [Attribute "x" intAtomType]
      testrv = "testrv"
      dbExpr = Define testrv attrs
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
    
testHeadTransactionUUID :: SessionId -> Connection -> Test    
testHeadTransactionUUID sessionId conn = TestCase $ do
  uuid <- headTransactionUUID sessionId conn
  assertBool "invalid head transaction uuid" (isJust uuid)
  pure ()
  
testHeadName :: SessionId -> Connection -> Test
testHeadName sessionId conn = TestCase $ do
  mHeadName <- headName sessionId conn
  assertEqual "headName failure" (Just "master") mHeadName
  
testSession :: SessionId -> Connection -> Test
testSession _ conn = TestCase $ do
  -- create and close a new session using AtHead and AtCommit
  eSessionId1 <- createSessionAtHead defaultHeadName conn
  case eSessionId1 of
    Left _ -> assertFailure "invalid session" 
    Right sessionId1 -> do
      mHeadUUID <- headTransactionUUID sessionId1 conn
      case mHeadUUID of
        Nothing -> assertFailure "invalid head UUID"
        Just headUUID -> do
          eSessionId2 <- createSessionAtCommit headUUID conn
          assertBool ("invalid session: " ++ show eSessionId2) (isRight eSessionId2)
          closeSession sessionId1 conn

testNotificationCallback :: MVar () -> NotificationCallback
testNotificationCallback mvar _ _ = putMVar mvar ()

-- create a relvar x, add a notification on x, update x and wait for the notification
testNotification :: MVar () -> SessionId -> Connection -> Test
testNotification mvar sess conn = TestCase $ do
  let relvarx = RelationVariable "x"
  let check x = x >>= maybe  (pure ()) (\err -> assertFailure (show err))
  check $ executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationTrue))
  check $ executeDatabaseContextExpr sess conn (AddNotification "test notification" relvarx relvarx)  
  check $ commit sess conn
  check $executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationFalse))
  check $ commit sess conn
  takeMVar mvar