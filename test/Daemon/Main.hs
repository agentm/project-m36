{-# LANGUAGE OverloadedStrings #-}
{-
test client/daemon interaction
-}
import Test.HUnit
import ProjectM36.Client
import ProjectM36.Daemon
import ProjectM36.Daemon.Config
import ProjectM36.Relation
import ProjectM36.Atom
import ProjectM36.TupleSet

import System.Exit
import Control.Concurrent

testList :: Connection -> Test
testList conn = TestList $ map (\t -> t conn) [testRelationalExpr,
                                               testDatabaseContextExpr,
                                               testGraphExpr,
                                               testTypeForRelationalExpr,
                                               testPlanForDatabaseContextExpr,
                                               testTransactionGraphAsRelation,
                                               testHeadTransactionUUID,
                                               testHeadName
                                               ]
           
main :: IO ()
main = do
  port <- launchTestServer
  eTestConn <- testConnection port
  case eTestConn of
    Left err -> putStrLn (show err) >> exitFailure
    Right testConn -> do
      tcounts <- runTestTT (testList testConn)
      if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testDatabaseName :: DatabaseName
testDatabaseName = "test"

testConnection :: Port -> IO (Either ConnectionError Connection)
testConnection port = do
  let connInfo = RemoteProcessConnectionInfo testDatabaseName (createNodeId "127.0.0.1" port)
  connectProjectM36 connInfo

-- | A version of 'launchDaemon' which returns the port on which the server is listening on a secondary thread
launchTestServer :: IO (Port)
launchTestServer = do
  let config = defaultDaemonConfig { databaseName = testDatabaseName }
  portVar <- newEmptyMVar
  _ <- forkIO $ launchServer config (Just portVar) >> pure ()
  takeMVar portVar
  
testRelationalExpr :: Connection -> Test  
testRelationalExpr conn = TestCase $ do
  relResult <- executeRelationalExpr conn (RelationVariable "true")
  assertEqual "invalid relation result" (Right relationTrue) relResult
  
testDatabaseContextExpr :: Connection -> Test
testDatabaseContextExpr conn = TestCase $ do 
  let attrs = attributesFromList [Attribute "x" intAtomType]
      testrv = "testrv"
  dbResult <- executeDatabaseContextExpr conn (Define testrv attrs)
  case dbResult of
    Just err -> assertFailure (show err)
    Nothing -> do
      eRel <- executeRelationalExpr conn (RelationVariable testrv)
      let expected = mkRelation attrs emptyTupleSet
      case eRel of
        Left err -> assertFailure (show err)
        Right rel -> assertEqual "dbcontext definition failed" expected (Right rel)
        
testGraphExpr :: Connection -> Test        
testGraphExpr conn = TestCase $ do
  graphResult <- executeGraphExpr conn (JumpToHead "master")
  case graphResult of
    Just err -> assertFailure (show err)
    Nothing -> pure ()
    
testTypeForRelationalExpr :: Connection -> Test
testTypeForRelationalExpr conn = TestCase $ do
  relResult <- typeForRelationalExpr conn (RelationVariable "true")
  case relResult of
    Left err -> assertFailure (show err)
    Right rel -> assertEqual "typeForRelationalExpr failure" relationFalse rel
    
testPlanForDatabaseContextExpr :: Connection -> Test    
testPlanForDatabaseContextExpr conn = TestCase $ do
  let attrs = attributesFromList [Attribute "x" intAtomType]
      testrv = "testrv"
      dbExpr = Define testrv attrs
  planResult <- planForDatabaseContextExpr conn dbExpr
  case planResult of
    Left err -> assertFailure (show err)
    Right plan -> assertEqual "planForDatabaseContextExpr failure" dbExpr plan
    
    
testTransactionGraphAsRelation :: Connection -> Test    
testTransactionGraphAsRelation conn = TestCase $ do
  eGraph <- transactionGraphAsRelation conn
  case eGraph of
    Left err -> assertFailure (show err)
    Right _ -> pure ()
    
testHeadTransactionUUID :: Connection -> Test    
testHeadTransactionUUID conn = TestCase $ do
  _ <- headTransactionUUID conn
  pure ()
  
testHeadName :: Connection -> Test
testHeadName conn = TestCase $ do
  mHeadName <- headName conn
  assertEqual "headName failure" (Just "master") mHeadName
  
  