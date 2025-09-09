import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.DatabaseContext.SelfTest
import ProjectM36.Attribute
import ProjectM36.Session
import ProjectM36.Client
import ProjectM36.Relation
--import ProjectM36.RelExprCache
import ProjectM36.Cache.Tuple
import Test.HUnit
import System.Exit
--import Control.Concurrent.STM
import Data.UUID.V4
import qualified Streamly.Data.Stream.Prelude as S
import qualified Data.Vector as V
import System.IO.Temp
import System.IO
import qualified Data.Text as T
import System.FilePath
import qualified Data.Map as M
import Data.Time.Clock

testList :: Test
testList = TestList [--testInMemoryCache,
                     testTupleCacheRoundtripv000,
                     testExpensiveExpr]

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess  


testConnection :: NotificationCallback -> IO (SessionId, Connection)
testConnection callback = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence callback [] selfTestDatabaseContext [superAdminRole])
  case dbconn of 
    Left err -> error (show err)
    Right conn -> do
      eSessionId <- createSessionAtHead conn "master"
      case eSessionId of
        Left err -> error (show err)
        Right sessionId -> do
          Right _ <- commit sessionId conn 
          pure (sessionId, conn)

{-
testCachePurgeProbability :: Test
testCachePurgeProbability = TestCase $ do
  

testInMemoryCache :: Test
testInMemoryCache = TestCase $ do
  cache <- empty
  --test adding an entry
-}

testTupleCacheRoundtripv000 :: Test
testTupleCacheRoundtripv000 = TestCase $ do
  newTid <- nextRandom
  let tuples = mkRelationTuples attrs (map (\n -> V.fromList [IntegerAtom n, TextAtom (T.pack (show n))]) numbers)
      numbers = [1 .. 500]
      attrs = attributesFromList [Attribute "a" IntegerAtomType, Attribute "b" TextAtomType]
      expr = RelationVariable "x" newTid
  
  withSystemTempDirectory "pm36tuplecache" $ \tmpdir ->
    withFile (tmpdir </> "pm36tuplecache") ReadWriteMode $ \h -> do
      writeTupleStream h expr 100 tuples
      hSeek h AbsoluteSeek 0
      rrTuples <- S.toList (readTupleStream h)
      assertEqual "round-trip tuple cache" tuples rrTuples
    
testExpensiveExpr :: Test
testExpensiveExpr = TestCase $ do
  -- run expensive query twice, the second time the result should be cached since the cache is large enough and nothing else should be in the cache.
  (session, conn) <- testConnection emptyNotificationCallback
  Right headTransId <- headTransactionId session conn
  let expensiveExpr = MakeRelationFromExprs Nothing (TupleExprs tmarker [TupleExpr (M.singleton "expensive" (FunctionAtomExpr "test_expensive" [NakedAtomExpr (TextAtom "test"),
                                                                                                                                                NakedAtomExpr (IntegerAtom 1000000)] tmarker))])
      tmarker = TransactionIdLookup headTransId
      expensiveResult = mkRelationFromList (attributesFromList [Attribute "expensive" TextAtomType]) [[TextAtom "test"]]
  before <- getCurrentTime
  result <- executeTransGraphRelationalExpr session conn expensiveExpr
  
  assertEqual "first expensive run" expensiveResult result
  after <- getCurrentTime
  print $ diffUTCTime after before
  assertBool "first expensive time" (diffUTCTime after before > 1.0)

  -- run the expensive expression again but we expect that it should be cached
  before' <- getCurrentTime
  result' <- executeTransGraphRelationalExpr session conn expensiveExpr
  
  assertEqual "second expensive run" expensiveResult result
  after' <- getCurrentTime
  print $ diffUTCTime after' before'
  assertBool "first expensive time" (diffUTCTime after before < 1.0)  
  
           
  
testStackedExpensiveQuery :: Test
testStackedExpensiveQuery = TestCase $ do
  -- run expensive query, then run expensive query + projection, result should be faster than first query execution
  undefined

                                                        
