import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.DatabaseContext.SelfTest
import ProjectM36.Attribute
import ProjectM36.Session
import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.Cache.RelationalExprCache
import ProjectM36.Cache.Tuple
import Test.HUnit
import System.Exit
import Control.Concurrent.STM
import Data.UUID.V4
import qualified Streamly.Data.Stream.Prelude as S
import qualified Data.Vector as V
import System.IO.Temp
import System.IO
import qualified Data.Text as T
import System.FilePath
import qualified Data.Map as M
import Data.Time.Clock
import qualified Data.Set as Set

testList :: Test
testList = TestList [
                     testTupleCacheRoundtripv000,
                     testExpensiveExpr,
                     testCacheEviction
                     ]

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess  


testConnection :: NotificationCallback -> IO (SessionId, Connection)
testConnection callback = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence callback [] selfTestDatabaseContext adminRoleName)
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
  cache <- case conn of
             RemoteConnection{} -> assertFailure "unexpected remote connection"
             InProcessConnection conf -> pure (ipRelExprCache conf)

  currentSize' <- readTVarIO (currentSize cache)
  assertEqual "cache size zero" 0 currentSize'
  
  Right headTransId <- headTransactionId session conn
  let expensiveExpr = MakeRelationFromExprs Nothing
                      (TupleExprs tmarker
                       [TupleExpr (M.fromList [("expensive",
                                                FunctionAtomExpr "test_expensive"
                                                  [NakedAtomExpr (TextAtom "test"),
                                                   NakedAtomExpr (IntegerAtom 1000000)] tmarker)
                                              ])
                       ])
      tmarker = TransactionIdLookup headTransId
      expensiveResult = mkRelationFromList (attributesFromList [Attribute "expensive" TextAtomType]) [[TextAtom "test"]]
  before <- getCurrentTime
  result1 <- executeTransGraphRelationalExpr session conn expensiveExpr
  
  assertEqual "first expensive run" expensiveResult result1
  after <- getCurrentTime  
  let firstDiff = diffUTCTime after before
--  print $ firstDiff
  assertBool "first expensive time" (firstDiff > 1.0)

  -- run the expensive expression again but we expect that it should be cached
  before' <- getCurrentTime
  result' <- executeTransGraphRelationalExpr session conn expensiveExpr
  
  assertEqual "second expensive run" expensiveResult result'
  after' <- getCurrentTime
  let secondDiff = diffUTCTime after' before'
--  print secondDiff
  assertBool ("second expensive time, actual: " <> show secondDiff) (secondDiff < 1.0)  

  currentSize'' <- readTVarIO (currentSize cache)
  assertEqual "primed cache size" 80 currentSize''

  putStrLn "ext_expensive"
  -- project on the expensive attribute to check that the composed expression can still be serviced by the cache
  let expensiveExpr2 = Project (AttributeNames (Set.singleton "ext_expensive")) $ Extend (AttributeExtendTupleExpr "ext_expensive" (FunctionAtomExpr "text_length" [AttributeAtomExpr "expensive"] tmarker)) expensiveExpr
      expensiveResult2 = mkRelationFromList (attributesFromList [Attribute "ext_expensive" IntegerAtomType]) [[IntegerAtom 4]]
  before'' <- getCurrentTime

  result'' <- executeTransGraphRelationalExpr session conn expensiveExpr2

  assertEqual "extended expensive (cached)" expensiveResult2 result''

  after'' <- getCurrentTime

  let expensive2Diff = diffUTCTime after'' before''
  assertBool ("stacked expensive time, actual: " <> show expensive2Diff) (expensive2Diff < 1.0)    

--test cache eviction when size boundaries are hit
testCacheEviction :: Test
testCacheEviction = TestCase $ do
  (session, conn) <- testConnection emptyNotificationCallback
  Right headTransId <- headTransactionId session conn  
  let maxCacheSize = 100
      -- change the attribute to create multiple cache entries
      tmarker = TransactionIdLookup headTransId      
      expensiveExpr attr = MakeRelationFromExprs Nothing
                      (TupleExprs tmarker
                       [TupleExpr (M.fromList [(attr,
                                                FunctionAtomExpr "test_expensive"
                                                  [NakedAtomExpr (TextAtom "test"),
                                                   NakedAtomExpr (IntegerAtom 1000000)] tmarker)
                                              ])
                       ])
  
  cache <- case conn of
             RemoteConnection{} -> assertFailure "unexpected remote connection"
             InProcessConnection conf -> pure (ipRelExprCache conf)
  atomically $ writeTVar (upperBound cache) maxCacheSize
  
  -- add something to the cache
  _result1 <- executeTransGraphRelationalExpr session conn (expensiveExpr "expensive1")
  
  -- check size
  currentSize' <- readTVarIO (currentSize cache)
  assertBool "expensive1 cache size" (currentSize' > 0)

  -- add an item to go over the max cache size
  _result2 <- executeTransGraphRelationalExpr session conn (expensiveExpr "expensive2")  

  -- check cache size to ensure that previous entry was evicted
  currentSize'' <- readTVarIO (currentSize cache)
  assertBool ("expensive2 cache size: " <> show currentSize'') (currentSize'' < maxCacheSize)  

  -- check that the correct entry was evicted
  before <- getCurrentTime
  _result2 <- executeTransGraphRelationalExpr session conn (expensiveExpr "expensive2")
  after <- getCurrentTime

  assertBool "expensive2 was not cached" (diffUTCTime after before < 1.0)
  
