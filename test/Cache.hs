import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.Attribute
--import ProjectM36.RelExprCache
import ProjectM36.Cache.Tuple
import Test.HUnit
import System.Exit
--import Control.Concurrent.STM
import Data.UUID.V4
import qualified Streamly.Prelude as S
import qualified Data.Vector as V
import System.IO.Temp
import System.IO
import qualified Data.Text as T
import System.FilePath

testList :: Test
testList = TestList [--testInMemoryCache,
                     testTupleCacheRoundtripv000]

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess  

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
      numbers = [1 .. 5]
      attrs = attributesFromList [Attribute "a" IntegerAtomType, Attribute "b" TextAtomType]
      expr = RelationVariable "x" newTid
  
  withSystemTempDirectory "pm36tuplecache" $ \tmpdir ->
    withFile (tmpdir </> "pm36tuplecache") ReadWriteMode $ \h -> do
      writeTupleStream h expr 1 tuples
      hSeek h AbsoluteSeek 0
      rrTuples <- S.toList (readTupleStream h)
      assertEqual "round-trip tuple cache" tuples rrTuples
    
  
