-- an in-memory cache for relational expression results keyed off of the expressions
module ProjectM36.Cache.RelationalExprCache where
import ProjectM36.Base
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Int
import qualified StmContainers.Map as STMMap
import Control.Concurrent.STM
import GHC.Conc (unsafeIOToSTM)
import System.Random
import Control.Monad
import qualified ProjectM36.RelExprSize as RE
import ProjectM36.SystemMemory
import ProjectM36.RelExprSize (ByteCount)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import ListT
import Data.List (sortBy)

--caching for uncommitted transactions may be a useful, future extension, but cannot be supported here since they are not (yet) uniquely identified

{- both the key and value of this cache are relational expressions, allowing for maximum flexibility

* if a key is a partial match of a large rel expr, then it can be used
* if a value is something other than a set of tuples, it can allow for a natural compression

the cache behaves probabalistically in that there is a non-zero chance for any cache item to be removed. As the cache increases in size, the chance of an item to be removed is increased.

Project:M36 passes all results to the cache, which decides if it is worth caching based on the time it took to calculate the result and how large the result is (if it won't blow out the cache maximum level).

-}


data RelExprCache = RelExprCache {
  upperBound :: TVar ByteCount,
  currentSize :: TVar ByteCount,
  cacheMap :: STMMap.Map PinnedRelationalExpr RelExprCacheInfo
  }

-- | Use all available RAM. In the future, some sort of memory heuristics engine could juggle how much memory is allocated to caching vs. processing.
defaultUpperBound :: IO ByteCount
defaultUpperBound = do
  mem <- getTotalMemory
  pure (fromMaybe 0 mem)

empty :: ByteCount -> IO RelExprCache
empty upper = do
  maxSize <- newTVarIO upper
  curSize <- newTVarIO 0
  newMap <- STMMap.newIO
  pure $ RelExprCache {
    upperBound = maxSize,
    currentSize = curSize,
    cacheMap = newMap
    }

-- | Relational results can be represented using multiple representations such as
-- * unsorted tupleset
-- * tuples sorted by some ordering
-- * pinned relational expression (which may have been partially evaluated and could refer to other potentially-cached expressions)
-- * b+tree with tuples
-- All representations are immutable and pegged to specific transactions.
-- These representations are used to cache evaluated relational expressions out of the transaction graph
data RelationRepresentation =
  PinnedExpressionRep PinnedRelationalExpr |
  UnsortedTupleSetRep Attributes RelationTupleSet |
  SortedTuplesRep [RelationTuple] (NE.NonEmpty (AttributeName, SortOrder))

instance RE.Size RelationRepresentation where
  size (PinnedExpressionRep pRelExpr) = RE.size pRelExpr
  size (UnsortedTupleSetRep _ tupSet) = RE.size tupSet
  size (SortedTuplesRep tups _) = RE.size tups

data SortOrder = AscSortOrder | DescSortOrder  
    
data RelExprCacheInfo =
  RelExprCacheInfo { calculatedInTime :: !NominalDiffTime, -- ^ the duration of time it took to compute the relational expression without this cache entry. This can be used to determine if using the cache is worthwhile.
                     result :: RelationRepresentation, -- ^ the cached relational expr (in memory)
                     createTime :: !UTCTime, -- ^  when this entry was added to the cache
                     lastRequestTime :: !(Maybe UTCTime), -- when this entry was last used
                     size :: !ByteCount
                   }

-- identify the least-recently-used entries whose size sum to the target size or more.
leastRecentlyUsedEntries :: ByteCount -> RelExprCache -> STM [PinnedRelationalExpr]
leastRecentlyUsedEntries targetSize cache = do
  cacheMapAssoc <- toList $ STMMap.listT (cacheMap cache)
  let lrusorted = sortBy lrusort cacheMapAssoc
      lrusort (_, cacheInfoA) (_,cacheInfoB) =
        lastRequestTime cacheInfoB `compare` lastRequestTime cacheInfoA -- lru should be at the front of the list
      keysToRemove = snd $ foldr sumToTargetSize (0,[]) lrusorted
      sumToTargetSize (k,cacheInfo) acc@(bytesAcc, keysAcc) = 
        if bytesAcc >= targetSize then
          acc
          else
          (size cacheInfo + bytesAcc, k : keysAcc)
  pure keysToRemove

-- | Delete the least-important cache items until the target size for this cache is reached.
purgeToSize :: RelExprCache -> ByteCount -> STM ()
purgeToSize = undefined

lookup :: PinnedRelationalExpr -> RelExprCache -> STM (Maybe RelExprCacheInfo)
lookup key cache = STMMap.lookup key (cacheMap cache)

type HitCount = Int64
type Probability = Double

-- | Decide probabalistically which cache entries to expunge depending on cache pressure. Any entry has a non-zero chance of being expunged.
--trimCache :: RelExprCache -> STM ()
--trimCache = do
  --attribute probability to all cache entries based on cache size, time to compute the entry, last request time

-- | A simple LRU-based cache where the upper-bound is the available memory.
executeLRUStrategy :: ByteCount -> -- ^ size of new, potential cache entry
                      NominalDiffTime -> -- ^ time it took to calculate this cache entry
                      RelExprCache ->
                      STM (Probability, [PinnedRelationalExpr]) -- ^ return the probability that the cache should retain this entry and, if so, which entries to purge to make room for it
executeLRUStrategy entrySize _calcTime cache = do
    upperBound' <- readTVar (upperBound cache)
    currentSize' <- readTVar (currentSize cache)
    if entrySize + currentSize' < upperBound' then
      pure (1.0, [])
      else do
      -- evict entries were least-recently used
      entriesToEvict <- leastRecentlyUsedEntries entrySize cache
      pure (1.0, entriesToEvict)

type IsRegisteredQuery = Bool

--allow the cache to decide if this result or one of it constituents should be cached
add :: RandomGen g 
    => g
    -> PinnedRelationalExpr
    -> RelationRepresentation
    -> NominalDiffTime -- ^ time it took to calculate this value
    -> IsRegisteredQuery -- ^ Used to determine if the result to cache may potentially be used to evaluate a registered query, which should increase the result's likelihood of being cached.
    -> RelExprCache
    -> STM g
add rgen expr exprResult calcTime _isRegisteredQuery cache = do
  -- if the time to calculate is less than a certain threshold, don't bother caching it
  now <- unsafeIOToSTM getCurrentTime  
  let newCacheInfo = RelExprCacheInfo { calculatedInTime = calcTime,
                                        result = exprResult,
                                        createTime = now,
                                        lastRequestTime = Nothing,
                                        size = RE.size exprResult + RE.size expr}
  mCacheInfo <- STMMap.lookup expr (cacheMap cache) --opt: replace with `focus`
  case mCacheInfo of
        Nothing -> do
          -- calculate new entry size
          let keySize = RE.size expr
              valSize = RE.size exprResult
          -- calculate probability of retention and, if retaining, which entries to evict
          (probRetain, entriesToEvict) <- executeLRUStrategy (keySize + valSize) calcTime cache
          let (rand, rgen') = uniformR (0.0, 1.0) rgen
          --traceShowM ("probRetain"::String, probRetain, "rand"::String, rand, probRetain >= rand)
          when (probRetain >= rand) $ do
            forM_ entriesToEvict $ \key -> do
              mval <- STMMap.lookup key (cacheMap cache)
              let delSize = case mval of
                    Nothing -> 0
                    Just val -> size val
              currentSize' <- readTVar (currentSize cache)                    
              STMMap.delete key (cacheMap cache)
              writeTVar (currentSize cache) (currentSize' - delSize)
            --traceShowM ("adding to cache"::String, expr)
            STMMap.insert newCacheInfo expr (cacheMap cache)
            currentSize'' <- readTVar (currentSize cache)             
            writeTVar (currentSize cache) (keySize + valSize + currentSize'')
          pure rgen'
        Just _ -> do -- then entry is already cached, nothing to do
          --traceShowM ("key already cached"::String)
          pure rgen


