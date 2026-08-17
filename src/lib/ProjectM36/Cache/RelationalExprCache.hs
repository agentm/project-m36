-- an in-memory cache for relational expression results keyed off of the expressions
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module ProjectM36.Cache.RelationalExprCache where
import ProjectM36.Base
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Int
import qualified StmContainers.Multimap as STMMMap
import qualified StmContainers.Set as STMSet
import Control.Concurrent.STM
import GHC.Conc (unsafeIOToSTM)
import System.Random
import Control.Monad
import qualified ProjectM36.RelExprSize as RE
import ProjectM36.SystemMemory
import ProjectM36.RelExprSize (ByteCount)
import qualified Data.List.NonEmpty as NE
import ListT
import Data.List (sortBy)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Ord (comparing)

--caching for uncommitted transactions may be a useful, future extension, but cannot be supported here since they are not (yet) uniquely identified

{- both the key and value of this cache are relational expressions, allowing for maximum flexibility

* if a key is a partial match of a large rel expr, then it can be used
* if a value is something other than a set of tuples, it can allow for a natural compression

Project:M36 passes all results to the cache, which decides if it is worth caching based on the time it took to calculate the result and how large the result is (if it won't blow out the cache maximum level).

In the future, the cache can be populated by predicting which queries are likely to be issued.
-}

-- limitation: ideally the value in the Map would be a set of RelExprCacheInfo so that we can serve queries of different priorities with different representations
type RelExprCacheMap = STMMMap.Multimap PinnedRelationalExpr RelExprCacheInfo

data RelExprCache = RelExprCache {
  upperBound :: TVar ByteCount,
  currentSize :: TVar ByteCount,
  cacheMap :: RelExprCacheMap
  }

-- | Use all available RAM. In the future, some sort of memory heuristics engine could juggle how much memory is allocated to caching vs. processing.
defaultUpperBound :: IO ByteCount
defaultUpperBound = do
  mem <- getMemoryStats
  case mem of
    Left _err -> pure 0
    Right (_,totalMem) -> pure totalMem

empty :: ByteCount -> IO RelExprCache
empty upper = do
  maxSize <- newTVarIO upper
  curSize <- newTVarIO 0
  newMap <- STMMMap.newIO
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
  deriving (Eq, Generic, Hashable)

instance RE.Size RelationRepresentation where
  size (PinnedExpressionRep pRelExpr) = RE.size pRelExpr
  size (UnsortedTupleSetRep _ tupSet) = RE.size tupSet
  size (SortedTuplesRep tups _) = RE.size tups

data SortOrder = AscSortOrder | DescSortOrder
  deriving (Eq, Generic, Hashable)
    
data RelExprCacheInfo =
  RelExprCacheInfo { calculatedInTime :: !NominalDiffTime, -- ^ the duration of time it took to compute the relational expression without this cache entry. This can be used to determine if using the cache is worthwhile.
                     result :: RelationRepresentation, -- ^ the cached relational expr (in memory)
                     createTime :: !UTCTime, -- ^  when this entry was added to the cache
                     lastRequestTime :: !(Maybe UTCTime), -- when this entry was last used
                     size :: !ByteCount
                   }
  deriving (Eq, Generic)

instance Hashable RelExprCacheInfo

type CachePath = (PinnedRelationalExpr, RelExprCacheInfo)

-- helper function for lru cache ejection but would be obsoleted by tracking lastRequestTime differently
relExprCacheInfosSortedByLastRequestTime :: RelExprCache -> STM [CachePath]
relExprCacheInfosSortedByLastRequestTime cache = do
  cacheMapAssoc <- toList $ STMMMap.listT (cacheMap cache)
  let lrusorted = sortBy lrusort cacheMapAssoc
      lrusort (_, cacheInfoA) (_, cacheInfoB) =
        lastRequestTime cacheInfoB `compare` lastRequestTime cacheInfoA -- lru should be at the front of the list
  pure lrusorted
        
-- identify the least-recently-used entries whose size sum to the target size or more.
leastRecentlyUsedEntries :: ByteCount -> RelExprCache -> STM [CachePath]
leastRecentlyUsedEntries targetSize cache = do
  lrusorted <- relExprCacheInfosSortedByLastRequestTime cache
  let pathsToRemove = snd $ foldr sumToTargetSize (0,[]) lrusorted
      sumToTargetSize path@(_key,cacheInfo) acc@(bytesAcc, pathsAcc) = 
        if bytesAcc >= targetSize then
          acc
          else
          (size cacheInfo + bytesAcc, path : pathsAcc)
  pure pathsToRemove

-- | Delete the least-important cache items until the target size for this cache is reached.
purgeToSize :: RelExprCache -> ByteCount -> STM ()
purgeToSize = undefined

-- | Find all RelExprCacheInfos for the expression key.
lookup :: PinnedRelationalExpr -> RelExprCache -> STM (Maybe (STMSet.Set RelExprCacheInfo))
lookup key cache = 
  STMMMap.lookupByKey key (cacheMap cache)

-- | Return the cache entry which returns the fastest result.
lookupFastestEntry :: PinnedRelationalExpr -> RelExprCache -> STM (Maybe RelExprCacheInfo)
lookupFastestEntry key cache = do
  mCacheOptions <- STMMMap.lookupByKey key (cacheMap cache)
  case mCacheOptions of
    Nothing -> pure  Nothing
    Just cacheOptions -> do
      options <- toList $ STMSet.listT cacheOptions
      let fastestOption =
            comparing (repSpeed . result)
          repSpeed :: RelationRepresentation -> Int
          repSpeed rep =
            case rep of
              SortedTuplesRep{} -> 1
              UnsortedTupleSetRep{} -> 2
              PinnedExpressionRep{} -> 3
          sortedFastest = sortBy fastestOption options
      case sortedFastest of
        [] -> pure Nothing
        (fastest:_) -> pure (Just fastest)

type HitCount = Int64
type Probability = Double

-- | Decide probabalistically which cache entries to expunge depending on cache pressure. Any entry has a non-zero chance of being expunged.
--trimCache :: RelExprCache -> STM ()
--trimCache = do
  --attribute probability to all cache entries based on cache size, time to compute the entry, last request time

type MemoryPressure = Double

-- | A simple LRU-based cache where the upper-bound is the available memory.
executeLRUStrategy :: ByteCount -> -- ^ size of new, potential cache entry
                      NominalDiffTime -> -- ^ time it took to calculate this cache entry
                      RelExprCache ->
                      FreeMemBytes ->
                      STM (Probability, [CachePath]) -- ^ return the probability that the cache should retain this entry and, if so, which entries to purge to make room for it
executeLRUStrategy entrySize _calcTime cache freeMem = do
    upperBound' <- readTVar (upperBound cache)
    currentSize' <- readTVar (currentSize cache)
    let proposedFreeMem = freeMem - entrySize
        prob = normalizedLogProb 1.0 (fromIntegral proposedFreeMem) (fromIntegral upperBound')
        --prob = logisticProb (fromIntegral proposedFreeMem) (fromIntegral upperBound')
--    traceShowM ("cache prob"::String, proposedFreeMem, upperBound', prob)
    if entrySize + currentSize' < upperBound' then
      pure (prob, []) -- should we probabilistically remove cache entries before the cache is full?
      else do
      -- evict entries were least-recently used
      entriesToEvict <- leastRecentlyUsedEntries entrySize cache
      pure (prob, entriesToEvict)

type IsRegisteredQuery = Bool

--allow the cache to decide if this result or one of it constituents should be cached
add :: RandomGen g 
    => g
    -> PinnedRelationalExpr
    -> RelationRepresentation
    -> NominalDiffTime -- ^ time it took to calculate this value
    -> IsRegisteredQuery -- ^ Used to determine if the result to cache may potentially be used to evaluate a registered query, which should increase the result's likelihood of being cached.
    -> MemoryStats
    -> RelExprCache
    -> STM g
add rgen expr exprResult calcTime _isRegisteredQuery memStats cache = do
  -- if the time to calculate is less than a certain threshold, don't bother caching it
  now <- unsafeIOToSTM getCurrentTime
  let newCacheInfo = RelExprCacheInfo { calculatedInTime = calcTime,
                                        result = exprResult,
                                        createTime = now,
                                        lastRequestTime = Nothing,
                                        size = RE.size exprResult + RE.size expr}
  mCacheInfo <- STMMMap.lookupByKey expr (cacheMap cache) --opt: replace with `focus`
  case mCacheInfo of
        Nothing -> do
          -- calculate new entry size
          let keySize = RE.size expr
              valSize = RE.size exprResult
          -- calculate probability of retention and, if retaining, which entries to evict
          upperBound' <- readTVar (upperBound cache)          
          (probRetain, entriesToEvict) <- executeLRUStrategy (keySize + valSize) calcTime cache (min (fst memStats) upperBound')
          let (rand, rgen') = uniformR (0.0, 1.0) rgen
          --traceShowM ("probRetain"::String, probRetain, "rand"::String, rand, probRetain >= rand)
          when (probRetain >= rand) $ do
            forM_ entriesToEvict $ \(keyExpr, targetInfo) -> do
              let delSize = size targetInfo
              STMMMap.delete targetInfo keyExpr (cacheMap cache)
              currentSize' <- readTVar (currentSize cache)
              writeTVar (currentSize cache) (currentSize' - delSize)
            --traceShowM ("adding to cache"::String, expr)
            STMMMap.insert newCacheInfo expr (cacheMap cache)
            currentSize'' <- readTVar (currentSize cache)             
            writeTVar (currentSize cache) (keySize + valSize + currentSize'')
          pure rgen'
        Just _ -> do -- then entry is already cached, nothing to do
          --traceShowM ("key already cached"::String)
          pure rgen

-- p(m) = log(1 + α*m) / log(1 + α*Mmax)
normalizedLogProb :: Double -> Double -> Double -> Double
normalizedLogProb alpha m mmax
  | m <= 0    = 0
  | m >= mmax = 1
  | otherwise = logBase (1 + alpha * mmax) (1 + alpha * m) 

logistic :: Double -> Double -> Double -> Double
logistic k m x0 =
  1.0 / (1.0 + (euler ** ((-k) * (m - x0))))

stdlogistic :: Double -> Double
stdlogistic m =
  logistic 1.0 m 0.0

logisticProb :: Double -> Double -> Double
logisticProb freeMem memMax = logistic 1.0 freeMem (memMax / 2.0)

euler :: Double
euler = 2.718281828459045        
