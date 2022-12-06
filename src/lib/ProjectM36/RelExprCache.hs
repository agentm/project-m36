{-# LANGUAGE BangPatterns #-}
-- an in-memory cache for relational expression results keyed off of the expressions
module ProjectM36.RelExprCache where
import ProjectM36.Base
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Int
import qualified StmContainers.Map as M
import Control.Concurrent.STM
import GHC.Conc (unsafeIOToSTM)
import System.Random
import Control.Monad
import qualified ProjectM36.RelExprSize as RE
import ProjectM36.RelExprSize (ByteCount)

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
  cacheMap :: M.Map PinnedRelationalExpr RelExprCacheInfo
  }

empty :: ByteCount -> IO RelExprCache
empty upper = do
  maxSize <- newTVarIO upper
  curSize <- newTVarIO 0
  newMap <- M.newIO
  pure $ RelExprCache {
    upperBound = maxSize,
    currentSize = curSize,
    cacheMap = newMap
    }
    
data RelExprCacheInfo =
  RelExprCacheInfo { calculatedInTime :: !NominalDiffTime, -- ^ the duration of time it took to compute the relational expression without this cache entry. This can be used to determine if using the cache is worthwhile.
                     result :: PinnedRelationalExpr, -- ^ the cached relational expr (in memory)
                     createTime :: !UTCTime, -- ^ 
                     lastRequestTime :: !(Maybe UTCTime),
                     size :: !ByteCount
                   }

-- | Delete the least-important cache items until the target size for this cache is reached.
purgeToSize :: RelExprCache -> ByteCount -> STM ()
purgeToSize = undefined


type HitCount = Int64
type Probability = Double

-- | Decide probabalistically which cache entries to expunge depending on cache pressure. Any entry has a non-zero chance of being expunged.
trimCache :: RelExprCache -> STM ()
trimCache = undefined

  
probOfRetention :: ByteCount -> -- ^ size of cache entry
                  ByteCount -> -- ^ upper memory bound requested
                  NominalDiffTime -> -- ^ last request time for entry
                  HitCount -> -- ^ number of times this entry has been requested
                  NominalDiffTime -> -- ^ time it took to calculate this cache entry
                  Probability
probOfRetention entrySize upperBound' sinceLastReqTime hitCount calcTime =
    --instead of dealing with multivariate normal distributions, we average the distributions' probabilities- yes, this is dumb and arbitrary, but it's a start
  (nEntrySize + nSinceLastReqTime + nHitCount + nCalcTime) / 4.0
  where
      --normalize the input values    
      e = 2.71828182845904523536028747135266249775724709369995
      normalDist :: Double -> Double -> Double -> Double
      normalDist t x s = (t / sqrt (2 * pi)) * e ** (-0.5 * (x / s) ** 2)  -- map to normal distribution which could be ML-trained later
      nEntrySize = fromIntegral $ min entrySize upperBound' `div` upperBound' -- larger cache entries are more highly valued      
      nSinceLastReqTime = normalDist 2.5 (realToFrac sinceLastReqTime) 10
      nHitCount = 1 - normalDist 2.5 (realToFrac hitCount) 30
      nCalcTime = 1 - normalDist 2.5 (realToFrac calcTime) 10

--allow the cache to decide if this result or one of it constituents should be cached
add :: RandomGen g 
    => g
    -> PinnedRelationalExpr
    -> PinnedRelationalExpr
    -> NominalDiffTime -- ^ time it took to calculate this value
    -> RelExprCache
    -> STM g
add rgen expr exprResult calcTime cache = do
  -- if the time to calculate is less than a certain threshold, don't bother caching it
  now <- unsafeIOToSTM getCurrentTime  
  let newCacheInfo = RelExprCacheInfo { calculatedInTime = calcTime,
                                        result = exprResult,
                                        createTime = now,
                                        lastRequestTime = Nothing,
                                        size = RE.size exprResult + RE.size expr}
  mCacheInfo <- M.lookup expr (cacheMap cache) --opt: replace with `focus`
  case mCacheInfo of
        Nothing -> do
          -- calculate new entry size
          let keySize = RE.size expr
              valSize = RE.size exprResult
          upperBound' <- readTVar (upperBound cache)
          -- calculate probability of immediate ejection
          let probRetain = probOfRetention (keySize + valSize) upperBound' 0 0 calcTime
              (rand, rgen') = uniformR (0.0, 1.0) rgen
          when (probRetain >= rand) $ M.insert newCacheInfo expr (cacheMap cache)
          pure rgen'
        Just _ -> -- then entry is already cached, nothing to do
          pure rgen


