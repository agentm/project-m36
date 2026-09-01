{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | How bulk lookups scale across cores, and how big a slice each worker should
-- take.
--
-- Lookups are read-only against an immutable tree, so there is nothing to
-- synchronise and the work is embarrassingly parallel. That makes the
-- interesting question not /whether/ it parallelises but /what stops it/, and
-- there are three candidates worth telling apart:
--
--   * __memory__: half of a descent is waiting on cache (measured), and the
--     deep levels are random accesses, so several cores pull disjoint lines
--     and contend for L2 and the shared last level;
--   * __the runtime__: the vectorised path makes @unsafe@ foreign calls, which
--     hold a capability, and allocates a result vector per batch, which feeds
--     the collector -- neither of which the scalar path does;
--   * __the cores__: this is a heterogeneous machine. Performance and
--     efficiency cores differ by roughly 3x, so equal slices finish at
--     unequal times.
--
-- Running the scalar and vectorised paths side by side separates the first two:
-- the scalar path allocates nothing and makes no foreign calls, so if it scales
-- and the batch does not, the runtime is the limit rather than memory.
--
-- == Work slices
--
-- Static equal division is just the special case of a shared queue whose chunk
-- is @total \/ workers@, so one sweep covers both: large chunks approach static
-- slicing, small chunks approach fully dynamic scheduling. What decides the
-- best size is the tension between two costs -- an atomic fetch and a foreign
-- call per chunk, against the tail left by whichever worker finishes last. The
-- @spread@ column is that tail, measured, and is what static slicing gets wrong
-- on a machine whose cores are not identical.
module Main (main) where

import Control.Concurrent (forkOn, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.Int (Int32)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumCapabilities)
import Text.Printf (printf)

import Data.STree
import Data.STree.Batch

treeSize, totalQueries :: Int
treeSize = 4000000
totalQueries = 2000000

randoms :: Word64 -> [Word64]
randoms = drop 1 . iterate (\s -> s * 6364136223846793005 + 1442695040888963407)

keysOf :: Int -> U.Vector Int32
keysOf n = U.fromList (sort (map fromIntegral (take n (randoms 42))))

queriesOf :: Int -> U.Vector Int32
queriesOf n = U.fromList (map fromIntegral (take n (randoms 0xBEEF)))

-- | A shared queue of half-open ranges. Static equal slicing is this with
-- @chunk = total \/ workers@; anything smaller is dynamic scheduling.
mkQueue :: Int -> Int -> IO (IO (Maybe (Int, Int)))
mkQueue total chunk = do
  cursor <- newIORef 0
  return $
    atomicModifyIORef' cursor $ \i ->
      if i >= total
        then (i, Nothing)
        else (i + chunk, Just (i, min chunk (total - i)))

-- | Run @k@ workers, each pinned to its own capability, until the queue is
-- empty. Returns the wall time and each worker's own elapsed time -- the spread
-- between those is the load imbalance.
parallelRun :: Int -> IO (Maybe (Int, Int)) -> ((Int, Int) -> IO Int) -> IO (Double, [Double])
parallelRun k next work = do
  dones <- replicateM k newEmptyMVar
  t0 <- getMonotonicTime
  forM_ (zip [0 ..] dones) $ \(i, done) ->
    forkOn i $ do
      s0 <- getMonotonicTime
      let loop !acc = do
            mc <- next
            case mc of
              Nothing -> return acc
              Just c -> work c >>= \r -> loop (acc + r)
      !acc <- loop 0
      s1 <- getMonotonicTime
      putMVar done (s1 - s0, acc)
  results <- mapM takeMVar dones
  t1 <- getMonotonicTime
  return (t1 - t0, map fst results)

-- | The two query paths, as a chunk of work.
batchWork :: BTree 16 Int32 -> U.Vector Int32 -> (Int, Int) -> IO Int
batchWork t qs (off, len) = return $! U.sum (lowerBoundIdxMany t (U.slice off len qs))

scalarWork :: BTree 16 Int32 -> U.Vector Int32 -> (Int, Int) -> IO Int
scalarWork t qs (off, len) =
  return $! U.foldl' (\ !acc k -> acc + lowerBoundIdx t k) 0 (U.slice off len qs)

-- | Best of three, since a parallel wall time can only be inflated by
-- interference, never deflated by it.
measure :: Int -> Int -> ((Int, Int) -> IO Int) -> IO (Double, Double)
measure workers chunk work = do
  runs <- forM [1 .. 3 :: Int] $ \_ -> do
    next <- mkQueue totalQueries chunk
    parallelRun workers next work
  let (wall, spreads) = minimum [(w, maximum ts - minimum ts) | (w, ts) <- runs]
  return (wall, spreads)

main :: IO ()
main = do
  caps <- getNumCapabilities
  let keys = keysOf treeSize
      qs = queriesOf totalQueries
      t = unsafeBuild keys :: BTree 16 Int32
  _ <- pure $! U.sum keys
  _ <- pure $! size t

  printf
    "tree: %d Int32 keys, L=16   queries: %d   capabilities: %d\n\n"
    treeSize
    totalQueries
    caps

  let threadCounts = [w | w <- [1, 2, 4, 6, 8, 10, 12], w <= max 1 caps]

  forM_ [("vectorised batch", batchWork t qs), ("scalar", scalarWork t qs)] $ \(nm, work) -> do
    printf "%s\n" (nm :: String)
    printf "  threads     chunk    wall ms    Mq/s   speedup   spread ms\n"
    -- One worker taking the whole range in one slice is the serial reference.
    (base, _) <- measure 1 totalQueries work
    forM_ threadCounts $ \w -> do
      let static = max 1 (totalQueries `div` w)
          chunks = dedup [static, 65536, 8192, 1024, 256]
      forM_ chunks $ \c -> do
        (wall, spread) <- measure w c work
        printf
          "  %7d %9d %10.1f %7.1f %8.2fx %9.2f%s\n"
          w
          c
          (wall * 1000)
          (fromIntegral totalQueries / wall / 1e6)
          (base / wall)
          (spread * 1000)
          (if c == static then "   <- static" else "")
    putStrLn ""
  where
    dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
