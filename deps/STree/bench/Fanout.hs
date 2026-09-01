{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Where does widening a node stop paying?
--
-- A query costs @height x (per-level cost)@. Widening the node cuts the height,
-- since @height = ceil(log_(L+1) n)@, but makes every level do more work. Those
-- pull in opposite directions and the optimum is wherever they cross, so the
-- table below reports the height and the derived per-level cost next to the
-- total -- the totals alone say where the crossover is, but not why.
--
-- The answer is different for the two query paths, and the reason is worth
-- knowing. On the scalar path a level costs @L@ comparisons, so widening
-- directly buys work; the comparisons dominate and narrow always wins. On the
-- vectorised path a level costs a handful of vector instructions almost
-- regardless of @L@, so what is left is one dependent memory access per level --
-- and then /fewer levels/ is what matters, which reverses the conclusion.
--
-- Every configuration holds the same million keys, so the differences are the
-- shape of the tree and nothing else.
module Main (main) where

import Control.Monad (forM, forM_)
import Data.Int (Int32)
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

import Data.STree
import Data.STree.Batch

n, queryCount, reps :: Int
n = 1000000
queryCount = 100000
reps = 5

randoms :: Word64 -> [Word64]
randoms = drop 1 . iterate (\s -> s * 6364136223846793005 + 1442695040888963407)

keys :: U.Vector Int32
keys = U.fromList (sort (map fromIntegral (take n (randoms 42))))

queries :: U.Vector Int32
queries = U.fromList (map fromIntegral (take (queryCount + reps) (randoms 0xBEEF)))

-- | A rep-dependent window, so no repetition can reuse another's result. Timing
-- the same pure expression twice measures the second as free.
--
-- The query count is large for a reason: the clock here resolves to a
-- microsecond, and at 1000 queries a batch measurement was 12 us total, so every
-- figure came out an exact multiple of 1 ns with about 8% quantisation error.
window :: Int -> U.Vector Int32
window r = U.slice r queryCount queries

timeBest :: (Int -> IO a) -> IO Double
timeBest act = do
  ts <- forM [0 .. reps - 1] $ \r -> do
    t0 <- getMonotonicTime
    _ <- act r
    t1 <- getMonotonicTime
    return (t1 - t0)
  return (minimum ts / fromIntegral queryCount)

data Row = Row
  { rowL :: Int
  , rowHeight :: Int
  , rowBytes :: Int
  , rowScalar :: Double
  , rowBatch :: Double
  }

-- | Time both paths for one already-built tree. The folds are passed in already
-- specialised: taking a width-polymorphic tree here instead would leave a
-- dictionary in the inner loop and flatten the very differences being measured.
measure :: Int -> Int -> Int -> (Int -> IO Int) -> (Int -> IO Int) -> IO Row
measure l h bytes scalarAct batchAct = do
  sc <- timeBest scalarAct
  ba <- timeBest batchAct
  return (Row l h bytes sc ba)

main :: IO ()
main = do
  _ <- timeBest (\_ -> return $! U.length keys)

  rows <-
    sequence
      [ do
          let t = unsafeBuild keys :: BTree 2 Int32
          measure 2 (logSize @2 n) (sizeInBytes t) (sc t) (ba t)
      , do
          let t = unsafeBuild keys :: BTree 4 Int32
          measure 4 (logSize @4 n) (sizeInBytes t) (sc t) (ba t)
      , do
          let t = unsafeBuild keys :: BTree 8 Int32
          measure 8 (logSize @8 n) (sizeInBytes t) (sc t) (ba t)
      , do
          let t = unsafeBuild keys :: BTree 16 Int32
          measure 16 (logSize @16 n) (sizeInBytes t) (sc t) (ba t)
      , do
          let t = unsafeBuild keys :: BTree 32 Int32
          measure 32 (logSize @32 n) (sizeInBytes t) (sc t) (ba t)
      , do
          let t = unsafeBuild keys :: BTree 64 Int32
          measure 64 (logSize @64 n) (sizeInBytes t) (sc t) (ba t)
      ]

  printf "%d Int32 keys, %d queries per measurement, best of %d\n\n" n queryCount reps

  let bestScalar = minimum (map rowScalar rows)
      bestBatch = minimum (map rowBatch rows)

  printf "  %-3s %-7s %-7s %10s %10s %6s   %10s %10s %6s\n" "L" "fanout" "height" "scalar" "/level" "vs best" "batch" "/level" "vs best"
  forM_ rows $ \row ->
    printf
      "  %-3d %-7d %-7d %8.1f ns %8.2f %5.0f%%   %8.1f ns %8.2f %5.0f%%\n"
      (rowL row)
      (rowL row + 1)
      (rowHeight row)
      (rowScalar row * 1e9)
      (rowScalar row * 1e9 / fromIntegral (rowHeight row))
      (100 * rowScalar row / bestScalar)
      (rowBatch row * 1e9)
      (rowBatch row * 1e9 / fromIntegral (rowHeight row))
      (100 * rowBatch row / bestBatch)

  putStrLn ""
  printf "  optimum: scalar L=%d, batch L=%d\n" (pick rowScalar rows) (pick rowBatch rows)
  putStrLn ""

  -- Where the marginal step stops helping: the first width at which doubling L
  -- makes things worse rather than better.
  putStrLn "  marginal effect of doubling L (negative = widening still helps)"
  printf "  %-10s %14s %14s\n" "step" "scalar" "batch"
  forM_ (zip rows (drop 1 rows)) $ \(a, b) ->
    printf
      "  %-10s %13.1f%% %13.1f%%\n"
      (show (rowL a) ++ " -> " ++ show (rowL b))
      (100 * (rowScalar b / rowScalar a - 1))
      (100 * (rowBatch b / rowBatch a - 1))

  putStrLn ""
  -- Padding rounds the key array up to a whole number of nodes, so a wider node
  -- can cost up to L-1 extra keys. At this n it costs nothing at any width,
  -- which is worth stating: the choice below is pure time, not a space trade.
  printf
    "  memory: %d B to %d B across widths (padding differs by %d keys)\n"
    (minimum (map rowBytes rows))
    (maximum (map rowBytes rows))
    ((maximum (map rowBytes rows) - minimum (map rowBytes rows)) `div` 4)
  where
    sc t r = return $! U.foldl' (\ !acc k -> acc + lowerBoundIdx t k) 0 (window r)
    ba t r = return $! U.sum (lowerBoundIdxMany t (window r))
    pick f = rowL . foldr1 (\a b -> if f a <= f b then a else b)
