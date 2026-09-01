{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | End to end: a million random numbers, then look some up. What does the tree
-- actually cost, and when does it pay for itself?
--
-- Two asymmetries decide this, and both favour the scan more than is obvious:
--
--   * __A scan needs no order.__ The tree does, so its setup honestly includes
--     sorting the input, which the scan path skips entirely. Sorting is also by
--     far the larger half of that setup.
--   * __A miss costs a full scan.__ These are membership queries, and a random
--     32-bit value is almost never among a million keys, so the scan walks the
--     whole array rather than half of it. Queries drawn from the data are
--     measured separately, where it averages half.
--
-- Sorting also buys binary search for free, so that is measured too: the useful
-- question is not only "is the tree worth building" but "given that the data had
-- to be sorted anyway, is the tree worth building /on top of/ that".
module Main (main) where

import Control.Monad (forM, forM_)
import Data.Int (Int32)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

import Data.STree
import Data.STree.Batch

n :: Int
n = 1000000

-- | Enough queries that the per-query figure is stable, but the scan is five
-- orders of magnitude slower than the tree, so it gets its own count.
scanQueries, treeQueries :: Int
scanQueries = 200
treeQueries = 100000

randoms :: Word64 -> [Word64]
randoms = drop 1 . iterate (\s -> s * 6364136223846793005 + 1442695040888963407)

unsortedKeys :: U.Vector Int32
unsortedKeys = U.fromList (map fromIntegral (take n (randoms 42)))

-- | Absent queries: fresh random values, which a million keys out of four
-- billion almost never contain.
absentQueries :: Int -> U.Vector Int32
absentQueries k = U.fromList (map fromIntegral (take (k + reps) (randoms 0xBEEF)))

-- | Present queries: drawn from the data itself.
presentQueries :: Int -> U.Vector Int32
presentQueries k =
  U.fromList
    [unsortedKeys U.! fromIntegral (w `rem` fromIntegral n) | w <- take (k + reps) (randoms 99)]

reps :: Int
reps = 3

-- ---------------------------------------------------------------------------
-- The four paths, all answering the same question: is this key present?
-- ---------------------------------------------------------------------------

-- | Naive sequential search over the /unsorted/ vector. No setup at all.
scanMember :: U.Vector Int32 -> Int32 -> Bool
scanMember v x = go 0
  where
    m = U.length v
    go !i
      | i >= m = False
      | U.unsafeIndex v i == x = True
      | otherwise = go (i + 1)

-- | Binary search over the sorted vector. Setup: the sort.
binMember :: U.Vector Int32 -> Int32 -> Bool
binMember v x = i < U.length v && U.unsafeIndex v i == x
  where
    i = go 0 (U.length v)
    go !lo !hi
      | lo >= hi = lo
      | U.unsafeIndex v mid < x = go (mid + 1) hi
      | otherwise = go lo mid
      where
        mid = (lo + hi) `quot` 2

-- | The tree gives the position; the sorted vector confirms the key. Keeping the
-- vector is what makes the confirmation O(1) -- 'index' would be another descent.
treeMember :: BTree 16 Int32 -> U.Vector Int32 -> Int32 -> Bool
treeMember t v x = i < U.length v && U.unsafeIndex v i == x
  where
    i = lowerBoundIdx t x

-- | Batched: one vectorised descent for the whole query set, then confirm.
treeMemberMany :: BTree 16 Int32 -> U.Vector Int32 -> U.Vector Int32 -> Int
treeMemberMany t v qs =
  U.length (U.filter id (U.imap (\j i -> i < U.length v && U.unsafeIndex v i == qs U.! j) idxs))
  where
    idxs = lowerBoundIdxMany t qs

-- ---------------------------------------------------------------------------

-- | Best of @reps@; a wall time can only be inflated by interference.
--
-- The action takes the repetition number and /must/ use it, so that each
-- repetition is a different expression. Timing the same pure expression twice
-- measures the second one as free -- it was already evaluated -- and taking the
-- minimum then reports zero. That is not hypothetical; the first version of this
-- file did exactly that and reported every query as 0.0 ns.
timeBest :: Int -> (Int -> IO a) -> IO Double
timeBest nreps act = do
  ts <- forM [0 .. nreps - 1] $ \r -> do
    t0 <- getMonotonicTime
    _ <- act r
    t1 <- getMonotonicTime
    return (t1 - t0)
  return (minimum ts)

sortVec :: U.Vector Int32 -> U.Vector Int32
sortVec v = U.modify Intro.sort v

sorted :: U.Vector Int32
sorted = sortVec unsortedKeys

btree :: BTree 16 Int32
btree = unsafeBuild sorted

countTrue :: (Int32 -> Bool) -> U.Vector Int32 -> Int
countTrue f = U.foldl' (\ !acc x -> if f x then acc + 1 else acc) 0

main :: IO ()
main = do
  printf "%d random Int32 keys\n\n" n
  _ <- timeBest 1 (\_ -> return $! U.sum unsortedKeys)

  -- Setup. Each repetition sorts (and builds from) a slightly different slice,
  -- so no repetition can reuse another's result. One element fewer is the same
  -- work to within a millionth.
  tSort <- timeBest reps (\r -> return $! U.sum (sortVec (U.drop r unsortedKeys)))
  tBuild <- timeBest reps (\r -> return $! size (unsafeBuild (U.drop r sorted) :: BTree 16 Int32))
  _ <- timeBest 1 (\_ -> return $! size btree)

  putStrLn "setup"
  printf "  sort (introsort, in place)          %9.2f ms\n" (tSort * 1000)
  printf "  build BTree 16 from the sorted data %9.2f ms\n" (tBuild * 1000)
  printf "  %-36s%9.2f ms\n" "total, tree path" ((tSort + tBuild) * 1000)
  printf "  %-36s%9.2f ms   (a scan needs no order)\n\n" "total, scan path" (0.0 :: Double)

  forM_ [("absent (random values)", absentQueries), ("present (drawn from the data)", presentQueries)] $
    \(label, mkQs) -> do
      printf "per-query cost, queries %s\n" (label :: String)
      let sq = mkQs scanQueries
          tq = mkQs treeQueries

      -- Same trick: a rep-dependent window over the query set.
      let win k v r = U.slice r k v
      tScan <- timeBest reps (\r -> return $! countTrue (scanMember unsortedKeys) (win scanQueries sq r))
      tBin <- timeBest reps (\r -> return $! countTrue (binMember sorted) (win treeQueries tq r))
      tTree <- timeBest reps (\r -> return $! countTrue (treeMember btree sorted) (win treeQueries tq r))
      tBatch <- timeBest reps (\r -> return $! treeMemberMany btree sorted (win treeQueries tq r))

      let perScan = tScan / fromIntegral scanQueries
          perBin = tBin / fromIntegral treeQueries
          perTree = tTree / fromIntegral treeQueries
          perBatch = tBatch / fromIntegral treeQueries
          setupTree = tSort + tBuild
          -- k queries pay for the setup when setup + k*fast < k*scan
          breakEven s fast
            | perScan <= fast = Nothing
            | otherwise = Just (ceiling (s / (perScan - fast)) :: Int)
          row nm per setup =
            printf
              "  %-34s %11s   %s\n"
              (nm :: String)
              (showTime per)
              ( case breakEven setup per of
                  _ | setup == 0 -> "-"
                  Just k -> "pays for its setup after " ++ show k ++ " queries"
                  Nothing -> "never pays off"
              )

      row "sequential scan, unsorted" perScan 0
      row "binary search, sorted" perBin tSort
      row "s-tree lowerBoundIdx" perTree setupTree
      row "s-tree lowerBoundIdxMany (batch)" perBatch setupTree
      printf "  %-34s %11s\n" "scan / batch, per query" (printf "%.0fx" (perScan / perBatch) :: String)
      -- Splitting the setup is the useful part: almost all of it is the sort,
      -- which binary search needs too, so very little of the break-even is the
      -- tree's own doing.
      printf
        "  of the %d-query break-even: %d for the sort, %d more for the tree\n\n"
        (ceiling (setupTree / (perScan - perBatch)) :: Int)
        (ceiling (tSort / (perScan - perBatch)) :: Int)
        (ceiling (tBuild / (perScan - perBatch)) :: Int)

showTime :: Double -> String
showTime s
  | s >= 1e-3 = printf "%.2f ms" (s * 1e3)
  | s >= 1e-6 = printf "%.2f us" (s * 1e6)
  | otherwise = printf "%.1f ns" (s * 1e9)
