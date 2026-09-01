{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The B+tree's two node widths, swept at a million keys — the companion to
-- @Fanout.hs@, which does the same for the B-tree's single width.
--
-- This is a presentation layer over 'calibrateBPlusScalar' rather than its own grid.
-- The library already has to instantiate all thirty-six combinations to do its
-- job, so duplicating them here would only create a second thing to drift; the
-- benchmark adds the derived columns the library has no business computing —
-- per-level cost, index size, marginal effect per axis — and runs the B-tree
-- calibrations alongside so the cross-structure comparison comes from one
-- process instead of two.
--
-- Why the index size is worth a column: the leaf width sets how many separators
-- exist, so it decides how large the index has to be — @n \/ ll@ keys. Two
-- configurations can be equally fast and differ fourfold in what they store,
-- which is exactly what 'calibrateBPlusScalar' breaks ties on.
module Main (main) where

import Data.Int (Int32)
import Data.List (sort, sortOn)
import qualified Data.Vector.Unboxed as U
import Data.Proxy (Proxy)
import Data.Word (Word64)
import Text.Printf (printf)

import Data.STree.BTree (layoutLength)
import Data.STree.Calibrate

n, queryCount :: Int
n = 1000000
queryCount = 100000

widths :: [Int]
widths = [2, 4, 8, 16, 32, 64]

randoms :: Word64 -> [Word64]
randoms = drop 1 . iterate (\s -> s * 6364136223846793005 + 1442695040888963407)

keys :: U.Vector Int32
keys = U.fromList (sort (map fromIntegral (take n (randoms 42))))

values :: U.Vector Int32
values = U.enumFromN 0 n

queries :: U.Vector Int32
queries = U.fromList (map fromIntegral (take queryCount (randoms 0xBEEF)))

-- | Keys the index has to hold at a given leaf width, padded to whole nodes.
-- Pure arithmetic: the separator count is one per leaf boundary, so no tree has
-- to be built to know how big the index will be.
indexKeys :: Int -> Int -> Int
indexKeys li ll = withWidth li (\(_ :: Proxy i) -> layoutLength @i seps)
  where
    leaves = max 1 ((n + ll - 1) `div` ll)
    seps = max 0 (leaves - 1)

main :: IO ()
main = do
  printf
    "%d Int32 keys, %d queries per measurement\n\n"
    n
    queryCount

  bp <- calibrateBPlusScalar defaultConfig keys values queries
  bulk <- calibrateBPlusBulk defaultConfig keys values queries
  scalar <- calibrateBTreeScalar defaultConfig keys queries
  batch <- calibrateBTreeBatch defaultConfig keys queries

  let cell li ll =
        case [m | m <- calMeasurements bp, mWidths m == (li, ll)] of
          (m : _) -> Just m
          [] -> Nothing
      ns li ll = maybe (0 / 0) mNanosPerQuery (cell li ll)
      ht li ll = maybe 0 mHeight (cell li ll)
      best = minimum (map mNanosPerQuery (calMeasurements bp))

  let bulkCell li ll =
        case [m | m <- calMeasurements bulk, mWidths m == (li, ll)] of
          (m : _) -> mNanosPerQuery m
          [] -> 0 / 0
      bulkBest = minimum (map mNanosPerQuery (calMeasurements bulk))

  putStrLn "ns per query, one at a time (BP.lowerBoundIdx)"
  header
  mapM_ (\li -> row li (\ll -> printf "%7.1f" (ns li ll))) widths

  putStrLn "\nns per query, bulk (BP.lowerBoundIdxMany: vectorised index, bisected leaf)"
  header
  mapM_ (\li -> row li (\ll -> printf "%7.1f" (bulkCell li ll))) widths

  putStrLn "\nlevels"
  header
  mapM_ (\li -> row li (\ll -> printf "%7d" (ht li ll))) widths

  putStrLn "\nns per level"
  header
  mapM_ (\li -> row li (\ll -> printf "%7.2f" (ns li ll / fromIntegral (max 1 (ht li ll))))) widths

  putStrLn "\nindex keys to store (independent of li except for padding)"
  printf "        "
  mapM_ (\ll -> printf "%9d" ll) widths
  putStrLn ""
  printf "        "
  mapM_ (\ll -> printf "%9d" (indexKeys 4 ll)) widths
  putStrLn ""

  printf "\nfastest bulk configurations\n"
  mapM_
    ( \m ->
        let (li, ll) = mWidths m
         in printf
              "  li=%-3d ll=%-3d %7.1f ns  %4.0f%% of best  index %8d keys\n"
              li
              ll
              (mNanosPerQuery m)
              (100 * mNanosPerQuery m / bulkBest)
              (indexKeys li ll)
    )
    (take 5 (sortOn mNanosPerQuery (calMeasurements bulk)))
  printf
    "  calibrateBPlusBulk chose li=%d ll=%d\n"
    (fst (calBest bulk))
    (snd (calBest bulk))

  printf "\nfastest one-at-a-time configurations\n"
  mapM_
    ( \m ->
        let (li, ll) = mWidths m
         in printf
              "  li=%-3d ll=%-3d %7.1f ns  %4.0f%% of best  index %8d keys\n"
              li
              ll
              (mNanosPerQuery m)
              (100 * mNanosPerQuery m / best)
              (indexKeys li ll)
    )
    (take 5 (sortOn mNanosPerQuery (calMeasurements bp)))

  printf
    "\n  calibrateBPlusScalar chose li=%d ll=%d (ties break toward the smaller index)\n"
    (fst (calBest bp))
    (snd (calBest bp))
  printf "  resolvable: %s\n" (show (calResolvable bp))

  -- Reporting one winner overstates the precision. What matters is how wide the
  -- region of near-optimal configurations is: if several are within a few per
  -- cent, the answer is a region and which member wins moves between runs.
  let sorted = sortOn mNanosPerQuery (calMeasurements bp)
      within p = length [m | m <- sorted, mNanosPerQuery m <= best * (1 + p)]
  printf
    "  configurations within 5%%/10%%/25%% of the best: %d, %d, %d of 36\n"
    (within 0.05)
    (within 0.10)
    (within 0.25)
  printf
    "  so read this as a region -- narrow index, moderate leaf -- not a single pair\n"

  putStrLn "\nsame session, for scale"
  printf
    "  B-tree scalar, best width L=%d: %7.1f ns\n"
    (calBest scalar)
    (pickNs scalar)
  printf
    "  B-tree batch,  best width L=%d: %7.1f ns\n"
    (calBest batch)
    (pickNs batch)
  printf "  B+tree one-at-a-time, best:       %7.1f ns\n" best
  printf "  B+tree bulk, best:                %7.1f ns\n" bulkBest
  where
    header = do
      printf "        "
      mapM_ (\ll -> printf "   ll=%-4d" ll) widths
      putStrLn ""
    row li f = do
      printf "  li=%-3d" li
      mapM_ (\ll -> f ll >> printf "  ") widths
      putStrLn ""
    pickNs c =
      minimum
        [mNanosPerQuery m | m <- calMeasurements c, mWidths m == calBest c]
