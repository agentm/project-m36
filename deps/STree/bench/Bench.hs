{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Criterion benchmarks for the static B-tree.
--
-- The interesting axes are size — because the whole point of the layout is
-- cache behaviour, the sizes below span L1 to main memory — and node width,
-- which is the one real tuning knob. Binary search over the original sorted
-- array is measured alongside, since that is what the tree has to beat to be
-- worth using.
--
-- Note on how the query benchmarks are written: every @env@ line below fixes
-- the node width concretely at its call site, so GHC specialises the query loop
-- for it. Wrapping these in a width-polymorphic helper would leave a dictionary
-- in the inner loop and quietly flatten the differences between widths, which is
-- exactly what this is trying to measure.
module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad.ST (runST)
import Criterion.Main
import qualified Data.Vector.Algorithms.Search as VA
import Data.Int (Int32)
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Data.STree
import Data.STree.Batch
import qualified Data.STree.BPlusTree as BP
import Data.STree.Serialize

import Data.Vector.Algorithms.Intro (sort)

-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

randoms :: Word64 -> [Word64]
randoms = drop 1 . iterate (\s -> s * 6364136223846793005 + 1442695040888963407)

keysOf :: Word64 -> Int -> U.Vector Int32
keysOf seed n = U.fromList (sort (map toKey (take n (randoms seed))))
  where
    toKey w = let k = fromIntegral w in if k == maxBound then k - 1 else k

-- | One iteration of a query benchmark answers this many queries, so that the
-- measurement is well above the timer's resolution and reports throughput
-- rather than the latency of a single dependent load chain.
--
-- Caveat worth knowing: criterion runs the same queries thousands of times, so a
-- thousand of them keeps their root-to-leaf paths hot. For the B-tree that does
-- not matter -- its batch path measures the same at a thousand queries as at two
-- hundred thousand -- but the B+tree's bulk path is sensitive to it, and the best
-- leaf width there flips between the two regimes. See the README, and prefer
-- calibration over these numbers for that structure.
queryCount :: Int
queryCount = 1000

queriesOf :: Int -> U.Vector Int32
queriesOf n = U.fromList (map toKey (take n (randoms 0xBEEF)))
  where
    toKey w = let k = fromIntegral w in if k == maxBound then k - 1 else k

setup :: forall l. LineWidth l => Int -> IO (BTree l Int32, U.Vector Int32)
setup n = return (unsafeBuild (keysOf 42 n), queriesOf queryCount)

-- ---------------------------------------------------------------------------
-- What is being measured
-- ---------------------------------------------------------------------------

sumLower :: forall l. LineWidth l => BTree l Int32 -> U.Vector Int32 -> Int
sumLower t = U.foldl' (\ !acc k -> acc + lowerBoundIdx t k) 0
{-# INLINE sumLower #-}

-- | The batched, vectorised path. Summing forces every result.
sumLowerSimd :: forall l. LineWidth l => BTree l Int32 -> U.Vector Int32 -> Int
sumLowerSimd t = U.sum . lowerBoundIdxMany t
{-# INLINE sumLowerSimd #-}

sumIndex :: forall l. LineWidth l => BTree l Int32 -> U.Vector Int -> Int
sumIndex t = U.foldl' (\ !acc i -> acc + fromIntegral (t ! i)) 0
{-# INLINE sumIndex #-}

-- | A B+tree over the same keys, values being the position.
setupBP :: forall li ll. (LineWidth li, LineWidth ll) => Int -> IO (BP.BPlusTree li ll Int32 Int, U.Vector Int32)
setupBP n = return (BP.unsafeBuild (keysOf 42 n) (U.enumFromN 0 n), queriesOf queryCount)

sumLowerBP :: forall li ll. (LineWidth li, LineWidth ll) => BP.BPlusTree li ll Int32 Int -> U.Vector Int32 -> Int
sumLowerBP t = U.foldl' (\ !acc k -> acc + BP.lowerBoundIdx t k) 0
{-# INLINE sumLowerBP #-}

-- | The bulk path: vectorised index descent, scalar leaf rank.
sumLowerBPMany :: forall li ll. (LineWidth li, LineWidth ll) => BP.BPlusTree li ll Int32 Int -> U.Vector Int32 -> Int
sumLowerBPMany t = U.sum . BP.lowerBoundIdxMany t
{-# INLINE sumLowerBPMany #-}

sumLookupBP :: forall li ll. (LineWidth li, LineWidth ll) => BP.BPlusTree li ll Int32 Int -> U.Vector Int32 -> Int
sumLookupBP t = U.foldl' (\ !acc k -> acc + U.sum (BP.lookup t k)) 0
{-# INLINE sumLookupBP #-}

sumKeyAtBP :: forall li ll. BP.BPlusTree li ll Int32 Int -> U.Vector Int -> Int
sumKeyAtBP t = U.foldl' (\ !acc i -> acc + fromIntegral (BP.keyAt t i)) 0
{-# INLINE sumKeyAtBP #-}

-- The baselines. A tree that cannot beat binary search over the same sorted
-- array has no reason to exist, so what it is compared against matters as much
-- as the tree itself. Three variants, because a single hand-written one invites
-- the question of whether it is a strawman:
--
--   * the textbook branchy loop,
--   * the branchless form, which is what a tuned implementation looks like and
--     the real competition for a cache-conscious layout,
--   * @vector-algorithms@, i.e. what someone would actually reach for.
--
-- 'checkBaselines' asserts all three agree before anything is timed.

-- | Textbook binary search: one unpredictable branch per step. Duplicated from
-- the test suite's @Oracle@ rather than shared, so the benchmark measures a
-- self-contained, obviously-correct competitor.
lowerBoundRef :: U.Vector Int32 -> Int32 -> Int
lowerBoundRef v x = go 0 (U.length v)
  where
    go !lo !hi
      | lo >= hi = lo
      | U.unsafeIndex v mid < x = go (mid + 1) hi
      | otherwise = go lo mid
      where
        mid = (lo + hi) `quot` 2

-- | Branchless binary search: the loop trip count depends only on the length,
-- and the comparison feeds a select rather than a jump, so nothing is
-- mispredicted. The classic formulation -- halve the length, conditionally
-- advance the base, and settle the last element at the end.
lowerBoundBranchless :: U.Vector Int32 -> Int32 -> Int
lowerBoundBranchless v x
  | n == 0 = 0
  | otherwise = let b = go 0 n in b + (if U.unsafeIndex v b < x then 1 else 0)
  where
    n = U.length v
    go !base !len
      | len <= 1 = base
      | otherwise = go base' (len - half)
      where
        half = len `quot` 2
        base' = if U.unsafeIndex v (base + half - 1) < x then base + half else base

-- | Sequential scan: walk from the start until a key stops you. No dependent
-- loads, perfect prefetch, and no log factor -- which is exactly why it wins
-- below some size, and the point of the sweep below is to find where.
--
-- Queries are drawn from the same uniform distribution as the keys, so a query's
-- rank among them is uniform and the scan does n/2 comparisons on average. That
-- is the fair figure; a scan measured with out-of-range queries either exits
-- immediately or walks everything.
lowerBoundLinear :: U.Vector Int32 -> Int32 -> Int
lowerBoundLinear v x = go 0
  where
    n = U.length v
    go !i
      | i >= n = n
      | U.unsafeIndex v i >= x = i
      | otherwise = go (i + 1)

sumLowerLinear :: U.Vector Int32 -> U.Vector Int32 -> Int
sumLowerLinear v = U.foldl' (\ !acc k -> acc + lowerBoundLinear v k) 0

sumLowerRef :: U.Vector Int32 -> U.Vector Int32 -> Int
sumLowerRef v = U.foldl' (\ !acc k -> acc + lowerBoundRef v k) 0

sumLowerBranchless :: U.Vector Int32 -> U.Vector Int32 -> Int
sumLowerBranchless v = U.foldl' (\ !acc k -> acc + lowerBoundBranchless v k) 0

-- | @vector-algorithms@. Its @binarySearchL@ works on a mutable vector, so the
-- array is thawed once per call to the fold, not per query; nothing writes to
-- it, which is what makes the unsafe thaw acceptable here.
sumLowerVA :: U.Vector Int32 -> U.Vector Int32 -> Int
sumLowerVA v qs = runST $ do
  mv <- U.unsafeThaw v
  U.foldM' (\ !acc k -> (acc +) <$> VA.binarySearchL mv k) 0 qs

-- | A baseline that disagrees would make every comparison meaningless, so check
-- before timing rather than trusting three separate implementations.
checkBaselines :: IO ()
checkBaselines = mapM_ one [0, 1, 2, 17, 1000]
  where
    one n = do
      let v = keysOf 42 n
          qs = queriesOf 200 U.++ v
          expect = U.map (lowerBoundRef v) qs
      mapM_
        (\(nm, got) ->
           if got == expect
             then return ()
             else error ("benchmark baseline " ++ nm ++ " disagrees at n=" ++ show n))
        [ ("branchless", U.map (lowerBoundBranchless v) qs)
        , ("linear scan", U.map (lowerBoundLinear v) qs)
        , ("vector-algorithms", U.map (\k -> sumLowerVA v (U.singleton k)) qs)
        ]

-- ---------------------------------------------------------------------------
-- Benchmarks
-- ---------------------------------------------------------------------------

-- 4 KB to 40 MB of keys: inside L1, inside L2, inside L3, and well past it.
sizes :: [Int]
sizes = [1000, 10000, 100000, 1000000, 10000000]

main :: IO ()
main = do
  checkBaselines
  defaultMain
    [ bgroup
        ("lower bound, " ++ show queryCount ++ " queries, L=16, Int32")
        [ bgroup
            (show n)
            [ env (setup @16 n) $ \ ~(t, qs) ->
                bench "s-tree" (whnf (sumLower t) qs)
            , env (return (keysOf 42 n, queriesOf queryCount)) $ \ ~(v, qs) ->
                bench "binary search" (whnf (sumLowerRef v) qs)
            , env (return (keysOf 42 n, queriesOf queryCount)) $ \ ~(v, qs) ->
                bench "binary search, branchless" (whnf (sumLowerBranchless v) qs)
            , env (return (keysOf 42 n, queriesOf queryCount)) $ \ ~(v, qs) ->
                bench "binary search, vector-algorithms" (whnf (sumLowerVA v) qs)
            ]
        | n <- sizes
        ]
    , bgroup
        ("lower bound by node width, " ++ show queryCount ++ " queries, n=1000000")
        [ env (setup @2 nMid) $ \ ~(t, qs) -> bench "L=2" (whnf (sumLower t) qs)
        , env (setup @4 nMid) $ \ ~(t, qs) -> bench "L=4" (whnf (sumLower t) qs)
        , env (setup @8 nMid) $ \ ~(t, qs) -> bench "L=8" (whnf (sumLower t) qs)
        , env (setup @16 nMid) $ \ ~(t, qs) -> bench "L=16" (whnf (sumLower t) qs)
        , env (setup @32 nMid) $ \ ~(t, qs) -> bench "L=32" (whnf (sumLower t) qs)
        , env (setup @64 nMid) $ \ ~(t, qs) -> bench "L=64" (whnf (sumLower t) qs)
        , env (return (keysOf 42 nMid, queriesOf queryCount)) $ \ ~(v, qs) ->
            bench "binary search" (whnf (sumLowerRef v) qs)
        , env (return (keysOf 42 nMid, queriesOf queryCount)) $ \ ~(v, qs) ->
            bench "binary search, vector-algorithms" (whnf (sumLowerVA v) qs)
        ]
    , -- Small n, where a scan is the thing to beat. O(n) with perfect locality
      -- against O(log n) with a dependent load per level: the crossover is what
      -- this group is for.
      bgroup
        ("scan vs tree, " ++ show queryCount ++ " queries, Int32")
        [ bgroup
            (show n)
            [ env (return (keysOf 42 n, queriesOf queryCount)) $ \ ~(v, qs) ->
                bench "linear scan" (whnf (sumLowerLinear v) qs)
            , env (return (keysOf 42 n, queriesOf queryCount)) $ \ ~(v, qs) ->
                bench "binary search" (whnf (sumLowerVA v) qs)
            , env (setup @16 n) $ \ ~(t, qs) ->
                bench "s-tree L=16" (whnf (sumLower t) qs)
            , env (setup @4 n) $ \ ~(t, qs) ->
                bench "s-tree L=4" (whnf (sumLower t) qs)
            , env (setup @16 n) $ \ ~(t, qs) ->
                bench "s-tree L=16 simd" (whnf (sumLowerSimd t) qs)
            , env (setup @4 n) $ \ ~(t, qs) ->
                bench "s-tree L=4 simd" (whnf (sumLowerSimd t) qs)
            ]
        | n <- [4, 8, 16, 32, 64, 128, 256, 512, 1024, 4096, 16384]
        ]
    , -- The same sweep over a tree that fits in L1. Comparing per-level cost
      -- against the sweep above separates the work the descent does from the
      -- memory latency it waits on, which is what decides whether interleaving
      -- several descents (batched queries) could buy anything.
      bgroup
        ("lower bound by node width, " ++ show queryCount ++ " queries, n=1000 (L1-resident)")
        [ env (setup @2 nSmall) $ \ ~(t, qs) -> bench "L=2" (whnf (sumLower t) qs)
        , env (setup @4 nSmall) $ \ ~(t, qs) -> bench "L=4" (whnf (sumLower t) qs)
        , env (setup @8 nSmall) $ \ ~(t, qs) -> bench "L=8" (whnf (sumLower t) qs)
        , env (setup @16 nSmall) $ \ ~(t, qs) -> bench "L=16" (whnf (sumLower t) qs)
        , env (setup @32 nSmall) $ \ ~(t, qs) -> bench "L=32" (whnf (sumLower t) qs)
        , env (setup @64 nSmall) $ \ ~(t, qs) -> bench "L=64" (whnf (sumLower t) qs)
        ]
    , -- The payoff: same queries, pure scalar descent against the vectorised
      -- batch. Both answer identical indices; the test suite pins that.
      -- Written out per width for the reason in the module header: a
      -- width-polymorphic helper would leave a dictionary in the pure loop.
      bgroup
        ("pure vs SIMD batch, " ++ show queryCount ++ " queries, n=1000000")
        [ bgroup
            "L=2"
            [ env (setup @2 nMid) $ \ ~(t, qs) -> bench "pure" (whnf (sumLower t) qs)
            , env (setup @2 nMid) $ \ ~(t, qs) -> bench "simd" (whnf (sumLowerSimd t) qs)
            ]
        , bgroup
            "L=4"
            [ env (setup @4 nMid) $ \ ~(t, qs) -> bench "pure" (whnf (sumLower t) qs)
            , env (setup @4 nMid) $ \ ~(t, qs) -> bench "simd" (whnf (sumLowerSimd t) qs)
            ]
        , bgroup
            "L=8"
            [ env (setup @8 nMid) $ \ ~(t, qs) -> bench "pure" (whnf (sumLower t) qs)
            , env (setup @8 nMid) $ \ ~(t, qs) -> bench "simd" (whnf (sumLowerSimd t) qs)
            ]
        , bgroup
            "L=16"
            [ env (setup @16 nMid) $ \ ~(t, qs) -> bench "pure" (whnf (sumLower t) qs)
            , env (setup @16 nMid) $ \ ~(t, qs) -> bench "simd" (whnf (sumLowerSimd t) qs)
            ]
        , bgroup
            "L=32"
            [ env (setup @32 nMid) $ \ ~(t, qs) -> bench "pure" (whnf (sumLower t) qs)
            , env (setup @32 nMid) $ \ ~(t, qs) -> bench "simd" (whnf (sumLowerSimd t) qs)
            ]
        , bgroup
            "L=64"
            [ env (setup @64 nMid) $ \ ~(t, qs) -> bench "pure" (whnf (sumLower t) qs)
            , env (setup @64 nMid) $ \ ~(t, qs) -> bench "simd" (whnf (sumLowerSimd t) qs)
            ]
        ]
    , bgroup
        ("SIMD batch by size, " ++ show queryCount ++ " queries, L=32, Int32")
        [ env (setup @32 n) $ \ ~(t, qs) -> bench (show n) (whnf (sumLowerSimd t) qs)
        | n <- sizes
        ]
    , bgroup
        "upper bound vs lower bound (L=16, n=1000000)"
        [ env (setup @16 nMid) $ \ ~(t, qs) ->
            bench "lowerBoundIdx" (whnf (sumLower t) qs)
        , env (setup @16 nMid) $ \ ~(t, qs) ->
            bench "upperBoundIdx" (whnf (U.foldl' (\ !acc k -> acc + upperBoundIdx t k) 0) qs)
        ]
    , bgroup
        ("index (operator[]), " ++ show queryCount ++ " positions, L=16, Int32")
        [ env (positions n) $ \ ~(t, is) -> bench (show n) (whnf (sumIndex t) is)
        | n <- sizes
        ]
    , bgroup
        "build, L=16, Int32"
        [ bgroup
            (show n)
            [ env (return (keysOf 42 n)) $ \v ->
                bench "unsafeBuild" (nf (unsafeBuild @16 @Int32) v)
            , -- Forcing the size forces the whole tree: the record's fields are
              -- strict, so its constructor cannot exist half-built.
              env (return (keysOf 42 n)) $ \v ->
                bench "build (checked)" (nf (either (const 0) size . build @16 @Int32) v)
            ]
        | n <- sizes
        ]
    , bgroup
        ("B+tree against B-tree, n=1000000, Int32, " ++ show queryCount ++ " queries")
        [ bgroup
            "lower bound (position only)"
            [ env (setup @16 nMid) $ \ ~(t, qs) -> bench "btree L=16" (whnf (sumLower t) qs)
            , env (setupBP @16 @16 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=16" (whnf (sumLowerBP t) qs)
            , env (setupBP @16 @32 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=32" (whnf (sumLowerBP t) qs)
            , env (setupBP @2 @16 nMid) $ \ ~(t, qs) -> bench "b+tree li=2 ll=16" (whnf (sumLowerBP t) qs)
            , env (setupBP @4 @16 nMid) $ \ ~(t, qs) -> bench "b+tree li=4 ll=16" (whnf (sumLowerBP t) qs)
            , env (setupBP @8 @16 nMid) $ \ ~(t, qs) -> bench "b+tree li=8 ll=16" (whnf (sumLowerBP t) qs)
            , env (setupBP @32 @16 nMid) $ \ ~(t, qs) -> bench "b+tree li=32 ll=16" (whnf (sumLowerBP t) qs)
            , env (setupBP @4 @8 nMid) $ \ ~(t, qs) -> bench "b+tree li=4 ll=8" (whnf (sumLowerBP t) qs)
            , env (setupBP @4 @32 nMid) $ \ ~(t, qs) -> bench "b+tree li=4 ll=32" (whnf (sumLowerBP t) qs)
            , env (setupBP @4 @4 nMid) $ \ ~(t, qs) -> bench "b+tree li=4 ll=4" (whnf (sumLowerBP t) qs)
            , env (setupBP @2 @8 nMid) $ \ ~(t, qs) -> bench "b+tree li=2 ll=8" (whnf (sumLowerBP t) qs)
            , env (setupBP @2 @4 nMid) $ \ ~(t, qs) -> bench "b+tree li=2 ll=4" (whnf (sumLowerBP t) qs)
            , env (setupBP @2 @2 nMid) $ \ ~(t, qs) -> bench "b+tree li=2 ll=2" (whnf (sumLowerBP t) qs)
            , env (setupBP @8 @8 nMid) $ \ ~(t, qs) -> bench "b+tree li=8 ll=8" (whnf (sumLowerBP t) qs)
            , env (setupBP @16 @64 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=64" (whnf (sumLowerBP t) qs)
            , env (return (keysOf 42 nMid, queriesOf queryCount)) $ \ ~(v, qs) ->
                bench "binary search" (whnf (sumLowerRef v) qs)
            , env (return (keysOf 42 nMid, queriesOf queryCount)) $ \ ~(v, qs) ->
                bench "binary search, branchless" (whnf (sumLowerBranchless v) qs)
            , env (return (keysOf 42 nMid, queriesOf queryCount)) $ \ ~(v, qs) ->
                bench "binary search, vector-algorithms" (whnf (sumLowerVA v) qs)
            ]
        , bgroup
            "bulk position query (vectorised index, scalar leaf)"
            [ env (setupBP @2 @8 nMid) $ \ ~(t, qs) -> bench "b+tree li=2 ll=8" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @4 @8 nMid) $ \ ~(t, qs) -> bench "b+tree li=4 ll=8" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @4 @16 nMid) $ \ ~(t, qs) -> bench "b+tree li=4 ll=16" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @16 @4 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=4" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @16 @8 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=8" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @16 @16 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=16" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @32 @4 nMid) $ \ ~(t, qs) -> bench "b+tree li=32 ll=4" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @32 @8 nMid) $ \ ~(t, qs) -> bench "b+tree li=32 ll=8" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @8 @4 nMid) $ \ ~(t, qs) -> bench "b+tree li=8 ll=4" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @8 @2 nMid) $ \ ~(t, qs) -> bench "b+tree li=8 ll=2" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @16 @2 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=2" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @32 @2 nMid) $ \ ~(t, qs) -> bench "b+tree li=32 ll=2" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @64 @4 nMid) $ \ ~(t, qs) -> bench "b+tree li=64 ll=4" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @16 @32 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=32" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @16 @64 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=64" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @32 @32 nMid) $ \ ~(t, qs) -> bench "b+tree li=32 ll=32" (whnf (sumLowerBPMany t) qs)
            , env (setupBP @32 @64 nMid) $ \ ~(t, qs) -> bench "b+tree li=32 ll=64" (whnf (sumLowerBPMany t) qs)
            , env (setup @16 nMid) $ \ ~(t, qs) -> bench "btree L=16 (full SIMD)" (whnf (sumLowerSimd t) qs)
            ]
        , bgroup
            "full lookup (position, key check and value)"
            [ env (setupBP @16 @16 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=16" (whnf (sumLookupBP t) qs)
            , env (setupBP @16 @32 nMid) $ \ ~(t, qs) -> bench "b+tree li=16 ll=32" (whnf (sumLookupBP t) qs)
            ]
        , bgroup
            "key at a position"
            [ env (positions nMid) $ \ ~(t, is) -> bench "btree index" (whnf (sumIndex t) is)
            , env (positionsBP nMid) $ \ ~(t, is) -> bench "b+tree keyAt" (whnf (sumKeyAtBP t) is)
            ]
        , bgroup
            "recover all keys in order"
            [ env (setup @16 nMid) $ \ ~(t, _) -> bench "btree toVector" (whnf (U.sum . toVector) t)
            , env (setupBP @16 @16 nMid) $ \ ~(t, _) -> bench "b+tree keys" (whnf (U.sum . BP.keys) t)
            ]
        , bgroup
            "build"
            [ env (return (keysOf 42 nMid)) $ \v -> bench "btree" (nf (unsafeBuild @16 @Int32) v)
            , env (return (keysOf 42 nMid, U.enumFromN (0 :: Int) nMid)) $ \ ~(ks, vs) ->
                bench "b+tree li=16 ll=16" (nf (\(a, b) -> BP.size (BP.unsafeBuild a b :: BP.BPlusTree 16 16 Int32 Int)) (ks, vs))
            ]
        ]
    , -- Repeated runs hit the page cache, so this measures the copy-and-syscall
      -- path, not the disk. That is the right number for a process that loads an
      -- index it just wrote; a cold read is bounded by the device instead.
      bgroup
        "serialization (page cache warm), L=16, Int32"
        [ envWithCleanup (setupFile n) cleanupFile $ \ ~(path, t) ->
            bgroup
              (show n)
              [ bench "writeSTree" (nfIO (writeSTree path t))
              , bench "readSTree" (nfIO (loadSize path))
              ]
        | n <- [100000, 1000000, 10000000]
        ]
    ]
  where
    nMid = 1000000
    nSmall = 1000


    positionsBP :: Int -> IO (BP.BPlusTree 16 16 Int32 Int, U.Vector Int)
    positionsBP n =
      return
        ( BP.unsafeBuild (keysOf 42 n) (U.enumFromN 0 n)
        , U.fromList (map (\w -> fromIntegral (w `rem` fromIntegral n)) (take queryCount (randoms 99)))
        )

    positions :: Int -> IO (BTree 16 Int32, U.Vector Int)
    positions n =
      return
        ( unsafeBuild (keysOf 42 n)
        , U.fromList (map (\w -> fromIntegral (w `rem` fromIntegral n)) (take queryCount (randoms 99)))
        )

    setupFile :: Int -> IO (FilePath, BTree 16 Int32)
    setupFile n = do
      dir <- getTemporaryDirectory
      let path = dir </> ("s-tree-bench-" ++ show n ++ ".st")
          t = unsafeBuild (keysOf 42 n)
      writeSTree path t
      return (path, t)

    cleanupFile (path, _) =
      removeFile path `catch` \e -> let _ = (e :: IOException) in return ()

    loadSize :: FilePath -> IO Int
    loadSize path = do
      r <- readSTree path :: IO (Either SerializeError (BTree 16 Int32))
      case r of
        Left err -> ioError (userError (show err))
        Right t -> return (size t)
