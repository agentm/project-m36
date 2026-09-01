{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The test suite.
--
-- Almost everything here is checked against plain binary search over the
-- original sorted array (see "Oracle"): that is the specification a lower or
-- upper bound has, so agreeing with it exactly is the strongest statement
-- available.
module Main (main) where

import Control.Exception (IOException, catch, finally)
import Control.Monad (forM_, unless, when)
import Data.Bits (xor)
import qualified Data.ByteString as B
import Data.Int (Int32, Int64)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Proxy (Proxy)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word64, Word8)
import Foreign.Storable (sizeOf)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hClose, hFileSize, hSeek, openBinaryTempFile, withBinaryFile)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC

import Data.STree
import Data.STree.BTree (exceedingLeaves, layout, layoutLength, unsafeFromLayout)
import Data.STree.Batch
import Data.STree.Calibrate
import qualified Data.STree.BPlus as BP
import Data.STree.Dot
import Data.STree.Internal.Rank (blockRank, refBlockRank)
import Data.STree.Serialize

import Oracle (lowerBoundRef, upperBoundRef)

-- ---------------------------------------------------------------------------
-- Deterministic sample data, one instance per supported key type
-- ---------------------------------------------------------------------------

class Key a => TestKey a where
  -- | Map a random word to an admissible key (never @>= sentinel@).
  fromSeed :: Word64 -> a
  keyName :: String

instance TestKey Int32 where
  fromSeed w = let k = fromIntegral w in if k == maxBound then k - 1 else k
  keyName = "Int32"

instance TestKey Int64 where
  fromSeed w = let k = fromIntegral w in if k == maxBound then k - 1 else k
  keyName = "Int64"

instance TestKey Word32 where
  fromSeed w = let k = fromIntegral w in if k == maxBound then k - 1 else k
  keyName = "Word32"

instance TestKey Word64 where
  fromSeed w = let k = fromIntegral w in if k == maxBound then k - 1 else k
  keyName = "Word64"

instance TestKey Float where
  fromSeed w = fromIntegral (fromIntegral w :: Int32) * 0.5
  keyName = "Float"

instance TestKey Double where
  fromSeed w = fromIntegral (fromIntegral w :: Int64) * 0.5
  keyName = "Double"

-- | A cheap deterministic stream, so failures reproduce exactly.
randoms :: Word64 -> [Word64]
randoms = drop 1 . iterate (\s -> s * 6364136223846793005 + 1442695040888963407)

sortedSample :: forall a. TestKey a => Word64 -> Int -> U.Vector a
sortedSample seed n = U.fromList (sort (map (fromSeed @a) (take n (randoms seed))))

-- | Keys to query with: mostly absent ones, plus a few deliberately taken from
-- the data itself.
probeKeys :: forall a. TestKey a => Int -> U.Vector a -> [a]
probeKeys count v =
  map (fromSeed @a) (take count (randoms 0xBEEF))
    ++ concat [[v U.! i] | n > 0, i <- [0, n `quot` 2, n - 1]]
  where
    n = U.length v

-- ---------------------------------------------------------------------------
-- The tests for one (line width, key type) pair
-- ---------------------------------------------------------------------------

-- | Sizes to build trees of: a spread of small and large ones, the degenerate
-- 0 and 1, and the exact powers of the fanout with their neighbours, which are
-- where the height computation and the partial deepest level are most
-- delicate.
sizesFor :: Int -> [Int]
sizesFor l =
  dedup (sort ([0, 1, 2, 3, 7, 8, 20, 63, 64, 65, 1000, 100000] ++ aroundPowers))
  where
    b = l + 1
    aroundPowers =
      [ n
      | k <- [1 .. 6 :: Int]
      , let p = b ^ k
      , p <= 200000
      , n <- [p - 1, p, p + 1]
      ]
    dedup (x : y : ys) | x == y = dedup (y : ys)
    dedup (x : ys) = x : dedup ys
    dedup [] = []

-- | Exhaustively check every position for small inputs; sample for big ones,
-- to keep the suite's runtime reasonable across 6 widths x 6 key types.
positionsOf :: Int -> [Int]
positionsOf n
  | n <= 5000 = [0 .. n - 1]
  | otherwise = [i * step | i <- [0 .. 1999]] ++ [n - 1]
  where
    step = max 1 (n `quot` 2000)

buildCase :: forall l a. (LineWidth l, TestKey a, Show a) => Int -> Assertion
buildCase n = do
  let v = sortedSample @a 42 n
  t <- case build v :: Either BuildError (BTree l a) of
    Left err -> assertFailure ("build failed: " ++ show err)
    Right t -> return t

  assertEqual "height" (logSize @l n) (height t)
  assertEqual "size" n (size t)
  assertEqual "layoutLength" (U.length (layout t)) (layoutLength @l n)

  -- Rebuilding the record from the layout must reproduce it exactly. This is
  -- what pins the loader's closed form for the exceeding-node count against
  -- the value the build loop computes, at every size and width covered.
  assertBool "unsafeFromLayout disagrees with build" (unsafeFromLayout n (layout t) == t)

  -- (a) the layout is a permutation that 'index' inverts: t ! i == v ! i.
  --     This alone pins down the whole build.
  forM_ (positionsOf n) $ \i ->
    unless (t ! i == v U.! i) $
      assertFailure ("index " ++ show i ++ ": expected " ++ show (v U.! i) ++ ", got " ++ show (t ! i))

  -- (b) every key present in the data is found -- compared by value, since
  --     with duplicates the index need not be i.
  forM_ (positionsOf n) $ \i -> do
    let k = v U.! i
    unless (v U.! lowerBoundIdx t k == k) $
      assertFailure ("lower bound of present key at " ++ show i ++ " gave " ++ show (lowerBoundIdx t k))
    unless (v U.! (upperBoundIdx t k - 1) == k) $
      assertFailure ("upper bound of present key at " ++ show i ++ " gave " ++ show (upperBoundIdx t k))

  -- (c) both bounds agree with the oracle exactly, on absent and present keys.
  forM_ (probeKeys @a 500 v) $ \k -> do
    assertEqual ("lowerBoundIdx " ++ show k) (lowerBoundRef v k) (lowerBoundIdx t k)
    assertEqual ("upperBoundIdx " ++ show k) (upperBoundRef v k) (upperBoundIdx t k)

-- | Same as 'buildCase' but over data drawn from a tiny range, so almost every
-- key is duplicated many times.
duplicatesCase :: forall l a. (LineWidth l, TestKey a, Show a) => Int -> Assertion
duplicatesCase n = do
  let v = U.fromList (sort (map (\w -> fromSeed @a (w `rem` 7)) (take n (randoms 7))))
  t <- case build v :: Either BuildError (BTree l a) of
    Left err -> assertFailure ("build failed: " ++ show err)
    Right t -> return t
  forM_ [0 .. n - 1] $ \i -> assertEqual ("index " ++ show i) (v U.! i) (t ! i)
  forM_ (U.toList v ++ map (fromSeed @a) [0 .. 9]) $ \k -> do
    assertEqual ("lowerBoundIdx " ++ show k) (lowerBoundRef v k) (lowerBoundIdx t k)
    assertEqual ("upperBoundIdx " ++ show k) (upperBoundRef v k) (upperBoundIdx t k)

widthTests :: forall l a. (LineWidth l, TestKey a, Show a) => TestTree
widthTests =
  testGroup
    ("L=" ++ show (lineWidth @l))
    [ testGroup "sizes" [testCase ("n=" ++ show n) (buildCase @l @a n) | n <- sizesFor (lineWidth @l)]
    , testGroup "duplicates" [testCase ("n=" ++ show n) (duplicatesCase @l @a n) | n <- [1, 5, 64, 300, 2000]]
    ]

keyTypeTests :: forall a. (TestKey a, Show a) => TestTree
keyTypeTests =
  testGroup
    (keyName @a)
    [ widthTests @2 @a
    , widthTests @4 @a
    , widthTests @8 @a
    , widthTests @16 @a
    , widthTests @32 @a
    , widthTests @64 @a
    ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | 'blockRank' against the independent formulation, on arbitrary (not
-- necessarily sorted) blocks, for every supported width.
prop_blockRank :: Int -> [Int] -> Int -> QC.Property
prop_blockRank rawL rawKeys x =
  QC.forAll (QC.elements [2, 4, 8, 16, 32, 64]) $ \l ->
    let keys = U.fromList (take (2 * l) (cycle (if null rawKeys then [0] else rawKeys)))
        off = (abs rawL `rem` (l + 1)) `min` l
     in blockRank l (>= x) keys off === refBlockRank l (>= x) keys off

-- | Both bounds against the oracle on arbitrary data, for a representative
-- width and key type.
prop_bounds :: [Int64] -> Int64 -> QC.Property
prop_bounds raw x =
  let v = U.fromList (sort (filter (/= maxBound) raw))
      t = unsafeBuild v :: BTree 16 Int64
   in QC.conjoin
        [ QC.counterexample "lower" (lowerBoundIdx t x === lowerBoundRef v x)
        , QC.counterexample "upper" (upperBoundIdx t x === upperBoundRef v x)
        , QC.counterexample "round-trip" (toVector t === v)
        ]

-- | Same, with a small fanout so that the trees are deep even for short
-- inputs, and with a floating point key type.
prop_boundsNarrow :: [Double] -> Double -> QC.Property
prop_boundsNarrow raw x =
  not (any isNaN raw) && not (isNaN x) QC.==>
    let v = U.fromList (sort raw)
        t = unsafeBuild v :: BTree 4 Double
     in QC.conjoin
          [ QC.counterexample "lower" (lowerBoundIdx t x === lowerBoundRef v x)
          , QC.counterexample "upper" (upperBoundIdx t x === upperBoundRef v x)
          , QC.counterexample "round-trip" (toVector t === v)
          ]

-- | Results are always usable as slice bounds.
prop_inRange :: [Int32] -> Int32 -> QC.Property
prop_inRange raw x =
  let v = U.fromList (sort (filter (/= maxBound) raw))
      t = unsafeBuild v :: BTree 8 Int32
      lo = lowerBoundIdx t x
      hi = upperBoundIdx t x
   in QC.counterexample (show (lo, hi)) (0 <= lo && lo <= hi && hi <= U.length v)

-- ---------------------------------------------------------------------------
-- Rejections and edge cases
-- ---------------------------------------------------------------------------

rejectionTests :: TestTree
rejectionTests =
  testGroup
    "input validation"
    [ testCase "unsorted is rejected" $
        assertEqual "" (Left NotSorted) (fmap size (buildFromList [3, 1, 2 :: Int32] :: Either BuildError (BTree 16 Int32)))
    , testCase "sentinel key is rejected" $
        assertEqual "" (Left KeyNotBelowSentinel) (fmap size (buildFromList [1, maxBound :: Int32] :: Either BuildError (BTree 16 Int32)))
    , testCase "infinity is rejected for Double" $
        assertEqual "" (Left KeyNotBelowSentinel) (fmap size (buildFromList [1, 1 / 0 :: Double] :: Either BuildError (BTree 16 Double)))
    , testCase "NaN is rejected" $
        assertEqual "" (Left NotSorted) (fmap size (buildFromList [1, 0 / 0, 2 :: Double] :: Either BuildError (BTree 16 Double)))
    , testCase "empty tree searches to 0" $ do
        let t = unsafeBuild U.empty :: BTree 16 Int32
        assertEqual "size" 0 (size t)
        assertEqual "lower" 0 (lowerBoundIdx t 5)
        assertEqual "upper" 0 (upperBoundIdx t 5)
        assertEqual "toVector" U.empty (toVector t)
    ]

-- ---------------------------------------------------------------------------
-- Graphviz output
-- ---------------------------------------------------------------------------

countOccurrences :: String -> String -> Int
countOccurrences needle = go
  where
    go h@(_ : rest)
      | needle `isPrefixOf` h = 1 + go rest
      | otherwise = go rest
    go [] = 0

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

-- | The graph has to be a faithful picture of the tree: one node per node of
-- the layout, a tree's worth of edges, and every key exactly once.
dotCase :: forall a l. (LineWidth l, TestKey a, Show a) => Int -> Assertion
dotCase n = do
  let v = sortedSample @a 42 n
      t = unsafeBuild v :: BTree l a
      l = lineWidth @l
      dot = toDot t
      ls = lines dot
      nodeLines = [x | x <- ls, "[label=" `isInfixOf` x, not ("node [" `isInfixOf` x)]
      edgeLines = [x | x <- ls, " -> " `isInfixOf` x]

      -- "  nK [label="<p0>|k0|<p1>"];"  ->  ["k0"]
      cellsOf x =
        let afterEq = drop 1 (dropWhile (/= '=') x)
            inner = takeWhile (/= '"') (drop 1 afterEq)
         in [f | f <- splitOn '|' inner, not ("<p" `isPrefixOf` f)]
      cells = concatMap cellsOf nodeLines
      shown = filter (/= "~") cells
      padding = length (filter (== "~") cells)

      e = exceedingLeaves t
      expectedNodes = if n == 0 then 0 else (n - e) `quot` l + (e + l - 1) `quot` l

  assertBool "not a digraph" ("digraph STree {" `isPrefixOf` dot)
  assertEqual "node count" expectedNodes (length nodeLines)
  -- A tree: every node but the root has exactly one parent.
  assertEqual "edge count" (max 0 (expectedNodes - 1)) (length edgeLines)
  -- Every key is drawn exactly once, and the record cells account for the
  -- whole layout array including its sentinel padding.
  assertEqual "keys drawn" (sort (map show (U.toList v))) (sort shown)
  assertEqual "padding cells" (layoutLength @l n - n) padding
  assertEqual "cells total" (layoutLength @l n) (length cells)

  -- The array variant adds the flat layout as an HTML-like table beneath the
  -- tree: one index cell per slot, and one shaded cell per slot plus one per
  -- level in the legend.
  let withArr = toDotStyled dotStyle {dotArray = True} t
      indexCells = countOccurrences "<FONT POINT-SIZE=\"8\">" withArr
      shadedCells = countOccurrences "<TD BGCOLOR=" withArr
  assertBool "array variant is missing the array" (n == 0 || "layoutArray" `isInfixOf` withArr)
  assertBool "default variant should not draw the array" (not ("layoutArray" `isInfixOf` toDot t))
  assertEqual "one index cell per array slot" (if n == 0 then 0 else layoutLength @l n) indexCells
  assertEqual
    "one shaded cell per slot, plus the legend"
    (if n == 0 then 0 else layoutLength @l n + height t)
    shadedCells

dotWidths :: forall a. (TestKey a, Show a) => [(Int, Int -> Assertion)]
dotWidths =
  [ (2, dotCase @a @2)
  , (4, dotCase @a @4)
  , (8, dotCase @a @8)
  , (16, dotCase @a @16)
  ]

dotTests :: TestTree
dotTests =
  testGroup
    "graphviz output"
    ( [ testGroup
          nm
          [ testCase ("L=" ++ show w ++ " n=" ++ show n) (f n)
          | (w, f) <- widths
          , n <- [0, 1, 2, 5, 10, 63, 64, 65, 300]
          ]
      | (nm, widths) <-
          [ (keyName @Int32, dotWidths @Int32)
          , (keyName @Int64, dotWidths @Int64)
          , (keyName @Double, dotWidths @Double)
          ]
      ]
        ++ [dotExampleTest]
    )

-- | L=2, n=10 has layout [5,8,3,4,6,7,9,10,1,2]. The root separates on 5 and 8;
-- the keys below 3 hang off the first port of the node [3,4]. Pinning the exact
-- text keeps the rendering honest about the structure, not just self-consistent.
dotExampleTest :: TestTree
dotExampleTest = testCase "worked example (L=2, n=10)" $ do
  let dot = toDot (unsafeBuild (U.fromList [1 .. 10]) :: BTree 2 Int32)
  -- The label only, not the whole line: node attributes such as the level
  -- shading may be added without changing which keys sit in which node.
  forM_
    [ "n0 [label=\"<p0>|5|<p1>|8|<p2>\""
    , "n2 [label=\"<p0>|3|<p1>|4|<p2>\""
    , "n8 [label=\"<p0>|1|<p1>|2|<p2>\""
    , "n0:p0 -> n2;"
    , "n0:p1 -> n4;"
    , "n0:p2 -> n6;"
    , "n2:p0 -> n8;"
    ]
    $ \frag -> assertBool ("missing: " ++ frag) (frag `isInfixOf` dot)

-- ---------------------------------------------------------------------------
-- Batch (SIMD) path
-- ---------------------------------------------------------------------------

-- | The vectorised descent in C must agree with the pure one exactly — not
-- merely be correct, but return the same index for the same query, since it is
-- a drop-in replacement.
batchCase :: forall a l. (LineWidth l, BatchKey a, TestKey a) => Int -> Assertion
batchCase n = do
  let v = sortedSample @a 42 n
      t = unsafeBuild v :: BTree l a
      -- More than the 256-query chunk size, so the chunking loop is exercised.
      qs = U.fromList (probeKeys @a 700 v)

  assertEqual "lower" (U.map (lowerBoundIdx t) qs) (lowerBoundIdxMany t qs)
  assertEqual "upper" (U.map (upperBoundIdx t) qs) (upperBoundIdxMany t qs)

  -- A slice has a non-zero offset into its backing array, which the FFI has to
  -- account for; a bug here would be invisible with freshly built vectors.
  let sliced = U.slice 3 (U.length qs - 5) qs
  assertEqual "lower/sliced" (U.map (lowerBoundIdx t) sliced) (lowerBoundIdxMany t sliced)

  -- Fewer queries than one chunk, and none at all.
  let short = U.take 5 qs
  assertEqual "lower/short" (U.map (lowerBoundIdx t) short) (lowerBoundIdxMany t short)
  assertEqual "lower/empty" U.empty (lowerBoundIdxMany t U.empty)

-- | The six widths, listed once and reused for every key type.
batchWidths :: forall a. (BatchKey a, TestKey a) => [(Int, Int -> Assertion)]
batchWidths =
  [ (2, batchCase @a @2)
  , (4, batchCase @a @4)
  , (8, batchCase @a @8)
  , (16, batchCase @a @16)
  , (32, batchCase @a @32)
  , (64, batchCase @a @64)
  ]

batchTests :: TestTree
batchTests =
  testGroup
    "batch (SIMD) agrees with the pure descent"
    [ testGroup
        nm
        [ testCase ("L=" ++ show w ++ " n=" ++ show n) (f n)
        | (w, f) <- widths
        , n <- [0, 1, 2, 300, 5000, 100000]
        ]
    | (nm, widths) <-
        [ (keyName @Int32, batchWidths @Int32)
        , (keyName @Int64, batchWidths @Int64)
        , (keyName @Word32, batchWidths @Word32)
        , (keyName @Word64, batchWidths @Word64)
        , (keyName @Float, batchWidths @Float)
        , (keyName @Double, batchWidths @Double)
        ]
    ]


-- ---------------------------------------------------------------------------
-- B+tree
-- ---------------------------------------------------------------------------

-- | Values are the position, so every check reads directly: @valueAt t i == i@,
-- and @lookup t k@ is the list of positions holding k.
bplusCase ::
  forall k li ll.
  (LineWidth li, LineWidth ll, TestKey k, BatchKey k, Show k) =>
  U.Vector k ->
  Assertion
bplusCase ks = do
  let n = U.length ks
      vs = U.enumFromN (0 :: Int) n
      ll = lineWidth @ll
  t <- case BP.build ks vs :: Either BP.BuildError (BP.BPlusTree li ll k Int) of
    Left err -> assertFailure ("build failed: " ++ show err)
    Right t -> return t

  assertEqual "size" n (BP.size t)
  assertEqual "keys" ks (BP.keys t)
  assertEqual "values" vs (BP.values t)
  assertEqual "leafWidth" ll (BP.leafWidth t)

  -- Round trip. Both are O(1) here, unlike the B-tree's index.
  forM_ (positionsOf n) $ \i -> do
    assertEqual ("keyAt " ++ show i) (ks U.! i) (BP.keyAt t i)
    assertEqual ("valueAt " ++ show i) i (BP.valueAt t i)

  -- The separator invariant, checked against the definition rather than its
  -- consequences: one per leaf boundary, each the first key of the leaf after.
  let leafCount = max 1 ((n + ll - 1) `div` ll)
      seps = toVector (BP.indexTree t)
  assertEqual "separator count" (leafCount - 1) (U.length seps)
  forM_ [0 .. leafCount - 2] $ \j ->
    assertEqual ("separator " ++ show j) (ks U.! ((j + 1) * ll)) (seps U.! j)
  -- and that they really do partition: nothing in leaf j exceeds s_j.
  forM_ [0 .. leafCount - 2] $ \j ->
    forM_ [j * ll .. min n ((j + 1) * ll) - 1] $ \i ->
      assertBool
        ("key " ++ show i ++ " above its separator")
        (ks U.! i <= seps U.! j)

  -- The bulk path must agree with the one-at-a-time path exactly: it is a
  -- drop-in replacement, and its index step is vectorised while the leaf rank is
  -- not, so the two halves have to line up.
  let bulkProbes = U.fromList (probeKeys @k 400 ks)
  assertEqual
    "lowerBoundIdxMany"
    (U.map (BP.lowerBoundIdx t) bulkProbes)
    (BP.lowerBoundIdxMany t bulkProbes)
  assertEqual
    "upperBoundIdxMany"
    (U.map (BP.upperBoundIdx t) bulkProbes)
    (BP.upperBoundIdxMany t bulkProbes)
  -- sliced input, so a non-zero offset reaches the FFI underneath
  let slicedProbes = U.slice 3 (U.length bulkProbes - 5) bulkProbes
  assertEqual
    "lowerBoundIdxMany/sliced"
    (U.map (BP.lowerBoundIdx t) slicedProbes)
    (BP.lowerBoundIdxMany t slicedProbes)
  assertEqual "lowerBoundIdxMany/empty" U.empty (BP.lowerBoundIdxMany t U.empty)

  forM_ (probeKeys @k 400 ks) $ \x -> do
    let lo = lowerBoundRef ks x
        hi = upperBoundRef ks x
    assertEqual ("lowerBoundIdx " ++ show x) lo (BP.lowerBoundIdx t x)
    assertEqual ("upperBoundIdx " ++ show x) hi (BP.upperBoundIdx t x)
    assertEqual ("member " ++ show x) (lo < hi) (BP.member t x)
    -- every value stored under x, against a naive filter
    assertEqual
      ("lookup " ++ show x)
      (U.fromList [i | i <- [0 .. n - 1], ks U.! i == x])
      (BP.lookup t x)
    -- an empty interval really is empty
    assertEqual ("rangeIdx " ++ show x ++ " " ++ show x) (lo, lo) (BP.rangeIdx t x x)

  -- Real intervals, from consecutive pairs of probes.
  let probes = probeKeys @k 200 ks
  forM_ (zip probes (drop 1 probes)) $ \(a, b) -> do
    let from = min a b
        to = max a b
        expectIdx = (lowerBoundRef ks from, lowerBoundRef ks to)
        expectVals = U.fromList [i | i <- [0 .. n - 1], ks U.! i >= from, ks U.! i < to]
    assertEqual ("rangeIdx " ++ show (from, to)) expectIdx (BP.rangeIdx t from to)
    assertEqual ("rangeValues " ++ show (from, to)) expectVals (BP.rangeValues t from to)
    assertEqual
      ("rangeKeys " ++ show (from, to))
      (U.fromList [ks U.! i | i <- [0 .. n - 1], ks U.! i >= from, ks U.! i < to])
      (BP.rangeKeys t from to)

-- | Sizes that exercise the leaf boundaries for a given width.
bplusSizes :: Int -> [Int]
bplusSizes ll = [0, 1, 2, ll - 1, ll, ll + 1, 2 * ll, 2 * ll + 1, 300, 5000]

-- | Ordinary data, plus data drawn from a range so small that runs of equal
-- keys straddle leaf boundaries -- the case that distinguishes the two index
-- bounds, and the only one that catches using the wrong one.
bplusCases ::
  forall k li ll.
  (LineWidth li, LineWidth ll, TestKey k, BatchKey k, Show k) =>
  [(String, Assertion)]
bplusCases =
  [ ("n=" ++ show n, bplusCase @k @li @ll (sortedSample @k 42 n))
  | n <- bplusSizes (lineWidth @ll)
  ]
    ++ [ ("duplicates n=" ++ show n, bplusCase @k @li @ll (dupSample @k n))
       | n <- [1, 5, 64, 300, 2000]
       ]
    ++ [("all equal n=" ++ show n, bplusCase @k @li @ll (U.replicate n (fromSeed @k 7))) | n <- [2, 4, 33, 300]]

dupSample :: forall k. TestKey k => Int -> U.Vector k
dupSample n = U.fromList (sort (map (\w -> fromSeed @k (w `rem` 5)) (take n (randoms 7))))

bplusTests :: TestTree
bplusTests =
  testGroup
    "B+tree"
    [ testGroup
        ("leaf widths, index 16, " ++ keyName @Int32)
        [testGroup ("ll=" ++ show w) [testCase nm c | (nm, c) <- cs] | (w, cs) <- leafSweep]
    , testGroup
        ("index widths, leaves 16, " ++ keyName @Int32)
        [testGroup ("li=" ++ show w) [testCase nm c | (nm, c) <- cs] | (w, cs) <- indexSweep]
    , testGroup
        "key types, li=16 ll=16"
        [testGroup nm [testCase cn c | (cn, c) <- cs] | (nm, cs) <- typeSweep]
    , testGroup
        "asymmetric widths, Int32"
        [ testGroup "li=64 ll=2" [testCase nm c | (nm, c) <- bplusCases @Int32 @64 @2]
        , testGroup "li=2 ll=64" [testCase nm c | (nm, c) <- bplusCases @Int32 @2 @64]
        ]
    , testGroup "large" [testCase "li=16 ll=32 n=100000" (bplusCase @Int32 @16 @32 (sortedSample @Int32 42 100000))]
    , bplusGoldenTest
    , bplusDotTests
    , nanTests
    , bplusRejectionTests
    ]
  where
    leafSweep =
      [ (2 :: Int, bplusCases @Int32 @16 @2)
      , (4, bplusCases @Int32 @16 @4)
      , (8, bplusCases @Int32 @16 @8)
      , (16, bplusCases @Int32 @16 @16)
      , (32, bplusCases @Int32 @16 @32)
      , (64, bplusCases @Int32 @16 @64)
      ]
    indexSweep =
      [ (2 :: Int, bplusCases @Int32 @2 @16)
      , (4, bplusCases @Int32 @4 @16)
      , (8, bplusCases @Int32 @8 @16)
      , (32, bplusCases @Int32 @32 @16)
      , (64, bplusCases @Int32 @64 @16)
      ]
    typeSweep =
      [ (keyName @Int64, bplusCases @Int64 @16 @16)
      , (keyName @Word32, bplusCases @Word32 @16 @16)
      , (keyName @Word64, bplusCases @Word64 @16 @16)
      , (keyName @Float, bplusCases @Float @16 @16)
      , (keyName @Double, bplusCases @Double @16 @16)
      ]

-- | Worked out by hand: ten keys at ll=2 make five leaves, so four separators,
-- being the first key of each leaf after the first. Pinning the convention
-- itself, not just what it implies.
-- | The map renderer: every key, value and position drawn once, and exactly one
-- highlight per separator -- the property the diagram exists to show.
bplusDotCase :: forall li ll. (LineWidth li, LineWidth ll) => Int -> Assertion
bplusDotCase n = do
  let ks = sortedSample @Int32 42 n
      vs = U.enumFromN (0 :: Int32) n
      ll = lineWidth @ll
      t = BP.unsafeBuild ks vs :: BP.BPlusTree li ll Int32 Int32
      dot = toDotBPlus t
      leafCount = max 1 ((n + ll - 1) `div` ll)
      padded = leafCount * ll

  assertBool "not a digraph" ("digraph SBPlusTree {" `isPrefixOf` dot)
  -- one position cell per leaf slot, padding included
  assertEqual "position cells" padded (countOccurrences "<FONT POINT-SIZE=\"8\">" dot)
  -- one leaf header per leaf. Count the exact cell marker, not a loose
  -- substring: the legend mentions leaves and uses bold too, and both caught
  -- this test out before the markers were made precise.
  assertEqual "leaf headers" leafCount (countOccurrences "<FONT POINT-SIZE=\"9\">leaf " dot)
  -- one highlight per separator, and the separator count is one per boundary
  assertEqual
    "highlighted separators"
    (leafCount - 1)
    (countOccurrences "#cfe3f7\"><B>" dot)
  assertEqual "separators in the index" (leafCount - 1) (size (BP.indexTree t))
  -- every key and every value is drawn
  forM_ [0 .. n - 1] $ \i -> do
    assertBool ("missing key " ++ show i) (show (ks U.! i) `isInfixOf` dot)
    assertBool ("missing value " ++ show i) (show (vs U.! i) `isInfixOf` dot)

bplusDotTests :: TestTree
bplusDotTests =
  testGroup
    "graphviz, map variant"
    ( [testCase ("li=2 ll=2 n=" ++ show n) (bplusDotCase @2 @2 n) | n <- [0, 1, 2, 5, 14, 33]]
        ++ [testCase ("li=16 ll=4 n=" ++ show n) (bplusDotCase @16 @4 n) | n <- [7, 64, 300]]
    )

-- | A NaN query has no position in a sorted order, so which one comes back is
-- unspecified -- but it must be in range and must not throw.
--
-- Before the clamp in @descend@ this crashed. No comparison against NaN is
-- true, so the leaf rank ran past the last real key into the sentinel padding,
-- and 'BP.lookup' then asked for a slice of negative length. Build rejects NaN
-- keys; nothing validates a NaN query.
nanCase ::
  forall k li ll.
  (LineWidth li, LineWidth ll, TestKey k, BatchKey k, Fractional k, Show k) =>
  Int ->
  Assertion
nanCase n = do
  let ks = sortedSample @k 42 n
      vs = U.enumFromN (0 :: Int) n
      t = BP.unsafeBuild ks vs :: BP.BPlusTree li ll k Int
      nan = 0 / 0 :: k
      lo = BP.lowerBoundIdx t nan
      hi = BP.upperBoundIdx t nan
  assertBool ("lowerBoundIdx out of range: " ++ show lo ++ " of " ++ show n) (lo >= 0 && lo <= n)
  assertBool ("upperBoundIdx out of range: " ++ show hi ++ " of " ++ show n) (hi >= 0 && hi <= n)
  assertEqual "lookup" U.empty (BP.lookup t nan)
  -- the bulk path must be NaN-safe in the same way
  assertEqual
    "lowerBoundIdxMany with NaN"
    (U.map (BP.lowerBoundIdx t) (U.singleton nan))
    (BP.lowerBoundIdxMany t (U.singleton nan))
  assertEqual
    "upperBoundIdxMany with NaN"
    (U.map (BP.upperBoundIdx t) (U.singleton nan))
    (BP.upperBoundIdxMany t (U.singleton nan))
  assertBool "member" (not (BP.member t nan))
  assertEqual "rangeValues" U.empty (BP.rangeValues t nan nan)
  assertEqual "rangeKeys" U.empty (BP.rangeKeys t nan nan)
  let (rlo, rhi) = BP.rangeIdx t nan nan
  assertBool "rangeIdx ordered and in range" (0 <= rlo && rlo <= rhi && rhi <= n)
  -- and with only one endpoint NaN
  when (n > 0) $ do
    let (alo, ahi) = BP.rangeIdx t (ks U.! 0) nan
        (blo, bhi) = BP.rangeIdx t nan (ks U.! (n - 1))
    assertBool "rangeIdx, NaN upper" (0 <= alo && alo <= ahi && ahi <= n)
    assertBool "rangeIdx, NaN lower" (0 <= blo && blo <= bhi && bhi <= n)

-- | The B-tree is checked for the same postcondition. It has never had the bug
-- -- its two-branch conversion of the node cursor happens to keep the result in
-- range -- but the contract is documented there too, so it is worth pinning.
btreeNanCase :: forall l. LineWidth l => Int -> Assertion
btreeNanCase n = do
  let t = unsafeBuild (sortedSample @Double 42 n) :: BTree l Double
      nan = 0 / 0 :: Double
  assertBool "lowerBoundIdx out of range" (let i = lowerBoundIdx t nan in i >= 0 && i <= n)
  assertBool "upperBoundIdx out of range" (let i = upperBoundIdx t nan in i >= 0 && i <= n)

nanTests :: TestTree
nanTests =
  testGroup
    "NaN queries stay in range"
    ( concat
        [ [testCase ("B+tree Double li=2 ll=2 n=" ++ show n) (nanCase @Double @2 @2 n) | n <- sizes]
        , [testCase ("B+tree Double li=16 ll=16 n=" ++ show n) (nanCase @Double @16 @16 n) | n <- sizes]
        , [testCase ("B+tree Float li=2 ll=2 n=" ++ show n) (nanCase @Float @2 @2 n) | n <- sizes]
        , [testCase ("B+tree Float li=16 ll=64 n=" ++ show n) (nanCase @Float @16 @64 n) | n <- sizes]
        , [testCase ("B-tree Double L=2 n=" ++ show n) (btreeNanCase @2 n) | n <- sizes]
        , [testCase ("B-tree Double L=16 n=" ++ show n) (btreeNanCase @16 n) | n <- sizes]
        ]
    )
  where
    sizes = [0, 1, 2, 3, 15, 64, 300]

bplusGoldenTest :: TestTree
bplusGoldenTest = testCase "golden separators (li=2, ll=2, n=10)" $ do
  let ks = U.fromList [10, 20, 30, 40, 50, 60, 70, 80, 90, 100] :: U.Vector Int32
  case BP.build ks (U.enumFromN (0 :: Int) 10) :: Either BP.BuildError (BP.BPlusTree 2 2 Int32 Int) of
    Left err -> assertFailure (show err)
    Right t -> do
      -- leaves [10,20] [30,40] [50,60] [70,80] [90,100]
      assertEqual "separators" (U.fromList [30, 50, 70, 90]) (toVector (BP.indexTree t))
      assertEqual "height" 3 (BP.height t)
      assertEqual "lookup 60" (U.fromList [5]) (BP.lookup t 60)
      assertEqual "lookup 65" U.empty (BP.lookup t 65)
      assertEqual "range [30,70)" (U.fromList [2, 3, 4, 5]) (BP.rangeValues t 30 70)

bplusRejectionTests :: TestTree
bplusRejectionTests =
  testGroup
    "input validation"
    [ testCase "length mismatch" $
        assertEqual
          ""
          (Left (BP.LengthMismatch 3 2))
          (fmap BP.size (BP.build (U.fromList [1, 2, 3 :: Int32]) (U.fromList [0, 1 :: Int]) :: Either BP.BuildError (BP.BPlusTree 16 16 Int32 Int)))
    , testCase "unsorted keys" $
        assertEqual
          ""
          (Left BP.KeysNotSorted)
          (fmap BP.size (BP.build (U.fromList [3, 1, 2 :: Int32]) (U.fromList [0, 1, 2 :: Int]) :: Either BP.BuildError (BP.BPlusTree 16 16 Int32 Int)))
    , testCase "sentinel key" $
        assertEqual
          ""
          (Left BP.KeyNotBelowSentinel)
          (fmap BP.size (BP.build (U.fromList [1, maxBound :: Int32]) (U.fromList [0, 1 :: Int]) :: Either BP.BuildError (BP.BPlusTree 16 16 Int32 Int)))
    , testCase "empty is fine" $ do
        let t = BP.unsafeBuild U.empty U.empty :: BP.BPlusTree 16 16 Int32 Int
        assertEqual "size" 0 (BP.size t)
        assertEqual "lower" 0 (BP.lowerBoundIdx t 5)
        assertEqual "upper" 0 (BP.upperBoundIdx t 5)
        assertEqual "lookup" U.empty (BP.lookup t 5)
        assertBool "member" (not (BP.member t 5))
    ]

-- ---------------------------------------------------------------------------
-- Calibration
-- ---------------------------------------------------------------------------

-- | The timings are hardware-dependent, so the tests pin the parts that are
-- not: that the reified width round-trips, that every supported width is
-- measured with the right height, that the chosen width is one you can actually
-- use, and that degenerate inputs do not throw.
calibrateTests :: TestTree
calibrateTests =
  testGroup
    "calibration"
    [ testCase "withWidth round-trips every supported width" $
        forM_ [2, 4, 8, 16, 32, 64] $ \w ->
          assertEqual ("width " ++ show w) w (withWidth w (\(_ :: Proxy l) -> lineWidth @l))
    , testCase "withWidth falls back to 16 for anything else" $
        forM_ [0, 1, 3, 7, 128, -4] $ \w ->
          assertEqual ("width " ++ show w) 16 (withWidth w (\(_ :: Proxy l) -> lineWidth @l))
    , testCase "calibrateBTreeBatch measures every width" (shapeOk =<< calibrateBTreeBatch cfg keys200 qs50)
    , testCase "calibrateBTreeScalar measures every width" (shapeOk =<< calibrateBTreeScalar cfg keys200 qs50)
    , testCase "no queries does not throw" $ do
        c <- calibrateBTreeBatch cfg keys200 U.empty
        shapeOk c
        assertBool "all zero" (all ((== 0) . mNanosPerQuery) (calMeasurements c))
    , testCase "fewer queries than repetitions does not throw" $
        shapeOk =<< calibrateBTreeBatch cfg keys200 (U.take 2 qs50)
    , testCase "no keys does not throw" $ shapeOk =<< calibrateBTreeBatch cfg U.empty qs50
    , -- The tie-break is a pure function of the measurements, so it is tested as
      -- one. Asserting it through a timing run made the outcome depend on
      -- whether sub-microsecond passes happened to round to zero, which is how
      -- this test first came to fail for reasons unrelated to the policy.
      testCase "ties break toward the narrower width" $ do
        let m :: Int -> Double -> Measurement Int
            m w ns = Measurement w 5 ns 1
            wide = defaultConfig {cfgTolerance = 1e9}
        assertEqual
          "everything ties, narrowest wins"
          2
          (calBest (chooseBy wide id [m 2 100, m 4 50, m 8 60]))
        assertEqual
          "outside the band loses even though it is narrower"
          4
          (calBest (chooseBy defaultConfig id [m 2 100, m 4 50, m 8 51]))
        assertEqual
          "inside a 5% band, the narrower of two near-equals wins"
          4
          (calBest (chooseBy defaultConfig id [m 4 51, m 8 50]))
    , testCase "the B+tree tie-break prefers a smaller index" $ do
        let m :: (Int, Int) -> Double -> Measurement (Int, Int)
            m ws ns = Measurement ws 5 ns 1
            prefer (li, ll) = (negate ll, li)
        -- Equal speeds: the wider leaf stores fewer separators, so it wins even
        -- though the narrower index node would be preferred on its own axis.
        assertEqual
          ""
          (2, 64)
          (calBest (chooseBy defaultConfig prefer [m (2, 2) 50, m (2, 64) 50, m (4, 64) 50]))
    , testCase "a run too short to time reports itself unresolvable" $ do
        -- 200 keys and 50 queries finish inside the clock granularity, so the
        -- figures are quantisation. The point is that it says so rather than
        -- returning a width picked by noise.
        c <- calibrateBTreeBatch cfg keys200 qs50
        assertBool "should not claim to be resolvable" (not (calResolvable c))
    , testCase "calibrateBPlusBulk measures the whole grid" $ do
        c <- calibrateBPlusBulk cfg keys200 vals200 qs50
        assertEqual
          "every (index, leaf) pair"
          [(li, ll) | li <- widths, ll <- widths]
          (map mWidths (calMeasurements c))
        assertBool
          ("chosen pair not usable: " ++ show (calBest c))
          (fst (calBest c) `elem` widths && snd (calBest c) `elem` widths)
    , testCase "calibrateBPlusBulk survives degenerate inputs" $ do
        forM_ [U.empty, U.take 2 qs50] $ \q -> do
          c <- calibrateBPlusBulk cfg keys200 vals200 q
          assertEqual "grid size" 36 (length (calMeasurements c))
        c0 <- calibrateBPlusBulk cfg U.empty (U.empty :: U.Vector Int) qs50
        assertEqual "grid size" 36 (length (calMeasurements c0))
    , testCase "calibrateBPlusScalar measures the whole grid" $ do
        c <- calibrateBPlusScalar cfg keys200 vals200 qs50
        assertEqual
          "every (index, leaf) pair"
          [(li, ll) | li <- widths, ll <- widths]
          (map mWidths (calMeasurements c))
        assertBool
          ("chosen pair not usable: " ++ show (calBest c))
          (fst (calBest c) `elem` widths && snd (calBest c) `elem` widths)
        forM_ (calMeasurements c) $ \m ->
          assertBool ("height " ++ show (mWidths m)) (mHeight m >= 2)

    , testCase "calibrateBPlusScalar survives degenerate inputs" $ do
        forM_ [U.empty, U.take 2 qs50] $ \q -> do
          c <- calibrateBPlusScalar cfg keys200 vals200 q
          assertEqual "grid size" 36 (length (calMeasurements c))
        c0 <- calibrateBPlusScalar cfg U.empty (U.empty :: U.Vector Int) qs50
        assertEqual "grid size" 36 (length (calMeasurements c0))
    , testCase "a run long enough to time reports itself resolvable" $ do
        let bigKeys = sortedSample @Int32 42 200000
            bigQs = U.fromList (probeKeys @Int32 20000 bigKeys)
        c <- calibrateBTreeBatch defaultConfig bigKeys bigQs
        assertBool "should be resolvable" (calResolvable c)
        assertBool "chosen width usable" (calBest c `elem` [2, 4, 8, 16, 32, 64])
    ]
  where
    cfg = defaultConfig {cfgReps = 3}
    widths = [2, 4, 8, 16, 32, 64]
    keys200 = sortedSample @Int32 42 200
    vals200 = U.enumFromN (0 :: Int) 200
    qs50 = U.fromList (probeKeys @Int32 50 keys200)

    shapeOk c = do
      assertEqual "widths measured" [2, 4, 8, 16, 32, 64] (map mWidths (calMeasurements c))
      assertBool
        ("chosen width not usable: " ++ show (calBest c))
        (calBest c `elem` [2, 4, 8, 16, 32, 64])
      forM_ (calMeasurements c) $ \m ->
        assertBool
          ("height for L=" ++ show (mWidths m) ++ " is " ++ show (mHeight m))
          (mHeight m >= 1)
      assertBool "some measurement is non-negative" (all ((>= 0) . mNanosPerQuery) (calMeasurements c))

-- ---------------------------------------------------------------------------
-- Serialization
-- ---------------------------------------------------------------------------

withTempPath :: (FilePath -> IO a) -> IO a
withTempPath act = do
  dir <- getTemporaryDirectory
  (path, h) <- openBinaryTempFile dir "s-tree-test.st"
  hClose h
  act path `finally` ignoringIOErrors (removeFile path)
  where
    ignoringIOErrors io = io `catch` \e -> let _ = (e :: IOException) in return ()

-- | Round trip through a file, and check that what comes back is the same tree
-- in every field — which also pins the closed form for @exceeding_nodes@ that
-- the loader uses against the loop the builder uses.
roundTripCase :: forall l a. (LineWidth l, SerializableKey a, TestKey a, Show a) => Int -> Assertion
roundTripCase n = withTempPath $ \path -> do
  let v = sortedSample @a 42 n
  t <- case build v :: Either BuildError (BTree l a) of
    Left err -> assertFailure ("build failed: " ++ show err)
    Right t -> return t
  writeSTree path t
  loaded <- readSTree path
  case loaded of
    Left err -> assertFailure ("read failed: " ++ show err)
    Right t' -> do
      assertBool "loaded tree differs" (t == t')
      -- and it actually answers queries
      forM_ (probeKeys @a 50 v) $ \k ->
        assertEqual ("lowerBoundIdx " ++ show k) (lowerBoundRef v k) (lowerBoundIdx (t' :: BTree l a) k)

-- | Write a tree, then hand the file to a test after mangling it.
withMangled ::
  forall l a.
  (LineWidth l, SerializableKey a, TestKey a) =>
  Int ->
  (FilePath -> IO ()) ->
  (FilePath -> IO ()) ->
  IO ()
withMangled n mangle act = withTempPath $ \path -> do
  let t = unsafeBuild (sortedSample @a 7 n) :: BTree l a
  writeSTree path t
  mangle path
  act path

patchByte :: FilePath -> Int -> (Word8 -> Word8) -> IO ()
patchByte path off f = do
  bs <- B.readFile path
  let (before, rest) = B.splitAt off bs
  B.writeFile path (B.concat [before, B.singleton (f (B.head rest)), B.drop 1 rest])

chopBytes :: FilePath -> Int -> IO ()
chopBytes path k = do
  bs <- B.readFile path
  B.writeFile path (B.take (B.length bs - k) bs)

appendBytes :: FilePath -> Int -> IO ()
appendBytes path k = do
  bs <- B.readFile path
  B.writeFile path (bs <> B.replicate k 0xAA)

serializeTests :: TestTree
serializeTests =
  testGroup
    "serialization"
    [ testGroup
        "round trip, key types (L=16)"
        [ testCase (nm ++ " n=" ++ show n) c
        | (nm, cases) <-
            [ (keyName @Int32, \n -> roundTripCase @16 @Int32 n)
            , (keyName @Int64, \n -> roundTripCase @16 @Int64 n)
            , (keyName @Word32, \n -> roundTripCase @16 @Word32 n)
            , (keyName @Word64, \n -> roundTripCase @16 @Word64 n)
            , (keyName @Float, \n -> roundTripCase @16 @Float n)
            , (keyName @Double, \n -> roundTripCase @16 @Double n)
            ]
        , n <- [0, 1, 17, 300, 5000]
        , let c = cases n
        ]
    , testGroup
        "round trip, node widths (Int32)"
        [ testCase ("L=" ++ show w ++ " n=" ++ show n) c
        | (w, cases) <-
            [ (2 :: Int, \n -> roundTripCase @2 @Int32 n)
            , (4, \n -> roundTripCase @4 @Int32 n)
            , (8, \n -> roundTripCase @8 @Int32 n)
            , (16, \n -> roundTripCase @16 @Int32 n)
            , (32, \n -> roundTripCase @32 @Int32 n)
            , (64, \n -> roundTripCase @64 @Int32 n)
            ]
        , n <- [0, 1, 17, 300, 5000]
        , let c = cases n
        ]
    , -- 700k Int32 keys is 2.8 MB, i.e. three passes of the 1 MiB staging
      -- buffer, so the chunk loop's boundaries actually get exercised.
      testCase "round trip across several chunks (n=700000)" $
        roundTripCase @16 @Int32 700000
    , testCase "keyBytes agrees with Storable sizeOf" $ do
        assertEqual "Int32" (sizeOf (undefined :: Int32)) (keyBytes @Int32)
        assertEqual "Int64" (sizeOf (undefined :: Int64)) (keyBytes @Int64)
        assertEqual "Word32" (sizeOf (undefined :: Word32)) (keyBytes @Word32)
        assertEqual "Word64" (sizeOf (undefined :: Word64)) (keyBytes @Word64)
        assertEqual "Float" (sizeOf (undefined :: Float)) (keyBytes @Float)
        assertEqual "Double" (sizeOf (undefined :: Double)) (keyBytes @Double)
        assertEqual "Int" (sizeOf (undefined :: Int)) (keyBytes @Int)
        assertEqual "Word" (sizeOf (undefined :: Word)) (keyBytes @Word)
    , testGroup
        "rejections"
        [ testCase "bad magic" $
            withMangled @16 @Int32 100 (\p -> patchByte p 0 (+ 1)) $ \p -> do
              r <- readSTree p :: IO (Either SerializeError (BTree 16 Int32))
              assertEqual "" (Left BadMagic) (void' r)
        , testCase "unsupported version" $
            withMangled @16 @Int32 100 (\p -> patchByte p 8 (+ 9)) $ \p -> do
              r <- readSTree p :: IO (Either SerializeError (BTree 16 Int32))
              assertEqual "" (Left (UnsupportedVersion (formatVersion + 9))) (void' r)
        , testCase "wrong key type" $
            withMangled @16 @Int32 100 (const (return ())) $ \p -> do
              r <- readSTree p :: IO (Either SerializeError (BTree 16 Int64))
              assertEqual "" (Left (KeyTypeMismatch (keyTag @Int64) (keyTag @Int32))) (void' r)
        , testCase "wrong node width" $
            withMangled @16 @Int32 100 (const (return ())) $ \p -> do
              r <- readSTree p :: IO (Either SerializeError (BTree 8 Int32))
              assertEqual "" (Left (LineWidthMismatch 8 16)) (void' r)
        , testCase "opposite endianness" $
            -- Flip the flag bit rather than setting it, so the file always
            -- claims the byte order this host does not have.
            withMangled @16 @Int32 100 (\p -> patchByte p 12 (`xor` 1)) $ \p -> do
              r <- readSTree p :: IO (Either SerializeError (BTree 16 Int32))
              assertEqual "" (Left EndiannessMismatch) (void' r)
        , testCase "absurd key count" $
            withMangled @16 @Int32 100 (\p -> patchByte p 22 (const 0xFF)) $ \p -> do
              r <- readSTree p :: IO (Either SerializeError (BTree 16 Int32))
              assertBool (show (void' r)) (isInconsistent (void' r))
        , testCase "truncated file" $
            withMangled @16 @Int32 100 (`chopBytes` 12) $ \p -> do
              r <- readSTree p :: IO (Either SerializeError (BTree 16 Int32))
              assertBool (show (void' r)) (isShortFile (void' r))
        , testCase "header alone is too short" $
            withMangled @16 @Int32 100 (`chopBytes` 500) $ \p -> do
              r <- readSTree p :: IO (Either SerializeError (BTree 16 Int32))
              assertBool (show (void' r)) (isShortFile (void' r))
        , testCase "trailing bytes" $
            withMangled @16 @Int32 100 (`appendBytes` 5) $ \p -> do
              r <- readSTree p :: IO (Either SerializeError (BTree 16 Int32))
              assertEqual "" (Left (UnexpectedTrailingBytes 5)) (void' r)
        , testCase "payload ends early when the budget lies" $
            -- readSTree cannot hit this, because it derives the budget from the
            -- file size; a caller-supplied budget can.
            withMangled @16 @Int32 100 (`chopBytes` 12) $ \p ->
              withBinaryFile p ReadMode $ \h -> do
                r <- hGetSTree h (10 * 1024) :: IO (Either SerializeError (BTree 16 Int32))
                assertBool (show (void' r)) (isTruncated (void' r))
        ]
    , testCase "hGetSTree reads a tree embedded in a larger file" $
        withTempPath $ \inner -> withTempPath $ \outer -> do
          let t = unsafeBuild (sortedSample @Int32 3 500) :: BTree 16 Int32
          writeSTree inner t
          body <- B.readFile inner
          B.writeFile outer (B.replicate 16 0x5A <> body <> B.replicate 9 0x5A)
          withBinaryFile outer ReadMode $ \h -> do
            total <- hFileSize h
            hSeek h AbsoluteSeek 16
            r <- hGetSTree h (total - 16)
            case r of
              Left err -> assertFailure (show err)
              Right t' -> assertBool "loaded tree differs" (t == t')
    ]
  where
    -- Errors compare more readably than trees do.
    void' = either Left (const (Right ()))
    isInconsistent (Left (InconsistentHeader _)) = True
    isInconsistent _ = False
    isShortFile (Left (ShortFile _ _)) = True
    isShortFile _ = False
    isTruncated (Left (Truncated _)) = True
    isTruncated _ = False

-- | The example from the README.
exampleTest :: TestTree
exampleTest = testCase "README example" $
  case buildFromList [-3, 2, 4, 11, 35, 60] :: Either BuildError (BTree 16 Int32) of
    Left err -> assertFailure (show err)
    Right t -> do
      assertEqual "lower 11" 3 (lowerBoundIdx t 11)
      assertEqual "lower 12" 4 (lowerBoundIdx t 12)
      assertEqual "t ! 3" 11 (t ! 3)

-- | The permutation itself, not just its observable behaviour. Small enough to
-- work out by hand from the layout rules, which is what makes these golden
-- values evidence rather than merely a record of what the code happens to do.
layoutTests :: TestTree
layoutTests =
  testGroup
    "golden layouts (L=2, B=3)"
    [ -- n = B^2-1: a complete tree of 2 levels. Level 0 holds the separators
      -- 3 and 6 at offset B^0-1 = 0; level 1 holds the three leaves at offset
      -- B^1-1 = 2.
      testCase "n=8 (complete)" $
        assertLayout [1 .. 8] [3, 6, 1, 2, 4, 5, 7, 8]
    , -- n = 10: 3 levels, the deepest one partial with e = 10-8 = 2 keys, which
      -- live contiguously at the end. The root's separators are 5 and 8.
      testCase "n=10 (partial deepest level)" $
        assertLayout [1 .. 10] [5, 8, 3, 4, 6, 7, 9, 10, 1, 2]
    , -- Odd n, so one slot of sentinel padding closes the last node. Root
      -- separators 3 and 5; leaves [1,2] and [4, pad].
      testCase "n=5 (padded)" $
        assertLayout [1 .. 5] [3, 5, 1, 2, 4, maxBound]
    ]
  where
    assertLayout input expected =
      case buildFromList input :: Either BuildError (BTree 2 Int32) of
        Left err -> assertFailure (show err)
        Right t -> assertEqual "" (U.fromList expected) (layout t)

main :: IO ()
main =
  defaultMain $
    testGroup
      "static B-tree"
      [ exampleTest
      , layoutTests
      , rejectionTests
      , batchTests
      , dotTests
      , bplusTests
      , calibrateTests
      , serializeTests
      , testGroup
          "against the oracle"
          [ keyTypeTests @Int32
          , keyTypeTests @Int64
          , keyTypeTests @Word32
          , keyTypeTests @Word64
          , keyTypeTests @Float
          , keyTypeTests @Double
          ]
      , testGroup
          "properties"
          [ QC.testProperty "blockRank matches the reference" prop_blockRank
          , QC.testProperty "bounds match the oracle (L=16, Int64)" prop_bounds
          , QC.testProperty "bounds match the oracle (L=4, Double)" prop_boundsNarrow
          , QC.testProperty "bounds are valid slice indices" prop_inRange
          ]
      , testGroup
          "large inputs"
          [ testCase "L=16, Int32, n=1000000" (buildCase @16 @Int32 1000000)
          , testCase "L=64, Double, n=1000000" (buildCase @64 @Double 1000000)
          ]
      ]
