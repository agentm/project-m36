{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Choosing the node width for the machine you are actually running on.
--
-- The best width is not a property of the structure; it is a property of the
-- hardware, the key type, the number of keys and which query path you use. On
-- one machine the vectorised path measured 4x faster at @L=16@ than at @L=4@,
-- while the scalar path was flat from @L=2@ to @L=16@ and only then fell off —
-- and the two paths disagree about why, so neither answer transfers.
-- 'calibrateBTreeScalar' and 'calibrateBTreeBatch' build a tree at every supported width
-- over your own keys and time your own queries against them.
--
-- > cal <- calibrateBTreeBatch defaultConfig sortedKeys sampleQueries
-- > print (calBest cal, calMeasurements cal)
-- >
-- > let idxs = withWidth (calBest cal) $ \(_ :: Proxy l) ->
-- >              lowerBoundIdxMany (unsafeBuild sortedKeys :: BTree l Int32) queries
--
-- == What it costs
--
-- One tree per width — six of them — plus 'cfgReps' passes over the queries at
-- each. At a million 32-bit keys that is a fraction of a second; at a hundred
-- million it is minutes, because building dominates. Calibrating on a subsample
-- is tempting and misleading: the optimum moves with the number of keys
-- precisely because it is decided by how much of the tree stays in cache, which
-- is what subsampling changes.
--
-- == A caveat about specialisation
--
-- Both functions are @INLINABLE@ so they specialise to your key type at the call
-- site. That is not a micro-optimisation. Unspecialised, the timed loops
-- dispatch every comparison through a dictionary and box a key per array access,
-- which elsewhere in this library measured 38x slower than the specialised
-- equivalent — and calibrating code 38x slower than the code you will run can
-- easily prefer a different width. If you wrap either function in one of your
-- own that is polymorphic in the key type, mark that @INLINABLE@ as well.
module Data.STree.Calibrate
  ( -- * Calibrating
    calibrateBTreeScalar
  , calibrateBTreeBatch
  , calibrateBPlusTreeScalar
  , calibrateBPlusTreeBulk
  , Config (..)
  , defaultConfig

    -- * Results
  , Calibration (..)
  , Measurement (..)
  , chooseBy

    -- * Using the answer
  , withWidth
  , withWidths
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import qualified Data.Vector.Unboxed as U
import GHC.Clock (getMonotonicTime)

import Data.STree.BTree
import Data.STree.Batch (BatchKey, lowerBoundIdxMany)
import qualified Data.STree.Batch as Batch
import qualified Data.STree.BPlusTree as BP
import Data.STree.Key (Key)

-- | How thoroughly to measure.
data Config = Config
  { cfgReps :: !Int
  -- ^ Timed passes per width; the fastest is kept, since a wall time can only
  -- be inflated by interference, never deflated by it.
  , cfgTolerance :: !Double
  -- ^ Widths within this fraction of the fastest count as tied, and the
  -- narrowest of those is chosen. The scalar path is genuinely flat over a wide
  -- range, so without a tie band the choice there is decided by noise; breaking
  -- ties toward narrow also wastes fewer keys on node padding.
  }
  deriving (Eq, Show)

-- | Five passes and a 5% tie band.
defaultConfig :: Config
defaultConfig = Config {cfgReps = 5, cfgTolerance = 0.05}

-- | One configuration's result. @w@ describes the widths: 'Int' for a B-tree,
-- @(index, leaf)@ for a B+tree.
data Measurement w = Measurement
  { mWidths :: !w
  , mHeight :: !Int
  -- ^ Levels the tree needs at this width, which is what the width is really
  -- being traded against.
  , mNanosPerQuery :: !Double
  , mElapsed :: !Double
  -- ^ Wall seconds of the fastest pass, before dividing by the query count.
  -- Exposed because it is what decides whether the figure means anything: a
  -- pass shorter than the clock's granularity reports zero.
  }
  deriving (Eq, Show)

data Calibration w = Calibration
  { calBest :: !w
  -- ^ The width to use: the fastest measured, then narrowed as far as the tie
  -- band allows.
  , calMeasurements :: ![Measurement w]
  -- ^ Every width, narrowest first. Worth logging: a small spread means the
  -- choice did not matter, a large one means the machine mattered a great deal.
  , calResolvable :: !Bool
  -- ^ Whether the passes were long enough to time. 'False' means at least one
  -- finished inside the clock's granularity, so its figure is quantisation
  -- rather than measurement and 'calBest' is not trustworthy — pass more
  -- queries, or raise 'cfgReps'. Checking this is the difference between a
  -- calibrated width and an arbitrary one.
  }
  deriving (Eq, Show)

-- | Reify a width from 'calBest' back to a type. An unrecognised value falls
-- back to 16.
withWidth :: Int -> (forall l. LineWidth l => Proxy l -> r) -> r
withWidth w k = case w of
  2 -> k (Proxy @2)
  4 -> k (Proxy @4)
  8 -> k (Proxy @8)
  16 -> k (Proxy @16)
  32 -> k (Proxy @32)
  64 -> k (Proxy @64)
  _ -> k (Proxy @16)

-- | Reify a pair of widths, for 'calibrateBPlusTreeScalar'. Nested 'withWidth', so the
-- six cases are not written out thirty-six times.
withWidths ::
  Int ->
  Int ->
  (forall li ll. (LineWidth li, LineWidth ll) => Proxy li -> Proxy ll -> r) ->
  r
withWidths i l k = withWidth i (\pi_ -> withWidth l (\pl -> k pi_ pl))

-- | Time the scalar 'lowerBoundIdx' at every supported width.
--
-- Keys must be sorted, as for 'unsafeBuild'. The queries should look like the
-- ones you will actually make: their distribution decides how much of the tree
-- stays in cache, and so decides the answer.
calibrateBTreeScalar ::
  forall a.
  Key a =>
  Config ->
  U.Vector a ->
  U.Vector a ->
  IO (Calibration Int)
calibrateBTreeScalar cfg keys qs = do
  ms <-
    sequence
      [ go 2 (logSize @2 n) (work (unsafeBuild keys :: BTree 2 a))
      , go 4 (logSize @4 n) (work (unsafeBuild keys :: BTree 4 a))
      , go 8 (logSize @8 n) (work (unsafeBuild keys :: BTree 8 a))
      , go 16 (logSize @16 n) (work (unsafeBuild keys :: BTree 16 a))
      , go 32 (logSize @32 n) (work (unsafeBuild keys :: BTree 32 a))
      , go 64 (logSize @64 n) (work (unsafeBuild keys :: BTree 64 a))
      ]
  return (chooseBy cfg id ms)
  where
    n = U.length keys
    go = timeWidth cfg qs
    work t = U.foldl' (\ !acc k -> acc + lowerBoundIdx t k) 0
{-# INLINABLE calibrateBTreeScalar #-}

-- | Time the vectorised 'lowerBoundIdxMany' at every supported width.
--
-- This is the path worth tuning: it is where the width makes the largest
-- difference, and where the intuition from the scalar path is wrong.
calibrateBTreeBatch ::
  forall a.
  BatchKey a =>
  Config ->
  U.Vector a ->
  U.Vector a ->
  IO (Calibration Int)
calibrateBTreeBatch cfg keys qs = do
  ms <-
    sequence
      [ go 2 (logSize @2 n) (work (unsafeBuild keys :: BTree 2 a))
      , go 4 (logSize @4 n) (work (unsafeBuild keys :: BTree 4 a))
      , go 8 (logSize @8 n) (work (unsafeBuild keys :: BTree 8 a))
      , go 16 (logSize @16 n) (work (unsafeBuild keys :: BTree 16 a))
      , go 32 (logSize @32 n) (work (unsafeBuild keys :: BTree 32 a))
      , go 64 (logSize @64 n) (work (unsafeBuild keys :: BTree 64 a))
      ]
  return (chooseBy cfg id ms)
  where
    n = U.length keys
    go = timeWidth cfg qs
    work t = U.sum . lowerBoundIdxMany t
{-# INLINABLE calibrateBTreeBatch #-}


-- | Time the B+tree's position query at every combination of index and leaf
-- width.
--
-- The full 6x6 grid, deliberately. Coordinate descent -- sweep the leaf width,
-- then the index width at the winner -- is six times cheaper and was tried
-- first: on the machine this was developed against it converged to a
-- configuration 12 per cent slower than the true optimum, because the surface is
-- not monotone in the leaf width (83.8, 97.7, 87.2 and 100.8 ns at leaf widths
-- 8, 4, 2 and 16, with the index width fixed at 2). A local search on a surface
-- like that finds a local answer.
--
-- Thirty-six trees is less alarming than it sounds: a B+tree build needs no
-- permutation of the leaves, so it costs roughly a tenth of a B-tree build.
-- Queries dominate the run, so shorten the query vector rather than the grid if
-- this is too slow.
--
-- There is no batch counterpart. "Data.STree.Batch" does not cover the B+tree,
-- so the scalar descent is the only path there is to tune.
calibrateBPlusTreeScalar ::
  forall a v.
  Key a =>
  Config ->
  -- | keys, sorted
  U.Vector a ->
  -- | values, positionally paired with the keys
  U.Vector v ->
  -- | queries to time against
  U.Vector a ->
  IO (Calibration (Int, Int))
calibrateBPlusTreeScalar cfg keys vals qs = do
  ms <-
    sequence
      [go (2, 2) (bpHeight 2 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 2 a v))
      , go (2, 4) (bpHeight 2 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 4 a v))
      , go (2, 8) (bpHeight 2 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 8 a v))
      , go (2, 16) (bpHeight 2 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 16 a v))
      , go (2, 32) (bpHeight 2 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 32 a v))
      , go (2, 64) (bpHeight 2 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 64 a v))
      , go (4, 2) (bpHeight 4 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 2 a v))
      , go (4, 4) (bpHeight 4 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 4 a v))
      , go (4, 8) (bpHeight 4 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 8 a v))
      , go (4, 16) (bpHeight 4 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 16 a v))
      , go (4, 32) (bpHeight 4 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 32 a v))
      , go (4, 64) (bpHeight 4 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 64 a v))
      , go (8, 2) (bpHeight 8 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 2 a v))
      , go (8, 4) (bpHeight 8 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 4 a v))
      , go (8, 8) (bpHeight 8 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 8 a v))
      , go (8, 16) (bpHeight 8 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 16 a v))
      , go (8, 32) (bpHeight 8 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 32 a v))
      , go (8, 64) (bpHeight 8 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 64 a v))
      , go (16, 2) (bpHeight 16 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 2 a v))
      , go (16, 4) (bpHeight 16 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 4 a v))
      , go (16, 8) (bpHeight 16 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 8 a v))
      , go (16, 16) (bpHeight 16 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 16 a v))
      , go (16, 32) (bpHeight 16 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 32 a v))
      , go (16, 64) (bpHeight 16 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 64 a v))
      , go (32, 2) (bpHeight 32 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 2 a v))
      , go (32, 4) (bpHeight 32 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 4 a v))
      , go (32, 8) (bpHeight 32 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 8 a v))
      , go (32, 16) (bpHeight 32 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 16 a v))
      , go (32, 32) (bpHeight 32 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 32 a v))
      , go (32, 64) (bpHeight 32 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 64 a v))
      , go (64, 2) (bpHeight 64 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 2 a v))
      , go (64, 4) (bpHeight 64 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 4 a v))
      , go (64, 8) (bpHeight 64 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 8 a v))
      , go (64, 16) (bpHeight 64 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 16 a v))
      , go (64, 32) (bpHeight 64 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 32 a v))
      , go (64, 64) (bpHeight 64 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 64 a v))
      ]
  return (chooseBy cfg preferSmallIndex ms)
  where
    n = U.length keys
    go = timeWidth cfg qs
    work t = U.foldl' (\ !acc k -> acc + BP.lowerBoundIdx t k) 0

    -- One level of leaves beneath an index holding one separator per leaf
    -- boundary.
    bpHeight li ll =
      let leaves = max 1 ((n + ll - 1) `div` ll)
       in heightOfIndex li (leaves - 1) + 1
    heightOfIndex li m = withWidth li (\(_ :: Proxy i) -> logSize @i m)

    -- Among equally fast configurations, prefer the one that stores less. A
    -- wider leaf means fewer separators and so a smaller index -- n/ll of them --
    -- which dominates the at most ll-1 keys of leaf padding it costs. This is
    -- the opposite of the B-tree rule, where narrower simply wastes less; here
    -- the leaf width decides how large a second structure has to be.
    preferSmallIndex (li, ll) = (negate ll, li)
{-# INLINABLE calibrateBPlusTreeScalar #-}
{-# SPECIALIZE calibrateBPlusTreeScalar :: Config -> U.Vector Int32 -> U.Vector v -> U.Vector Int32 -> IO (Calibration (Int, Int)) #-}
{-# SPECIALIZE calibrateBPlusTreeScalar :: Config -> U.Vector Int64 -> U.Vector v -> U.Vector Int64 -> IO (Calibration (Int, Int)) #-}
{-# SPECIALIZE calibrateBPlusTreeScalar :: Config -> U.Vector Double -> U.Vector v -> U.Vector Double -> IO (Calibration (Int, Int)) #-}


-- | Time the B+tree's /bulk/ position query at every combination of index and
-- leaf width.
--
-- A separate function from 'calibrateBPlusScalar' because the two paths want
-- different shapes and the scalar answer is actively wrong for the bulk one. The
-- bulk descent vectorises the index step, so a wider index wins there where a
-- narrow one wins on the scalar path; on the machine this was developed against,
-- using the scalar optimum for bulk queries cost 2.4x.
--
-- Requires 'BatchKey', so @Int@ and @Word@ keys are out -- their width is
-- platform-dependent and the vectorised kernel is selected by width.
calibrateBPlusTreeBulk ::
  forall a v.
  Batch.BatchKey a =>
  Config ->
  -- | keys, sorted
  U.Vector a ->
  -- | values, positionally paired with the keys
  U.Vector v ->
  -- | queries to time against
  U.Vector a ->
  IO (Calibration (Int, Int))
calibrateBPlusTreeBulk cfg keys vals qs = do
  ms <-
    sequence
      [go (2, 2) (bpHeight 2 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 2 a v))
      , go (2, 4) (bpHeight 2 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 4 a v))
      , go (2, 8) (bpHeight 2 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 8 a v))
      , go (2, 16) (bpHeight 2 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 16 a v))
      , go (2, 32) (bpHeight 2 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 32 a v))
      , go (2, 64) (bpHeight 2 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 2 64 a v))
      , go (4, 2) (bpHeight 4 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 2 a v))
      , go (4, 4) (bpHeight 4 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 4 a v))
      , go (4, 8) (bpHeight 4 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 8 a v))
      , go (4, 16) (bpHeight 4 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 16 a v))
      , go (4, 32) (bpHeight 4 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 32 a v))
      , go (4, 64) (bpHeight 4 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 4 64 a v))
      , go (8, 2) (bpHeight 8 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 2 a v))
      , go (8, 4) (bpHeight 8 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 4 a v))
      , go (8, 8) (bpHeight 8 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 8 a v))
      , go (8, 16) (bpHeight 8 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 16 a v))
      , go (8, 32) (bpHeight 8 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 32 a v))
      , go (8, 64) (bpHeight 8 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 8 64 a v))
      , go (16, 2) (bpHeight 16 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 2 a v))
      , go (16, 4) (bpHeight 16 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 4 a v))
      , go (16, 8) (bpHeight 16 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 8 a v))
      , go (16, 16) (bpHeight 16 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 16 a v))
      , go (16, 32) (bpHeight 16 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 32 a v))
      , go (16, 64) (bpHeight 16 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 16 64 a v))
      , go (32, 2) (bpHeight 32 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 2 a v))
      , go (32, 4) (bpHeight 32 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 4 a v))
      , go (32, 8) (bpHeight 32 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 8 a v))
      , go (32, 16) (bpHeight 32 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 16 a v))
      , go (32, 32) (bpHeight 32 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 32 a v))
      , go (32, 64) (bpHeight 32 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 32 64 a v))
      , go (64, 2) (bpHeight 64 2) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 2 a v))
      , go (64, 4) (bpHeight 64 4) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 4 a v))
      , go (64, 8) (bpHeight 64 8) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 8 a v))
      , go (64, 16) (bpHeight 64 16) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 16 a v))
      , go (64, 32) (bpHeight 64 32) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 32 a v))
      , go (64, 64) (bpHeight 64 64) (work (BP.unsafeBuild keys vals :: BP.BPlusTree 64 64 a v))
      ]
  return (chooseBy cfg preferSmallIndex ms)
  where
    n = U.length keys
    go = timeWidth cfg qs
    work t = U.sum . BP.lowerBoundIdxMany t

    bpHeight li ll =
      let leaves = max 1 ((n + ll - 1) `div` ll)
       in heightOfIndex li (leaves - 1) + 1
    heightOfIndex li m = withWidth li (\(_ :: Proxy i) -> logSize @i m)

    -- The same preference as the scalar version, and better justified here: the
    -- bulk path bisects the leaf, so its width barely affects speed and is
    -- almost purely a memory dial. Among equals, take the smaller index.
    preferSmallIndex (li, ll) = (negate ll, li)
{-# INLINABLE calibrateBPlusTreeBulk #-}

-- INLINABLE permits specialisation but does not force it, and a thirty-six
-- branch function is large enough that GHC might decline, so these are insurance
-- against the failure this module's header warns about.
--
-- Measured honestly: adding them changed nothing. They were added while chasing a
-- disagreement between this function and a benchmark over the best leaf width,
-- and the cause turned out to be query locality rather than specialisation --
-- the benchmark repeated a thousand queries where this function was given a
-- hundred thousand distinct ones. They are kept because the risk they guard
-- against is real, not because they were shown to help.
{-# SPECIALIZE calibrateBPlusTreeBulk :: Config -> U.Vector Int32 -> U.Vector v -> U.Vector Int32 -> IO (Calibration (Int, Int)) #-}
{-# SPECIALIZE calibrateBPlusTreeBulk :: Config -> U.Vector Int64 -> U.Vector v -> U.Vector Int64 -> IO (Calibration (Int, Int)) #-}
{-# SPECIALIZE calibrateBPlusTreeBulk :: Config -> U.Vector Word32 -> U.Vector v -> U.Vector Word32 -> IO (Calibration (Int, Int)) #-}
{-# SPECIALIZE calibrateBPlusTreeBulk :: Config -> U.Vector Word64 -> U.Vector v -> U.Vector Word64 -> IO (Calibration (Int, Int)) #-}
{-# SPECIALIZE calibrateBPlusTreeBulk :: Config -> U.Vector Float -> U.Vector v -> U.Vector Float -> IO (Calibration (Int, Int)) #-}
{-# SPECIALIZE calibrateBPlusTreeBulk :: Config -> U.Vector Double -> U.Vector v -> U.Vector Double -> IO (Calibration (Int, Int)) #-}

-- | Run one width's timed passes.
--
-- Each pass uses a different window of the queries, and that is load-bearing:
-- timing the same pure expression twice measures the second as already
-- evaluated, so keeping the fastest of several identical passes reports zero. An
-- earlier benchmark in this package printed 0.0 ns per query for exactly that
-- reason.
timeWidth ::
  U.Unbox a =>
  Config ->
  U.Vector a ->
  w ->
  Int ->
  (U.Vector a -> Int) ->
  IO (Measurement w)
timeWidth cfg qs w h work
  | total == 0 = return (Measurement w h 0 0)
  | otherwise = do
      ts <- forM [0 .. reps - 1] $ \r -> do
        t0 <- getMonotonicTime
        _ <- evaluate (work (U.slice (min r (total - count)) count qs))
        t1 <- getMonotonicTime
        return (t1 - t0)
      let best = minimum ts
      return (Measurement w h (best * 1e9 / fromIntegral count) best)
  where
    total = U.length qs
    -- Never ask for more passes than there are distinct windows.
    reps = max 1 (min (cfgReps cfg) total)
    count = max 1 (total - reps + 1)
{-# INLINABLE timeWidth #-}

-- | Fastest, then broken toward the preferred configuration as far as the tie
-- band allows. Exposed so a caller can apply a different policy to
-- 'calMeasurements' -- and so the policy can be tested without depending on a
-- timing run. @prefer@ maps a configuration to a key, and the smallest key
-- among those inside the band wins.
--
-- The band carries an absolute floor as well as the proportional tolerance. A
-- purely proportional band collapses to nothing when the fastest measurement is
-- zero, which is what happens when the passes are shorter than the clock can
-- resolve -- and the width would then be decided by which ones happened to
-- quantise to zero. With the floor, unresolvable runs tie across the board and
-- the narrowest wins, which is at least deterministic. 'calResolvable' is how
-- the caller finds out that is what happened.
chooseBy :: Ord k => Config -> (w -> k) -> [Measurement w] -> Calibration w
chooseBy cfg prefer ms = Calibration best ms resolvable
  where
    fastest = minimum (map mNanosPerQuery ms)
    band = fastest * (1 + cfgTolerance cfg) + tieFloorNanos
    banded = [m | m <- ms, mNanosPerQuery m <= band]
    best =
      mWidths
        ( foldr1
            (\a b -> if prefer (mWidths a) <= prefer (mWidths b) then a else b)
            (if null banded then ms else banded)
        )
    resolvable = all ((>= resolutionFloorSeconds) . mElapsed) ms

-- | Half a nanosecond per query. Below this, differences are not real.
tieFloorNanos :: Double
tieFloorNanos = 0.5

-- | A pass shorter than this cannot be timed usefully: at the microsecond
-- granularity seen on some platforms it is fewer than a hundred ticks.
resolutionFloorSeconds :: Double
resolutionFloorSeconds = 1e-4
