{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Leaf width, on its own, shaped for a profiler.
--
-- Everything else is nailed down — a million @Int32@ keys, index width 16, the
-- bulk query path — so the only variable is @ll@. The bulk path is the one to
-- profile for this question: its index descent is the C kernel, so whatever time
-- lands in Haskell /is/ the leaf rank, and a Time Profiler trace separates the
-- two without any inference.
--
-- == Running it
--
-- > s-tree-leafwidth                  -- sweep every width, print a table
-- > s-tree-leafwidth 16               -- only ll=16, looped, for profiling
-- > s-tree-leafwidth 16 --seconds 20 --pool 400000
--
-- Given a width it runs that one configuration and nothing else for the whole
-- process, which is what makes an Instruments trace readable: every sample in it
-- belongs to the width under test. Sweeping mode is for a quick answer, not for
-- profiling — six configurations in one trace cannot be told apart.
--
-- == Two things that matter for the numbers
--
-- @--pool@ is the count of /distinct/ queries, and it is not a detail. Repeating
-- a small set keeps the leaves hot and reverses the answer: with a thousand
-- queries a wide leaf looks free, and past about ten thousand it does not. The
-- default is deliberately larger than cache. If a profile disagrees with an
-- earlier measurement, check this first.
--
-- == Reading the trace
--
-- Do not expect to find @drive16@ in it. The drivers are 'OPAQUE', which does
-- keep them from being inlined away, but GHC still emits no symbol under that
-- name -- the code lands in local labels like @LcaAC_info@ instead. Attribution
-- comes from the process, not the symbol: one width per run means every sample
-- in the trace belongs to the width on the command line.
--
-- Within a run the split that matters /is/ visible by name, because the two
-- halves are in different languages:
--
--   * @stree_batch_i32@ -- the vectorised index descent, in C.
--   * @L...._info@ frames under it -- the leaf rank, in Haskell.
--
-- Measured that way with @sample@, 200k distinct queries, self time on the main
-- thread (the RTS threads idle in @cvwait@ and must be excluded or they swamp
-- everything):
--
-- >         index (C)   leaf (Haskell)
-- > ll=2       57.6%            40.4%
-- > ll=64      25.2%            73.4%
--
-- Which is the whole trade in one table: a narrow leaf pushes the work into the
-- index, a wide one pushes it into the leaf scan.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.Int (Int32)
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

import Data.STree
import qualified Data.STree.BPlus as BP

keyCount :: Int
keyCount = 1000000

-- | Queries per call into the tree. Large enough that the foreign call and the
-- result allocation disappear, small enough to stay a normal usage pattern.
batchSize :: Int
batchSize = 1000

randoms :: Word64 -> [Word64]
randoms = drop 1 . iterate (\s -> s * 6364136223846793005 + 1442695040888963407)

-- | Fixed seeds, named rather than inline, so a run reproduces exactly and the
-- three streams are visibly independent rather than accidentally correlated.
keySeed, valueSeed, querySeed :: Word64
keySeed = 42
valueSeed = 0x5EED
querySeed = 0xBEEF

keys :: U.Vector Int32
keys = U.fromList (sort (map fromIntegral (take keyCount (randoms keySeed))))

-- | Random rather than an enumeration, so nothing here depends on the payload
-- being ordered or small.
--
-- It makes no difference to the timings, and that is worth stating rather than
-- leaving to be rediscovered: this benchmark asks only for /positions/, and
-- 'BP.lowerBoundIdxMany' reads @bpIndex@ and @bpKeys@ and never @bpValues@. The
-- array is built and carried because a real tree carries it, but it is not read
-- in the timed loop. A benchmark of 'BP.lookup' would be a different measurement
-- -- one more dependent miss per query, into an array the leaf width does not
-- shrink.
values :: U.Vector Int32
values = U.fromList (map fromIntegral (take keyCount (randoms valueSeed)))

queryPool :: Int -> U.Vector Int32
queryPool poolSize = U.fromList (map fromIntegral (take poolSize (randoms querySeed)))

-- | Answer @batches * batchSize@ queries starting from batch @start@ of the
-- pool. Returns a checksum so nothing can be optimised away.
--
-- @start@ is not decoration. Without it every call would be the same expression,
-- and a repeated @evaluate@ of one expression measures the second and later
-- calls as free -- which is exactly how the first version of this file reported
-- 0.0 ns for every width.
driveWith ::
  forall li ll.
  (LineWidth li, LineWidth ll) =>
  BP.BPlusTree li ll Int32 Int32 ->
  U.Vector Int32 ->
  Int ->
  Int ->
  Int
driveWith t pool start batches = go 0 0
  where
    stride = max 1 (U.length pool - batchSize)
    go !acc !i
      | i >= batches = acc
      | otherwise =
          let off = ((start + i) * batchSize) `mod` stride
              r = U.sum (BP.lowerBoundIdxMany t (U.slice off batchSize pool))
           in go (acc + r) (i + 1)
{-# INLINE driveWith #-}

-- The trees are top-level so each is built exactly once, and lazily, so
-- profiling one width does not pay to build the other five. Constructing them
-- inside the drivers would leave it to full laziness whether the build happened
-- once or on every call.
tree2 :: BP.BPlusTree 16 2 Int32 Int32
tree2 = BP.unsafeBuild keys values
tree4 :: BP.BPlusTree 16 4 Int32 Int32
tree4 = BP.unsafeBuild keys values
tree8 :: BP.BPlusTree 16 8 Int32 Int32
tree8 = BP.unsafeBuild keys values
tree16 :: BP.BPlusTree 16 16 Int32 Int32
tree16 = BP.unsafeBuild keys values
tree32 :: BP.BPlusTree 16 32 Int32 Int32
tree32 = BP.unsafeBuild keys values
tree64 :: BP.BPlusTree 16 64 Int32 Int32
tree64 = BP.unsafeBuild keys values
{-# NOINLINE tree2 #-}
{-# NOINLINE tree4 #-}
{-# NOINLINE tree8 #-}
{-# NOINLINE tree16 #-}
{-# NOINLINE tree32 #-}
{-# NOINLINE tree64 #-}

-- One driver per width, each 'OPAQUE' so a profile can name it.
--
-- OPAQUE rather than NOINLINE, and the difference is not academic: with NOINLINE
-- these six bindings vanished from the symbol table entirely. NOINLINE only
-- forbids inlining, leaving GHC free to eta-reduce, worker\/wrapper and rename
-- them until nothing recognisable is left. OPAQUE bars that whole class of
-- transformation, which is what keeps a name in the binary for Instruments to
-- attribute samples to. The cost is one un-worker\/wrappered call per 64000
-- queries; the loop inside is untouched.
drive2, drive4, drive8, drive16, drive32, drive64 :: U.Vector Int32 -> Int -> Int -> Int
drive2 p s b = driveWith tree2 p s b
drive4 p s b = driveWith tree4 p s b
drive8 p s b = driveWith tree8 p s b
drive16 p s b = driveWith tree16 p s b
drive32 p s b = driveWith tree32 p s b
drive64 p s b = driveWith tree64 p s b
{-# OPAQUE drive2 #-}
{-# OPAQUE drive4 #-}
{-# OPAQUE drive8 #-}
{-# OPAQUE drive16 #-}
{-# OPAQUE drive32 #-}
{-# OPAQUE drive64 #-}

driverFor :: Int -> Maybe (U.Vector Int32 -> Int -> Int -> Int)
driverFor 2 = Just drive2
driverFor 4 = Just drive4
driverFor 8 = Just drive8
driverFor 16 = Just drive16
driverFor 32 = Just drive32
driverFor 64 = Just drive64
driverFor _ = Nothing

-- | Separators the index must hold, and so the memory the leaf width buys back.
indexKeysAt :: Int -> Int
indexKeysAt ll = max 0 ((keyCount + ll - 1) `div` ll - 1)

-- | Run a width until the clock says to stop, then report throughput.
timed :: Int -> Double -> U.Vector Int32 -> IO Double
timed ll seconds pool =
  case driverFor ll of
    Nothing -> putStrLn ("unsupported leaf width: " ++ show ll) >> exitFailure
    Just drive -> do
      _ <- evaluate (drive pool 0 1) -- build the tree outside the timed region
      let loop !done !round_ !t0 = do
            !_ <- evaluate (drive pool round_ 64)
            now <- getMonotonicTime
            let done' = done + 64 * batchSize
            if now - t0 >= seconds
              then return (done', now - t0)
              else loop done' (round_ + 64) t0
      t0 <- getMonotonicTime
      (answered, elapsed) <- loop 0 0 t0
      return (elapsed * 1e9 / fromIntegral answered)

main :: IO ()
main = do
  args <- getArgs
  let poolSize = intArg "--pool" 200000 args
      seconds = fromIntegral (intArg "--seconds" 0 args) :: Double
      widths = [w | a <- positionals args, Just w <- [readMaybeInt a]]
      pool = queryPool poolSize
  _ <- evaluate (U.sum keys)
  _ <- evaluate (U.sum pool)

  case widths of
    [ll] -> do
      -- Profiling mode: one configuration, for as long as asked.
      let secs = if seconds > 0 then seconds else 10
      printf
        "profiling ll=%d (li=16, %d keys, %d distinct queries, %.0fs)\n"
        ll
        keyCount
        poolSize
        secs
      ns <- timed ll secs pool
      printf "  %.1f ns per query, index holds %d separators (%.2f MB)\n"
        ns
        (indexKeysAt ll)
        (fromIntegral (indexKeysAt ll) * 4 / 1048576 :: Double)
    _ -> do
      printf
        "leaf width sweep: li=16, %d keys, %d distinct queries\n"
        keyCount
        poolSize
      unless (poolSize > 20000) $
        putStrLn "  WARNING: a small pool keeps leaves hot and will favour wide leaves"
      printf "  %-6s %12s %10s %14s %10s\n" "ll" "ns/query" "vs best" "separators" "index MB"
      results <- mapM (\ll -> (,) ll <$> timed ll 1.5 pool) [2, 4, 8, 16, 32, 64]
      let best = minimum (map snd results)
      forM_ results $ \(ll, ns) ->
        printf
          "  %-6d %12.1f %9.0f%% %14d %10.2f\n"
          ll
          ns
          (100 * ns / best)
          (indexKeysAt ll)
          (fromIntegral (indexKeysAt ll) * 4 / 1048576 :: Double)
      putStrLn "  (to profile one of these: s-tree-leafwidth <ll> --seconds 20)"
  where
    readMaybeInt s = case reads s of
      [(v, "")] -> Just (v :: Int)
      _ -> Nothing
    -- A flag consumes the token after it, so that token is not a width. Scanning
    -- for bare integers instead made "16 --seconds 3" mean widths [16,3], which
    -- is not a single width, so it quietly ran the sweep and ignored both.
    positionals (a : v : rest)
      | take 2 a == "--" = positionals' v rest
      where positionals' _ r = positionals r
    positionals (a : rest)
      | take 2 a == "--" = positionals rest
      | otherwise = a : positionals rest
    positionals [] = []
    intArg name def as = case dropWhile (/= name) as of
      (_ : v : _) | Just n <- readMaybeInt v -> n
      _ -> def
