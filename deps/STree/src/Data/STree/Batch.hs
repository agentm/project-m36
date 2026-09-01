{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Bulk lookups, answered by a vectorised descent in C.
--
-- == Why the batch is the unit
--
-- The node rank is a handful of instructions once vectorised, so calling into C
-- once per node — or even once per query — would spend the win on foreign-call
-- overhead. A batch amortises one call over hundreds of queries, which is why
-- the SIMD path is exposed here rather than behind 'lowerBoundIdx'. Single
-- lookups stay in Haskell and are unaffected.
--
-- == Portability
--
-- @cbits\/stree_simd.c@ uses GCC\/clang vector extensions, not intrinsics, so
-- there is one source for every target: the compiler lowers it to NEON, SSE\/AVX
-- or scalar code as appropriate. On x86-64 it compiles for the SSE2 baseline;
-- add @cc-options: -mavx2@ if you are building for a machine you control.
--
-- The results are identical to 'lowerBoundIdx' \/ 'upperBoundIdx' by
-- construction, and the test suite checks that against the whole matrix of node
-- widths, key types and sizes.
module Data.STree.Batch
  ( lowerBoundIdxMany
  , upperBoundIdxMany
  , BatchKey
  ) where

import Control.Concurrent (yield)
import Data.Int (Int32, Int64)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray (..))
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Base as UB
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32, Word64)
import GHC.Exts (ByteArray#, MutableByteArray#, RealWorld)
import System.IO.Unsafe (unsafePerformIO)

import Data.STree.BTree
import Data.STree.Key (Key)

-- | Queries per foreign call.
--
-- An @unsafe@ foreign call cannot be interrupted by the garbage collector: it
-- does not release its capability, and GHC's collector is stop-the-world, so
-- every other thread in the program waits for it. Measured on this machine, a
-- 14 ms @unsafe@ call stalled an unrelated allocating thread for the whole of
-- it, where the same work behind a @safe@ call cost that thread 177us.
--
-- Chunking alone does /not/ fix this, which is worth knowing before touching
-- the loop below: GHC omits yield checks from loops that do not allocate, and
-- this one does not, so all the chunks ran back to back with no safe point
-- between them and the pause was the same as with no chunking at all. Nor can
-- the library force the issue with @-fno-omit-yields@, since 'searchMany' is
-- @INLINABLE@ and the loop is therefore compiled in the caller's module.
--
-- Hence the explicit 'yield' between chunks. It is a primop the optimiser
-- cannot elide, and it brings the worst-case stall back down to what the
-- program's own collector pauses cost anyway. Price: about 3% of batch
-- throughput. Larger chunks do not buy it back -- the cost is the yield itself,
-- not its frequency -- so this stays small enough to bound the pause tightly.
chunkSize :: Int
chunkSize = 256

-- | The shape of the tree, as the C descent needs it: @n@, height, virtual
-- size, exceeding leaves, node width.
data Geom = Geom !Int !Int !Int !Int !Int

-- | The C entry point for one element type.
type BatchFn =
  ByteArray# -> -- tree
  Int -> -- tree offset, in elements
  Int -> -- n
  Int -> -- height
  Int -> -- virtual size
  Int -> -- exceeding leaves
  Int -> -- node width
  ByteArray# -> -- keys
  Int -> -- key offset
  Int -> -- count
  MutableByteArray# RealWorld -> -- output
  Int -> -- output offset
  Int -> -- 1 for a lower bound, 0 for an upper bound
  IO ()

foreign import ccall unsafe "stree_batch_i32" c_batch_i32 :: BatchFn
foreign import ccall unsafe "stree_batch_i64" c_batch_i64 :: BatchFn
foreign import ccall unsafe "stree_batch_u32" c_batch_u32 :: BatchFn
foreign import ccall unsafe "stree_batch_u64" c_batch_u64 :: BatchFn
foreign import ccall unsafe "stree_batch_f32" c_batch_f32 :: BatchFn
foreign import ccall unsafe "stree_batch_f64" c_batch_f64 :: BatchFn

-- | Key types with a vectorised descent. @Int@ and @Word@ are absent because
-- their width is platform-dependent, so a fixed-width kernel cannot be selected
-- for them.
class Key a => BatchKey a where
  -- | Run one chunk: @keys[from .. from+count)@ into @out[from ..)@.
  runChunk :: Bool -> Geom -> U.Vector a -> U.Vector a -> Int -> Int -> UM.IOVector Int -> IO ()

instance BatchKey Int32 where
  runChunk = withPrim c_batch_i32 (\(UB.V_Int32 p) -> p)
  {-# INLINE runChunk #-}

instance BatchKey Int64 where
  runChunk = withPrim c_batch_i64 (\(UB.V_Int64 p) -> p)
  {-# INLINE runChunk #-}

instance BatchKey Word32 where
  runChunk = withPrim c_batch_u32 (\(UB.V_Word32 p) -> p)
  {-# INLINE runChunk #-}

instance BatchKey Word64 where
  runChunk = withPrim c_batch_u64 (\(UB.V_Word64 p) -> p)
  {-# INLINE runChunk #-}

instance BatchKey Float where
  runChunk = withPrim c_batch_f32 (\(UB.V_Float p) -> p)
  {-# INLINE runChunk #-}

instance BatchKey Double where
  runChunk = withPrim c_batch_f64 (\(UB.V_Double p) -> p)
  {-# INLINE runChunk #-}

-- | Unwrap the unboxed vectors to the byte arrays behind them and make the
-- call.
--
-- Passing an unpinned 'ByteArray#' to a foreign function is safe precisely
-- because the call is @unsafe@: the garbage collector cannot run during it, so
-- the array cannot move. Each vector carries its own element offset, since it
-- may be a slice of a larger array.
withPrim ::
  BatchFn ->
  (U.Vector a -> P.Vector a) ->
  Bool ->
  Geom ->
  U.Vector a ->
  U.Vector a ->
  Int ->
  Int ->
  UM.IOVector Int ->
  IO ()
withPrim call unwrap lower (Geom n h vsize e l) tree keys from count out =
  case unwrap tree of
    P.Vector treeOff _ (ByteArray tree#) ->
      case unwrap keys of
        P.Vector keysOff _ (ByteArray keys#) ->
          case out of
            UB.MV_Int (PM.MVector outOff _ (MutableByteArray out#)) ->
              call
                tree#
                treeOff
                n
                h
                vsize
                e
                l
                keys#
                (keysOff + from)
                count
                out#
                (outOff + from)
                (if lower then 1 else 0)
{-# INLINE withPrim #-}

-- | Indices of the first key @>= x@ for every @x@ in the input, in order.
-- Equivalent to @'U.map' ('lowerBoundIdx' t)@, but with a vectorised descent.
lowerBoundIdxMany :: forall l a. (LineWidth l, BatchKey a) => BTree l a -> U.Vector a -> U.Vector Int
lowerBoundIdxMany = searchMany True
{-# INLINABLE lowerBoundIdxMany #-}

-- | Indices of the first key @> x@ for every @x@ in the input, in order.
-- Equivalent to @'U.map' ('upperBoundIdx' t)@, but with a vectorised descent.
upperBoundIdxMany :: forall l a. (LineWidth l, BatchKey a) => BTree l a -> U.Vector a -> U.Vector Int
upperBoundIdxMany = searchMany False
{-# INLINABLE upperBoundIdxMany #-}

searchMany :: forall l a. (LineWidth l, BatchKey a) => Bool -> BTree l a -> U.Vector a -> U.Vector Int
searchMany lower t keys
  | U.null keys = U.empty
  | otherwise = unsafePerformIO $ do
      out <- UM.new count
      let go !from
            | from >= count = return ()
            | otherwise = do
                let m = min chunkSize (count - from)
                runChunk lower geom (layout t) keys from m out
                yield
                go (from + m)
      go 0
      U.unsafeFreeze out
  where
    count = U.length keys
    geom =
      Geom
        (size t)
        (height t)
        (virtualSize t)
        (exceedingLeaves t)
        (lineWidth @l)
{-# INLINABLE searchMany #-}
