{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A static, pointer-free B-tree over a sorted array of keys.
--
-- == Layout
--
-- A node holds @L@ keys (a type-level parameter) and therefore has fanout
-- @B = L+1@. The sorted input is permuted once
-- into a generalisation of the Eytzinger layout: the tree is stored level by
-- level, level @d@ starting at array offset @B^d - 1@, with no pointers. The
-- original sorted array can be discarded — 'index' recovers any element in
-- @O(log_B n)@.
--
-- Because @n@ is generally not of the form @B^k - 1@, the deepest level is
-- partial. The build handles that by splitting the input into a prefix, which
-- fills the leftmost subtrees completely, and the rest, which forms a complete
-- tree of @h-1@ levels; the partial deepest level is stored contiguously at the
-- end of the array. That split is what all the @exceeding@ bookkeeping below is
-- about.
--
-- == Notes on the geometry
--
-- * The height is the smallest @h@ with @B^h > n@, computed with integer
--   arithmetic. Deriving it from a floating-point logarithm risks rounding to
--   one below the correct value when @n@ is an exact power of @B@, which would
--   make the virtual size smaller than @n@ and corrupt the layout.
-- * Powers of @B@ are computed on demand, so nothing bounds the height.
-- * Floating-point keys are padded with @+Infinity@; see 'Key'.
-- * @n = 0@ is supported: queries return @0@.
-- * The descent is a plain strict loop, not specialised on a statically known
--   height. Fixing the height at compile time would let the level constants be
--   folded, but GHC gains nothing measurable from it here.
--
-- == A note on the pragmas
--
-- Everything here is overloaded in the key type, and every hot function carries
-- an @INLINE@ or @INLINABLE@ pragma deliberately. Without one, GHC compiles the
-- function once with dictionary arguments and callers in other modules cannot
-- specialise it — @-fspecialise@ only specialises imported functions that are
-- marked @INLINABLE@. Measured cost of getting this wrong: 7x on 'unsafeBuild',
-- 14x on 'build', 3x on 'index'. Do not remove them.
module Data.STree.BTree
  ( -- * Type
    BTree
  , LineWidth
  , lineWidth

    -- * Construction
  , build
  , buildFromList
  , unsafeBuild
  , BuildError (..)

    -- * Queries
  , lowerBoundIdx
  , upperBoundIdx
  , index
  , (!)

    -- * Properties
  , size
  , height
  , lineSize
  , sizeInBytes
  , logSize
  , toVector

    -- * Internal: the layout array
    -- | Enough to persist a tree and rebuild it without redoing the
    -- permutation; see "Data.STree.Serialize".
  , layout
  , layoutLength
  , unsafeFromLayout
  , virtualSize
  , exceedingLeaves
  , elems
  , foldlKeys'
  , foldMKeys
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.TypeNats (KnownNat, Nat, natVal)

import Data.STree.Internal.Rank (blockRank)
import Data.STree.Key (Key (..))

-- | The number of keys per node, always a power of two. Widths from 2 to 64
-- are supported: 2 is useful for exercising deep trees on small inputs, and the
-- upper bound of 64 is real, since the scalar node rank is computed in a single
-- 'Data.Word.Word64' mask.
class KnownNat l => LineWidth (l :: Nat)

instance LineWidth 2
instance LineWidth 4
instance LineWidth 8
instance LineWidth 16
instance LineWidth 32
instance LineWidth 64

-- | The node width as a number. Use as @lineWidth \@16@.
lineWidth :: forall l. LineWidth l => Int
lineWidth = fromIntegral (natVal (Proxy :: Proxy l))
{-# INLINE lineWidth #-}

-- | A static B-tree with @l@ keys per node over keys of type @a@.
data BTree (l :: Nat) a = BTree
  { btLayout :: !(U.Vector a)
  -- ^ The permuted keys, followed by sentinel padding to a multiple of @L@.
  , btSize :: !Int
  -- ^ @tree_size@: the number of real keys.
  , btVirtualSize :: !Int
  -- ^ @B^h - 1@: the size the complete tree of this height would have (equal
  -- to 'btSize' exactly when @n = B^h - 1@).
  , btHeight :: !Int
  -- ^ The number of levels.
  , btExceedingNodes :: !Int
  -- ^ How many of the input elements were consumed filling the leftmost
  -- subtrees completely: the exceeding leaves plus the separators interleaved
  -- with them.
  , btExceedingLeaves :: !Int
  -- ^ @n - (B^(h-1) - 1)@: the number of keys on the partial deepest level.
  }
  deriving (Eq, Show)

-- | Every field is strict and the keys are unboxed, so a tree in weak head
-- normal form is already fully evaluated. No constraint on @a@ is needed.
instance NFData (BTree l a) where
  rnf (BTree arr n vs h en el) =
    arr `seq` n `seq` vs `seq` h `seq` en `seq` el `seq` ()

-- | Why an input cannot be turned into a tree.
data BuildError
  = -- | The input is not sorted in non-decreasing order (also reported for
    -- @NaN@, which is unordered).
    NotSorted
  | -- | Some key is @>= 'sentinel'@, which the padding scheme reserves.
    KeyNotBelowSentinel
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Geometry
-- ---------------------------------------------------------------------------

-- | @fanoutPow b k = b^k@ for @k >= 0@.
fanoutPow :: Int -> Int -> Int
fanoutPow b = go 1
  where
    go !acc !k
      | k <= 0 = acc
      | otherwise = go (acc * b) (k - 1)

-- | The number of levels for @n@ keys: the smallest @h >= 1@ with @B^h > n@,
-- i.e. @floor (log_B n) + 1@ computed exactly.
heightForSize :: Int -> Int -> Int
heightForSize b n = go 1 b
  where
    go !h !p
      | p > n = h
      | otherwise = go (h + 1) (p * b)

-- | The height a tree of @n@ keys will have, without building it.
logSize :: forall l. LineWidth l => Int -> Int
logSize = heightForSize (lineWidth @l + 1)

-- | Everything about a tree's shape that follows from the node width and the
-- number of keys. Nothing else has to be remembered about a built tree, which
-- is what makes persisting one trivial.
data Geometry = Geometry
  { gHeight :: !Int
  , gVirtualSize :: !Int
  , gReducedSize :: !Int
  -- ^ Size of the tree without the deepest, possibly partial, level.
  , gExceedingLeaves :: !Int
  , gExceedingNodes :: !Int
  , gPadding :: !Int
  }

geometry :: Int -> Int -> Geometry
geometry l n =
  Geometry
    { gHeight = h
    , gVirtualSize = fanoutPow b h - 1
    , gReducedSize = reduced
    , gExceedingLeaves = e
    , gExceedingNodes = exceedingNodes
    , gPadding = padding
    }
  where
    b = l + 1
    h = heightForSize b n

    -- Always a multiple of L, since (L+1)^k - 1 = 0 (mod L) — which is why
    -- every node of the layout, including the first one of the partial level,
    -- starts on a multiple of L.
    reduced = fanoutPow b (h - 1) - 1
    e = n - reduced

    -- Closed form of the loop in 'unsafeBuild' that interleaves the exceeding
    -- leaves with the separators between them: on the input, leaves come in
    -- runs of L separated by one separator each, so reaching the e-th leaf
    -- consumes @(e-1) `div` l@ separators. 'unsafeBuild' does not use this — it
    -- takes the value straight out of the loop — so that the round-trip test of
    -- 'unsafeFromLayout' pins the two against each other.
    exceedingNodes = if e == 0 then 0 else e + (e - 1) `div` l

    padding = case n `rem` l of
      0 -> 0
      r -> l - r

-- | Number of keys in the layout array of a tree of @n@ keys: @n@ plus the
-- sentinel padding that rounds the last node up to @l@ keys.
layoutLength :: forall l. LineWidth l => Int -> Int
layoutLength n = n + gPadding (geometry (lineWidth @l) n)

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Build a tree from a vector sorted in non-decreasing order.
--
-- @O(n)@ passes over the data, one per level, so @O(n log_B n)@ writes in
-- total.
{-# INLINABLE build #-}
build :: forall l a. (LineWidth l, Key a) => U.Vector a -> Either BuildError (BTree l a)
build v
  | not (isSortedAsc v) = Left NotSorted
  | not (U.null v) && not (U.last v < sentinel @a) = Left KeyNotBelowSentinel
  | otherwise = Right (unsafeBuild v)

-- | 'build' from a list.
{-# INLINABLE buildFromList #-}
buildFromList :: forall l a. (LineWidth l, Key a) => [a] -> Either BuildError (BTree l a)
buildFromList = build . U.fromList

-- | 'build' without the sortedness and sentinel checks. If the input violates
-- either, queries return meaningless indices (they stay in bounds).
{-# INLINABLE unsafeBuild #-}
unsafeBuild :: forall l a. (LineWidth l, Key a) => U.Vector a -> BTree l a
unsafeBuild v =
  BTree
    { btLayout = keys
    , btSize = n
    , btVirtualSize = gVirtualSize g
    , btHeight = gHeight g
    , btExceedingNodes = exceedingNodes
    , btExceedingLeaves = e
    }
  where
    l = lineWidth @l
    b = l + 1
    n = U.length v

    g = geometry l n
    reduced = gReducedSize g
    e = gExceedingLeaves g
    pad = gPadding g

    (keys, exceedingNodes) = runST $ do
      mv <- UM.new (n + pad)

      -- Padding, so that the rank of the last node cannot read garbage.
      let padLoop !i
            | i >= n + pad = return ()
            | otherwise = UM.unsafeWrite mv i (sentinel @a) >> padLoop (i + 1)
      padLoop n

      -- Walk the input while placing the `e` keys of the partial deepest level
      -- contiguously at the end of the array. Every B-th input element is a
      -- separator belonging to a higher level, so it goes to the front region
      -- instead, which will be permuted by 'buildBottomUp'.
      let prefix !remaining !cur !front
            | remaining <= 0 = return (cur, front)
            | cur `rem` b == l = do
                UM.unsafeWrite mv front (U.unsafeIndex v cur)
                prefix remaining (cur + 1) (front + 1)
            | otherwise = do
                UM.unsafeWrite mv (n - remaining) (U.unsafeIndex v cur)
                prefix (remaining - 1) (cur + 1) front
      (cur, front) <- prefix e 0 0

      -- The remaining input is already in the right order for the front
      -- region: `front + (n - cur) == reduced`.
      U.unsafeCopy (UM.unsafeSlice front (n - cur) mv) (U.unsafeSlice cur (n - cur) v)

      when (reduced > 0) $ buildBottomUp b reduced mv

      frozen <- U.unsafeFreeze mv
      return (frozen, cur)

-- | Permute the sorted run @mv[0 .. reduced)@ into the level-by-level layout,
-- bottom up.
--
-- One iteration per level: scanning right to left, each group of @B@ keys
-- contributes its last @L@ to the level being finalised — shifted right, to
-- the region @[internal, curSize)@ that the layout reserves for it — and its
-- first key to @buf@, which ends up holding the next level up, still sorted,
-- and is copied back over the front of the region.
--
-- @curSize@ is always of the form @B^k - 1@, hence @curSize \`rem\` B == L@:
-- the leftmost group has exactly @L@ keys and no separator, so the scan lands
-- exactly on the left edge.
buildBottomUp :: U.Unbox a => Int -> Int -> UM.MVector s a -> ST s ()
buildBottomUp b reduced mv
  | internal0 <= 0 = return ()
  | otherwise = do
      buf <- UM.new internal0
      let levels !curSize !curInternal
            | curInternal <= 0 = return ()
            | otherwise = do
                step buf curSize curInternal
                UM.unsafeCopy
                  (UM.unsafeSlice 0 curInternal mv)
                  (UM.unsafeSlice 0 curInternal buf)
                levels curInternal (curInternal `quot` b)
      levels reduced internal0
  where
    l = b - 1
    internal0 = reduced `quot` b

    -- w: write cursor into mv, r: read cursor into mv (always <= w),
    -- t: write cursor into buf. All exclusive, moving leftwards.
    step buf curSize curInternal = groups curSize curSize curInternal
      where
        groups !w !r !t
          | r <= 0 = return ()
          | otherwise = do
              (w', r') <- keysOfNode w r l
              if r' > 0
                then do
                  UM.unsafeRead mv (r' - 1) >>= UM.unsafeWrite buf (t - 1)
                  groups w' (r' - 1) (t - 1)
                else return ()

        keysOfNode !w !r 0 = return (w, r)
        keysOfNode !w !r !k = do
          UM.unsafeRead mv (r - 1) >>= UM.unsafeWrite mv (w - 1)
          keysOfNode (w - 1) (r - 1) (k - 1 :: Int)

-- | Carries the previous key rather than re-reading it, so the scan does one
-- load per element instead of two.
--
-- INLINE, not INLINABLE: as a separate call this stays overloaded even inside a
-- specialised 'build' — the specialiser does not reach a nested call created
-- during its own pass — and a dictionary-dispatched comparison per element costs
-- around 25× the specialised loop.
{-# INLINE isSortedAsc #-}
isSortedAsc :: (Ord a, U.Unbox a) => U.Vector a -> Bool
isSortedAsc v
  | n == 0 = True
  | otherwise = go 1 (U.unsafeIndex v 0)
  where
    n = U.length v
    go !i !prev
      | i >= n = True
      | otherwise =
          let x = U.unsafeIndex v i
           in prev <= x && go (i + 1) x

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

-- | Index of the first key @>= x@, i.e. the number of keys @< x@ — the
-- position @std.lower_bound@ would return. In @[0, 'size']@.
lowerBoundIdx :: forall l a. (LineWidth l, Key a) => BTree l a -> a -> Int
lowerBoundIdx t x = searchBy (\k -> not (k < x)) t
{-# INLINE lowerBoundIdx #-}

-- | Index of the first key @> x@, i.e. the number of keys @<= x@ — the
-- position @std.upper_bound@ would return. In @[0, 'size']@.
upperBoundIdx :: forall l a. (LineWidth l, Key a) => BTree l a -> a -> Int
upperBoundIdx t x = searchBy (\k -> not (k <= x)) t
{-# INLINE upperBoundIdx #-}

-- | The descent. @stops@ decides which keys terminate it and thereby which
-- bound is computed; see 'blockRank'.
--
-- The predicates are written @not (k < x)@ and @not (k <= x)@ rather than the
-- @k >= x@ and @k > x@ they are equivalent to. For any total order that is the
-- same test, and it costs the same comparison -- but a @NaN@ query is not
-- ordered, and there the difference matters. Every @<@ against a @NaN@ is
-- False, so the negated form stops the rank at the first key of a node instead
-- of running past the last one: the result is 0 rather than an index off the
-- end of the data, which is what the documented @[0, size]@ range requires. A
-- @NaN@ has no position in a sorted order, so 0 is as arbitrary as anything;
-- being in range is not.
--
-- @bb@ is a 1-based node counter, from which the array offset of the current
-- node is @(bb-1) * L@. After the last level it is converted to a
-- data index, by one of two formulas depending on whether the descent ended on
-- a real node or on a dummy leaf past the end of the array.
searchBy :: forall l a. (LineWidth l, Key a) => (a -> Bool) -> BTree l a -> Int
searchBy stops t = descend 0 1
  where
    l = lineWidth @l
    keys = btLayout t
    n = btSize t
    e = btExceedingLeaves t
    lastLevel = btHeight t - 1

    descend !lev !bb
      | lev < lastLevel =
          let off = (bb - 1) * l
              r = blockRank l stops keys off
           in descend (lev + 1) (off + bb + r + 1)
      | otherwise =
          let off = (bb - 1) * l
           in if off < n
                then
                  let r = blockRank l stops keys off
                      bb' = off + bb + r + 1
                   in bb' - btVirtualSize t `quot` l - 1
                else (bb - (n - e) `quot` l - 1) + e
{-# INLINE searchBy #-}

-- | The key at position @i@ of the original sorted input.
--
-- Costs @O(log_B n)@ rather than @O(1)@: the position has to be walked back
-- through the layout permutation. This is the price of not keeping the input
-- around.
{-# INLINABLE index #-}
index :: forall l a. (LineWidth l, Key a) => BTree l a -> Int -> a
index t i
  | i < 0 || i >= n =
      error $
        "Data.STree.BTree.index: index " ++ show i ++ " out of range for size " ++ show n
  | otherwise = U.unsafeIndex (btLayout t) (levelStart + offset)
  where
    l = lineWidth @l
    b = l + 1
    n = btSize t

    -- Two regimes. For i in the exceeding prefix, i+1 is the key's 1-based
    -- in-order rank in the (locally complete) tree of h levels. Past the
    -- prefix, the key lives in the complete tree of h-1 levels formed by the
    -- rest of the input, at 1-based rank i - exceedingLeaves + 1 — hence the
    -- level shift by one in 'levelStart'.
    inPrefix = i < btExceedingNodes t
    adj = (if inPrefix then i else i - btExceedingLeaves t) + 1

    -- Divide out the factors of B: a rank divisible by B^k but not B^(k+1)
    -- belongs to the level k above the deepest one.
    climb !p !d
      | adj `rem` p == 0 = climb (p * b) (d - 1)
      | otherwise = (p, d)
    (fanout, depth) = climb b (btHeight t - 1)

    -- Rank of the key within its level: keys at or before it on this level,
    -- minus those that sit on a level above.
    offset = adj `quot` (fanout `quot` b) - adj `quot` fanout - 1
    levelStart = fanoutPow b (if inPrefix then depth else depth - 1) - 1

-- | Infix 'index'.
(!) :: forall l a. (LineWidth l, Key a) => BTree l a -> Int -> a
(!) = index
{-# INLINE (!) #-}

infixl 9 !

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | The number of keys.
size :: BTree l a -> Int
size = btSize

-- | The number of levels (@log_B n + 1@).
height :: BTree l a -> Int
height = btHeight

-- | Keys per node, from the type-level parameter.
lineSize :: forall l a. LineWidth l => BTree l a -> Int
lineSize _ = lineWidth @l

-- | Bytes occupied by the key array, padding included. The record's own
-- handful of words is not counted.
sizeInBytes :: forall l a. Key a => BTree l a -> Int
sizeInBytes t = U.length (btLayout t) * keyBytes @a

-- | Recover the original sorted input, @O(n log_B n)@.
{-# INLINABLE toVector #-}
toVector :: forall l a. (LineWidth l, Key a) => BTree l a -> U.Vector a
toVector t = U.generate (btSize t) (index t)

-- | The size the complete tree of this height would have. Needed by an
-- out-of-module implementation of the descent.
virtualSize :: BTree l a -> Int
virtualSize = btVirtualSize

-- | The number of keys on the partial deepest level. Needed by an
-- out-of-module implementation of the descent.
exceedingLeaves :: BTree l a -> Int
exceedingLeaves = btExceedingLeaves

-- | The raw permuted array, padding included. Together with 'size' this is the
-- entire state of a tree.
layout :: BTree l a -> U.Vector a
layout = btLayout

-- | Rebuild a tree around an already-permuted layout array — the inverse of
-- 'layout', for loading a tree that was built earlier without redoing the
-- @O(n log_B n)@ permutation.
--
-- The length is checked, which is what keeps the queries in bounds: every index
-- the descent can compute is bounded by the size of the layout array. The
-- /contents/ are not checked, so a garbled array gives meaningless answers
-- rather than a crash. Verifying them would cost as much as rebuilding.
{-# INLINABLE unsafeFromLayout #-}
unsafeFromLayout :: forall l a. (LineWidth l, U.Unbox a) => Int -> U.Vector a -> BTree l a
unsafeFromLayout n arr
  | U.length arr /= expected =
      error $
        "Data.STree.BTree.unsafeFromLayout: layout has "
          ++ show (U.length arr)
          ++ " keys, expected "
          ++ show expected
          ++ " for a tree of size "
          ++ show n
  | otherwise =
      BTree
        { btLayout = arr
        , btSize = n
        , btVirtualSize = gVirtualSize g
        , btHeight = gHeight g
        , btExceedingNodes = gExceedingNodes g
        , btExceedingLeaves = gExceedingLeaves g
        }
  where
    g = geometry (lineWidth @l) n
    expected = n + gPadding g

{-# INLINE foldlKeys' #-}
foldlKeys' :: forall l a b. (LineWidth l, Key a) => (b -> a -> b) -> b -> BTree l a -> b
foldlKeys' f z0 t = go 1 0 z0
  where
    l = lineWidth @l
    ks = btLayout t
    n = btSize t
    lastLevel = btHeight t -1
    go !bb !lev !z
      | lev >= lastLevel = leaf off z
      | otherwise = step 0 z
      where
        off = (bb - 1) * l
        c0 = off + bb + 1
        step !r !acc
          | r >= 1 = go (c0 +r) (lev + 1) acc
          | otherwise =
            let !acc' = go (c0 + r) (lev + 1) acc
            in step (r + 1) (f acc' (U.unsafeIndex ks (off + r)))

    leaf !off !z = loop off z
      where
        hi = min n (off + l)
        loop !i !acc
          | i >= hi = acc
          | otherwise = loop (i + 1) (f acc (U.unsafeIndex ks i))

foldrKeys :: forall l a b. (LineWidth l, Key a) => (a -> b -> b) -> b -> BTree l a -> b
foldrKeys f z t = go 1 0 z
  where
    l = lineWidth @l
    ks = btLayout t
    n = btSize t
    lastLevel = btHeight t - 1

    go !bb !lev rest
      | lev >= lastLevel = leaf off rest
      | otherwise = step 0
      where
        off = (bb - 1) * l
        c0 = off + bb + 1
        step !r
          | r >= l = go (c0 + r) (lev + 1) rest
          | otherwise =
            go (c0 + r) (lev + 1) (f (U.unsafeIndex ks (off + r)) (step (r + 1)))

    leaf !off rest = loop off
      where
        hi = min n (off + l)
        loop !i
          | i >= hi = rest
          | otherwise = f (U.unsafeIndex ks i) (loop (i + 1))
      

foldMKeys :: forall l a b m.
  (Monad m, LineWidth l, Key a) =>
  (b -> a -> m b) ->
  b ->
  BTree l a ->
  m b
foldMKeys f z0 t = go 1 0 z0
  where
    l = lineWidth @l
    ks = btLayout t
    n = btSize t
    lastLevel = btHeight t - 1

    go !bb !lev !z
      | lev >= lastLevel = leaf off z
      | otherwise = step 0 z
      where
        off = (bb - 1) * l
        c0 = off + bb + 1
        step !r !acc
          | r >= l = go (c0 + r) (lev + 1) acc
          | otherwise = do
              !down <- go (c0 + r) (lev + 1) acc
              !here <- f down (U.unsafeIndex ks (off + r))
              step (r + 1) here

    leaf !off !z = loop off z
      where
        hi = min n (off + l)
        loop !i !acc
          | i >= hi = return acc
          | otherwise = do
              !acc' <- f acc (U.unsafeIndex ks i)
              loop (i + 1) acc'

elems :: forall l a. (LineWidth l, Key a) => BTree l a -> [a]
elems = foldrKeys (:) []
