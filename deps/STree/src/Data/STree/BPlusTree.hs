-- Staying in range is 'leafRank''s doing: it bisects a bounded interval, so it
-- cannot walk past the end of the leaf whatever the query is. The index step
-- still needs care -- 'B.lowerBoundIdx' is a linear node rank underneath, and it
-- is the negated comparison inside "Data.STree.BTree" that keeps a @NaN@ query
-- from running off a node there.
--
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A static B+tree: a sorted key/value map, built once and then only queried.
--
-- The difference from "Data.STree.BTree" is where the keys live. There, keys
-- occupy every level of the tree, so sorted order is scattered through the
-- layout: recovering the key at a position costs a walk back up it, and reading
-- a range costs that walk per element. Here every key lives in the leaves, in
-- sorted order and contiguous, and the internal levels hold only separators
-- copied out of them. That buys three things:
--
--   * a value array indexable directly by position, with no permutation;
--   * 'keyAt' and 'valueAt' in @O(1)@;
--   * ranges as slices — 'rangeValues' does no work per element at all.
--
-- It costs one extra copy of about @n \/ ll@ keys for the separators, so the key
-- storage overhead is @1\/ll@: 6.3% at @ll = 16@, 1.6% at @ll = 64@.
--
-- == Structure
--
-- The internal index is a search tree over the separators, which is precisely
-- what a "Data.STree.BTree" is, so this is built out of one rather than
-- reimplementing the layout. Nothing in that module is modified or duplicated.
--
-- The two node widths are independent. @ll@ sets how many keys a leaf holds,
-- and so both the final scan of a lookup and — since there is one separator per
-- leaf boundary — the size of the index. @li@ is the width of the index tree
-- itself, which is small and tends to stay cached. Their best values are not
-- the same.
--
-- == Usage
--
-- > import qualified Data.STree.BPlus as BP
-- >
-- > Right t <- pure (BP.build ks vs) :: Either BP.BuildError (BP.BPlusTree 16 16 Int32 Int32)
-- > BP.lookup t 42          -- every value stored under 42, in order
-- > BP.rangeValues t 10 20  -- every value with a key in [10, 20)
module Data.STree.BPlusTree
  ( -- * Type
    BPlusTree

    -- * Construction
  , build
  , unsafeBuild
  , BuildError (..)

    -- * Queries
  , lookup
  , member
  , lowerBoundIdx
  , upperBoundIdx
  , keyAt
  , valueAt

    -- * Bulk queries
  , lowerBoundIdxMany
  , upperBoundIdxMany

    -- * Ranges
  , rangeIdx
  , rangeKeys
  , rangeValues

    -- * Properties
  , size
  , height
  , leafWidth
  , indexWidth
  , sizeInBytes
  , keys
  , values
  , indexTree
  ) where

import Control.DeepSeq (NFData (..))
import qualified Data.Vector.Unboxed as U
import Foreign.Storable (Storable, sizeOf)
import GHC.TypeNats (Nat)
import Prelude hiding (lookup)

import qualified Data.STree.BTree as B
import qualified Data.STree.Batch as Batch
import Data.STree.Key (Key (..))

-- | A static map from @k@ to @v@. @li@ is the node width of the internal index,
-- @ll@ the number of keys per leaf.
data BPlusTree (li :: Nat) (ll :: Nat) k v = BPlusTree
  { bpIndex :: !(B.BTree li k)
  -- ^ Search tree over the separators: the first key of every leaf but the
  -- first. Empty when the whole map fits in one leaf.
  , bpKeys :: !(U.Vector k)
  -- ^ Every key, sorted, padded with 'sentinel' to a whole number of leaves.
  , bpValues :: !(U.Vector v)
  -- ^ Parallel to the first 'bpSize' entries of 'bpKeys'. Not padded: a
  -- position at or past 'bpSize' is never a valid value index.
  , bpSize :: !Int
  }
  deriving (Eq, Show)

-- | Strict fields over unboxed vectors, so weak head normal form is already
-- fully evaluated.
instance NFData (BPlusTree li ll k v) where
  rnf (BPlusTree ix ks vs n) = ix `seq` ks `seq` vs `seq` n `seq` ()

-- | Why an input cannot be turned into a map. Distinct from
-- "Data.STree.BTree"'s type of the same name, which has no length case.
data BuildError
  = -- | The keys are not sorted in non-decreasing order (also reported for
    -- @NaN@, which is unordered).
    KeysNotSorted
  | -- | Some key is @>= 'sentinel'@, which the leaf padding reserves.
    KeyNotBelowSentinel
  | -- | The key and value vectors are different lengths: keys, values.
    LengthMismatch Int Int
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Build from sorted keys and their values, positionally paired.
--
-- @O(n)@ to copy the keys and values, plus the cost of indexing @n\/ll@
-- separators — the leaves need no permutation, since sorted order is the order
-- they are stored in.
build ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Key k, U.Unbox v) =>
  U.Vector k ->
  U.Vector v ->
  Either BuildError (BPlusTree li ll k v)
build ks vs
  | U.length ks /= U.length vs = Left (LengthMismatch (U.length ks) (U.length vs))
  | not (isSortedAsc ks) = Left KeysNotSorted
  | not (U.null ks) && not (U.last ks < sentinel @k) = Left KeyNotBelowSentinel
  | otherwise = Right (unsafeBuild ks vs)
{-# INLINABLE build #-}

-- | 'build' without the checks. If the keys are unsorted or the vectors differ
-- in length, queries return meaningless results; they stay in bounds.
unsafeBuild ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Key k) =>
  U.Vector k ->
  U.Vector v ->
  BPlusTree li ll k v
unsafeBuild ks vs =
  BPlusTree
    { bpIndex = B.unsafeBuild separators
    , bpKeys = leaves
    , bpValues = vs
    , bpSize = n
    }
  where
    ll = B.lineWidth @ll
    n = U.length ks

    -- At least one leaf even when empty, so a rank always has a node to read.
    leafCount = max 1 ((n + ll - 1) `div` ll)
    padded = leafCount * ll
    leaves = ks U.++ U.replicate (padded - n) (sentinel @k)

    -- One separator per leaf boundary: the first key of the leaf after it.
    -- Fewer than two leaves means none, and an empty index tree, whose
    -- lowerBoundIdx is 0 — which is the right answer, leaf 0.
    separators = U.generate (leafCount - 1) (\j -> U.unsafeIndex leaves ((j + 1) * ll))
{-# INLINABLE unsafeBuild #-}

-- | Duplicated from "Data.STree.BTree" rather than shared, to leave that module
-- untouched. Carries the previous key instead of re-reading it.
--
-- @INLINE@ is not optional: as a separate overloaded call this loop measured
-- roughly 38x slower, dispatching every comparison through a dictionary.
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

-- | Index of the first key @>= x@: the number of keys below it. Always in
-- @[0, size]@, including for a @NaN@ query, which has no position in a sorted
-- order and so returns an unspecified one.
lowerBoundIdx ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Key k) =>
  BPlusTree li ll k v ->
  k ->
  Int
lowerBoundIdx t x = descend t (B.lowerBoundIdx (bpIndex t) x) (< x)
{-# INLINABLE lowerBoundIdx #-}

-- | Index of the first key @> x@: the number of keys at or below it. Always in
-- @[0, size]@; see 'lowerBoundIdx' on @NaN@.
upperBoundIdx ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Key k) =>
  BPlusTree li ll k v ->
  k ->
  Int
upperBoundIdx t x = descend t (B.upperBoundIdx (bpIndex t) x) (<= x)
{-# INLINABLE upperBoundIdx #-}

-- | Rank of a query within one leaf.
--
-- Scanned, not bisected, below @ll = 64@. Bisection does @log2 ll@ comparisons
-- against the scan's @ll/2@, which is the right trade when the leaf is already
-- in cache -- and that is exactly the regime a benchmark of a thousand repeated
-- queries measures, which is how bisection came to be chosen here first. On a
-- realistic query stream the leaf is cold, and then the shape of the accesses
-- matters more than their number: a scan walks forward through one or two cache
-- lines with nothing to wait on, while bisection's addresses each depend on the
-- previous comparison, so its handful of accesses serialise. Measured over 200k
-- distinct queries at @li = 16@, ns per query:
--
-- >   ll          2     4     8    16    32    64
-- >   scan     27.1  36.1  40.8  29.6  40.8  67.4
-- >   bisect   28.3  38.7  46.7  37.3  44.1  59.0
--
-- The crossover is between 32 and 64, where the leaf grows past a couple of
-- cache lines and the comparison count starts to dominate again. @ll@ is a
-- compile-time constant, so the choice below folds away.
--
-- This is what makes a wide leaf affordable, and a wide leaf is what keeps the
-- index small: at a million keys, @ll = 16@ costs 9% over @ll = 2@ and stores an
-- eighth as many separators.
--
-- Two incidental benefits, either way. Neither form needs the sentinel padding,
-- since the partial last leaf is bounded by 'bpSize' directly. And both are
-- @NaN@-safe without the negated predicate the index step needs: no comparison
-- against a @NaN@ is true, so a scan stops at once and a bisection collapses to
-- its lower end, both in range.
leafRank ::
  Key k =>
  U.Vector k ->
  Int ->
  Int ->
  Int ->
  (k -> Bool) ->
  Int
leafRank ks n ll base before
  | ll < 64 = scan base
  | otherwise = bisect base hi
  where
    hi = min n (base + ll)
    scan !i
      | i >= hi = i
      | before (U.unsafeIndex ks i) = scan (i + 1)
      | otherwise = i
    bisect !lo !up
      | lo >= up = lo
      | before (U.unsafeIndex ks mid) = bisect (mid + 1) up
      | otherwise = bisect lo mid
      where
        mid = (lo + up) `quot` 2
{-# INLINE leafRank #-}

-- | The second half of the descent: rank within the leaf the index chose.
--
-- @'B.lowerBoundIdx' index x@ counts the separators below @x@, which is the leaf
-- whose range contains the first key @>= x@. Returning @leaf@ means
-- @s(leaf-1) < x <= s(leaf)@, so if no key in that leaf stops the rank the
-- answer is the start of the next one -- correct, because @s(leaf)@ is that
-- leaf's first key.
--
-- The two bounds must use the /matching/ bound on the index, not both the lower
-- one. The symmetry is tempting and wrong, and it goes wrong exactly when the
-- query /equals a separator/ -- which needs no duplicates at all. Keys
-- @[1,2,3,4]@ at @ll = 2@ have the single separator @3@; a lower-bound index
-- step puts @upperBoundIdx 3@ in leaf 0, which ranks 2 and answers 2 instead of
-- 3. Duplicates only make it easier to hit, by mapping a whole run onto the
-- separator: @[5,5,5,5]@ answers 2 instead of 4.
--
-- Note what is absent next to the B-tree's descent: no virtual size, no
-- exceeding-leaf bookkeeping, no two-branch conversion at the end. Leaves are
-- contiguous, so the position is just @leaf * ll + rank@.
--
-- Keeping that in range depends on how the predicate is written. 'lowerBoundIdx'
-- passes @not (k < x)@ rather than the @k >= x@ it is equivalent to on any total
-- order, and the same comparison costs the same either way -- but a @NaN@ query
-- is not ordered. Every @<@ against a @NaN@ is False, so the negated form stops
-- the rank at the leaf's first key instead of running past its last into the
-- padding, which previously put the position beyond @bpSize@ and made 'lookup'
-- ask for a slice of negative length. Clamping with a @min@ here would also fix
-- it, and measured 14% slower on this path; the predicate costs nothing.
descend ::
  forall li ll k v.
  (B.LineWidth ll, Key k) =>
  BPlusTree li ll k v ->
  Int ->
  (k -> Bool) ->
  Int
descend t leaf before = leafRank (bpKeys t) (bpSize t) ll (leaf * ll) before
  where
    ll = B.lineWidth @ll
{-# INLINE descend #-}

-- | Every value stored under @x@, in input order. Empty if there is none.
--
-- @O(1)@ after the descent: equal keys are adjacent, so the result is a slice
-- sharing the underlying array. The extent is found by scanning forward from
-- the lower bound rather than by a second descent — for the usual handful of
-- matches that is a couple of sequential reads against another random walk down
-- the tree. Use 'rangeIdx' if you expect very long runs of one key.
lookup ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Key k, U.Unbox v) =>
  BPlusTree li ll k v ->
  k ->
  U.Vector v
lookup t x = U.slice lo (scan lo - lo) (bpValues t)
  where
    n = bpSize t
    ks = bpKeys t
    lo = lowerBoundIdx t x
    scan !i
      | i >= n = n
      | U.unsafeIndex ks i == x = scan (i + 1)
      | otherwise = i
{-# INLINABLE lookup #-}

-- | Whether any value is stored under @x@.
member ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Key k) =>
  BPlusTree li ll k v ->
  k ->
  Bool
member t x = i < bpSize t && U.unsafeIndex (bpKeys t) i == x
  where
    i = lowerBoundIdx t x
{-# INLINABLE member #-}

-- | The key at a position of the sorted input, @O(1)@.
keyAt :: Key k => BPlusTree li ll k v -> Int -> k
keyAt t i
  | i < 0 || i >= bpSize t = error (outOfRange "keyAt" i (bpSize t))
  | otherwise = U.unsafeIndex (bpKeys t) i
{-# INLINABLE keyAt #-}

-- | The value at a position of the sorted input, @O(1)@.
valueAt :: U.Unbox v => BPlusTree li ll k v -> Int -> v
valueAt t i
  | i < 0 || i >= bpSize t = error (outOfRange "valueAt" i (bpSize t))
  | otherwise = U.unsafeIndex (bpValues t) i
{-# INLINABLE valueAt #-}

outOfRange :: String -> Int -> Int -> String
outOfRange fn i n =
  "Data.STree.BPlus." ++ fn ++ ": index " ++ show i ++ " out of range for size " ++ show n

-- ---------------------------------------------------------------------------
-- Bulk queries
-- ---------------------------------------------------------------------------

-- | 'lowerBoundIdx' for many keys at once.
--
-- Half of this is vectorised for free. The index is an ordinary
-- "Data.STree.BTree", so the descent that picks a leaf for every query is one
-- call to the existing SIMD kernel; only the final rank inside the chosen leaf
-- stays in Haskell. That is @ll@ scalar comparisons per query against the
-- roughly @height@ scalar ranks the one-at-a-time path does, so the saving grows
-- with the size of the index and shrinks as the leaf widens.
--
-- The consequence for tuning is that it inverts the advice for 'leafWidth'. On
-- the scalar path @ll@ is nearly free and a wider leaf mainly shrinks the index;
-- here the leaf scan is the /only/ scalar work left, so @ll@ becomes the
-- expensive axis. Measure rather than reusing a scalar calibration.
--
-- A fully vectorised descent, leaf included, would need its own C kernel. This
-- needs none.
lowerBoundIdxMany ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Batch.BatchKey k) =>
  BPlusTree li ll k v ->
  U.Vector k ->
  U.Vector Int
lowerBoundIdxMany t = rankInLeaves t (Batch.lowerBoundIdxMany (bpIndex t)) (\x k -> k < x)
{-# INLINABLE lowerBoundIdxMany #-}

-- | 'upperBoundIdx' for many keys at once.
--
-- Note the index step uses 'Batch.upperBoundIdxMany', not the lower-bound one:
-- the two differ exactly when a query equals a separator, and using the wrong
-- one there lands in the previous leaf. See 'descend'.
upperBoundIdxMany ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Batch.BatchKey k) =>
  BPlusTree li ll k v ->
  U.Vector k ->
  U.Vector Int
upperBoundIdxMany t = rankInLeaves t (Batch.upperBoundIdxMany (bpIndex t)) (\x k -> k <= x)
{-# INLINABLE upperBoundIdxMany #-}

-- | Given a vectorised index descent and the leaf predicate, finish each query
-- with a scalar rank in the leaf the index chose.
--
-- The predicates are the negated forms, matching 'descend', and for the same
-- reason: they are equivalent on any total order but a @NaN@ query is not
-- ordered, and only the negated form stops the rank at the leaf's first key
-- rather than running past its last into the padding.
rankInLeaves ::
  forall li ll k v.
  (B.LineWidth ll, Key k) =>
  BPlusTree li ll k v ->
  (U.Vector k -> U.Vector Int) ->
  (k -> k -> Bool) ->
  U.Vector k ->
  U.Vector Int
rankInLeaves t indexStep before xs =
  U.zipWith
    (\leaf x -> leafRank ks n ll (leaf * ll) (before x))
    (indexStep xs)
    xs
  where
    ll = B.lineWidth @ll
    ks = bpKeys t
    n = bpSize t
{-# INLINE rankInLeaves #-}

-- ---------------------------------------------------------------------------
-- Ranges
-- ---------------------------------------------------------------------------

-- | Half-open bounds @[lo, hi)@ of the keys in @[from, to)@.
rangeIdx ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Key k) =>
  BPlusTree li ll k v ->
  k ->
  k ->
  (Int, Int)
rangeIdx t from to = (lo, max lo (lowerBoundIdx t to))
  where
    lo = lowerBoundIdx t from
{-# INLINABLE rangeIdx #-}

-- | The keys in @[from, to)@, in order. @O(1)@ after the two descents — a slice,
-- not a copy.
rangeKeys ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Key k) =>
  BPlusTree li ll k v ->
  k ->
  k ->
  U.Vector k
rangeKeys t from to = case rangeIdx t from to of
  (lo, hi) -> U.slice lo (hi - lo) (bpKeys t)
{-# INLINABLE rangeKeys #-}

-- | The values whose keys are in @[from, to)@, in order. @O(1)@ after the two
-- descents.
rangeValues ::
  forall li ll k v.
  (B.LineWidth li, B.LineWidth ll, Key k, U.Unbox v) =>
  BPlusTree li ll k v ->
  k ->
  k ->
  U.Vector v
rangeValues t from to = case rangeIdx t from to of
  (lo, hi) -> U.slice lo (hi - lo) (bpValues t)
{-# INLINABLE rangeValues #-}

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | The number of entries.
size :: BPlusTree li ll k v -> Int
size = bpSize

-- | Levels of the index, plus one for the leaves.
height :: BPlusTree li ll k v -> Int
height t = B.height (bpIndex t) + 1

-- | Keys per leaf.
leafWidth :: forall li ll k v. B.LineWidth ll => BPlusTree li ll k v -> Int
leafWidth _ = B.lineWidth @ll

-- | Node width of the internal index.
indexWidth :: forall li ll k v. B.LineWidth li => BPlusTree li ll k v -> Int
indexWidth _ = B.lineWidth @li

-- | Bytes held by the leaves, the index and the values together.
--
-- 'Storable' only to get the width of a value; nothing here stores one that
-- way. Requiring 'Key' of the value type instead would demand an ordering and a
-- sentinel of something that is only ever a payload.
sizeInBytes :: forall li ll k v. (Key k, U.Unbox v, Storable v) => BPlusTree li ll k v -> Int
sizeInBytes t =
  U.length (bpKeys t) * keyBytes @k
    + B.sizeInBytes (bpIndex t)
    + U.length (bpValues t) * sizeOf (undefined :: v)

-- | All keys, in order. @O(1)@: this is how they are stored.
keys :: Key k => BPlusTree li ll k v -> U.Vector k
keys t = U.take (bpSize t) (bpKeys t)

-- | All values, in key order. @O(1)@.
values :: BPlusTree li ll k v -> U.Vector v
values = bpValues

-- | The index tree over the separators, for inspection.
indexTree :: BPlusTree li ll k v -> B.BTree li k
indexTree = bpIndex
