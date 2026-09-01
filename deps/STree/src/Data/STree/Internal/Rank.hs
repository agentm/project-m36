{-# LANGUAGE BangPatterns #-}

-- | The node-level rank: the innermost operation of a search.
--
-- The vectorised way to do this is one compare-to-mask per register, ORing the
-- per-register masks together and taking a count of trailing zeros. GHC exposes
-- no compare-to-mask SIMD primops (and none at all on aarch64 as of 9.10), so
-- the mask is folded scalar-ly here — but in that shape rather than as an
-- early-exit loop, which keeps the work per node constant and makes this
-- directly comparable to the vectorised kernel in @cbits\/stree_simd.c@ that
-- "Data.STree.Batch" uses.
module Data.STree.Internal.Rank
  ( blockRank
  , refBlockRank
  ) where

import Data.Bits (countTrailingZeros, setBit, shiftL, (.|.))
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U

-- | @blockRank l stops keys off@ is the index of the first of the @l@ keys at
-- @keys[off ..]@ that satisfies @stops@, or @l@ if none does.
--
-- For a node whose keys are sorted, that is the node rank of the query:
-- passing @(>= x)@ gives the number of keys @< x@, which is a lower bound, and
-- passing @(> x)@ gives the number of keys @<= x@, which is an upper bound.
--
-- Preconditions, unchecked: @0 < l <= 64@ and @off + l <= length keys@.
blockRank :: U.Unbox a => Int -> (a -> Bool) -> U.Vector a -> Int -> Int
blockRank l stops keys off = countTrailingZeros (go 0 bound)
  where
    -- Bit @l@ is set so that the count of trailing zeros reports @l@ when no
    -- key stops the descent. For l == 64 there is no such bit, and we rely on
    -- 'countTrailingZeros' returning the width for a zero input.
    bound :: Word64
    bound = if l < 64 then setBit 0 l else 0

    go :: Int -> Word64 -> Word64
    go !i !m
      | i >= l = m
      | otherwise =
          let b = if stops (U.unsafeIndex keys (off + i)) then 1 else 0
           in go (i + 1) (m .|. (b `shiftL` i))
{-# INLINE blockRank #-}

-- | Independent, obviously-correct formulation, used to differentially test
-- 'blockRank'. Not used by the tree itself.
refBlockRank :: U.Unbox a => Int -> (a -> Bool) -> U.Vector a -> Int -> Int
refBlockRank l stops keys off =
  length (takeWhile (not . stops) [U.unsafeIndex keys (off + i) | i <- [0 .. l - 1]])
