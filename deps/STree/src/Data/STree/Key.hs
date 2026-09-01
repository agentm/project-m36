{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The class of key types a 'Data.STree.BTree.BTree' can be built over:
-- the fixed-width signed and unsigned integers, and the two floating-point
-- types.
module Data.STree.Key
  ( Key (..)
  ) where

import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import qualified Data.Vector.Unboxed as U

-- | Keys must be totally ordered, unboxable, and must provide a /sentinel/:
-- a value that compares greater than or equal to every admissible key.
--
-- The sentinel pads the final node of the layout so that a node rank never
-- runs past the end of the data.
--
-- Consequences, which 'Data.STree.BTree.build' rejects up front:
--
--   * no key may be @>= sentinel@;
--   * for floating point keys, no key may be @NaN@ (which breaks 'Ord'
--     altogether).
class (Ord a, U.Unbox a) => Key a where
  -- | Padding value; must be @>=@ every admissible key.
  sentinel :: a

  -- | Size of one key in bytes, used only by
  -- 'Data.STree.BTree.sizeInBytes'. @U.Unbox@ does not expose the element
  -- size, so it is provided here.
  keyBytes :: Int

instance Key Int where
  sentinel = maxBound
  keyBytes = 8
  {-# INLINE sentinel #-}

instance Key Int32 where
  sentinel = maxBound
  keyBytes = 4
  {-# INLINE sentinel #-}

instance Key Int64 where
  sentinel = maxBound
  keyBytes = 8
  {-# INLINE sentinel #-}

instance Key Word where
  sentinel = maxBound
  keyBytes = 8
  {-# INLINE sentinel #-}

instance Key Word32 where
  sentinel = maxBound
  keyBytes = 4
  {-# INLINE sentinel #-}

instance Key Word64 where
  sentinel = maxBound
  keyBytes = 8
  {-# INLINE sentinel #-}

-- | Padded with @+Infinity@ rather than the largest finite value: it satisfies
-- the sentinel contract for strictly more inputs, at no cost.
instance Key Float where
  sentinel = 1 / 0
  keyBytes = 4
  {-# INLINE sentinel #-}

-- | See the 'Float' instance.
instance Key Double where
  sentinel = 1 / 0
  keyBytes = 8
  {-# INLINE sentinel #-}
