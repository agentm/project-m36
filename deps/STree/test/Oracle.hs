{-# LANGUAGE BangPatterns #-}

-- | The specification the tree is checked against: plain binary search over
-- the original sorted array.
module Oracle
  ( lowerBoundRef
  , upperBoundRef
  ) where

import qualified Data.Vector.Unboxed as U

-- | Index of the first element @>= x@.
lowerBoundRef :: (Ord a, U.Unbox a) => U.Vector a -> a -> Int
lowerBoundRef v x = go 0 (U.length v)
  where
    go !lo !hi
      | lo >= hi = lo
      | U.unsafeIndex v mid < x = go (mid + 1) hi
      | otherwise = go lo mid
      where
        mid = (lo + hi) `quot` 2

-- | Index of the first element @> x@.
upperBoundRef :: (Ord a, U.Unbox a) => U.Vector a -> a -> Int
upperBoundRef v x = go 0 (U.length v)
  where
    go !lo !hi
      | lo >= hi = lo
      | U.unsafeIndex v mid <= x = go (mid + 1) hi
      | otherwise = go lo mid
      where
        mid = (lo + hi) `quot` 2
