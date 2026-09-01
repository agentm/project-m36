-- | A static, pointer-free, cache-friendly B-tree.
--
-- > import qualified Data.Vector.Unboxed as U
-- > import Data.STree
-- >
-- > main = do
-- >   let Right t = build (U.fromList [-3, 2, 4, 11, 35, 60]) :: Either BuildError (BTree 16 Int32)
-- >   print (lowerBoundIdx t 11)  -- 3
-- >   print (lowerBoundIdx t 12)  -- 4
-- >   print (t ! 3)               -- 11
--
-- See "Data.STree.BTree" for the details of the layout.
module Data.STree
  ( -- * The tree
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

    -- * Keys
  , Key (..)
  ) where

import Data.STree.BTree
import Data.STree.Key
