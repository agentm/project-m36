-- implements a write-once, read-many B+tree layed out using the eytzinger format for less CPU cache churn
{-# LANGUAGE TypeApplications, DeriveAnyClass, DeriveGeneric #-}
module ProjectM36.BTree (build, member, BTree, Branches, Level, Index, size, totalBytes) where
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad
import Control.DeepSeq
import Data.Maybe
--import Text.Printf
import GHC.Generics

import Debug.Trace

data BTree = BTree {
  branches :: Branches,
  elemCount :: Int,
  vec :: V.Vector Int }
  deriving (Show, Generic, NFData)

type Branches = Int
type Level = Int
type Index = Int

-- | return slicepoints for the b slices (returns b-1 indexes so that b nearly-equal slices are made)
sliceIndexes :: Branches -> Int -> [Int]
sliceIndexes b n | n < b = []
sliceIndexes b n | n == b = [0..b-2]
sliceIndexes b n = map islice [1 .. nodec]
  where
    nodec = b - 1
    islice x = (n * x) `divr` b
     
divr :: Integral a => a -> a -> a    
divr a b =
  --the `round` function rounds based even/odd integers- we just want dumb, unbiased rounding
  -- warning: this does not round properly
  if res - fromIntegral intres < 0.5 then
    intres
    else
    intres + 1
  where
    intres = floor res
    res :: Double
    res = fromIntegral a / fromIntegral b

-- | Calculate the size needed to flatten a sorted list into an Eytzinger b-tree (which may be sparse).
vectorSize :: Branches -> Int -> (Int, Int)
vectorSize b n = firstFit (zip (availableSizes b) [0..])
  where
    firstFit [] = (0,0) -- impossible
    firstFit ((size',i):ss) =
      if size' >= n then
        (size', i)
        else
        firstFit ss

-- | return list of vector sizes available for tree of node-branch factor b.
availableSizes :: Int -> [Int]
availableSizes b = takeWhile (>0) $ scanl (\acc x -> acc + (b - 1) * b ^ x) (b - 1) [(1 :: Int) ..]

-- | slice a vector into b+1 equally-sized slices and return the b roots
treeSlicer :: Branches -> V.Vector Int -> ([Int], [V.Vector Int])
treeSlicer b vin =
  if n >= b then
    (roots, slicesThroughIndexes sliceindexes vin)
  else
    (V.toList vin, [])
  where
    n = V.length vin
    sliceindexes = sliceIndexes b n
    roots = map (vin V.!) sliceindexes

-- cut up a vector into new vectors at the indexes but do not include the indexed value
slicesThroughIndexes :: [Int] -> V.Vector Int -> [V.Vector Int]
slicesThroughIndexes [] vin = [vin]
slicesThroughIndexes (i:is) vin =
  case V.splitAt i vin of
    (s1, sr) -> s1 : slicesThroughIndexes (map (\x -> x - i - 1) is) (V.tail sr)

sparseVal :: Int
sparseVal = -1 -- maxBound

-- the logical offset path within a tree, starting with the leaves (not the root).
type TreePath = [Int]

{-0
3 6 9 12
15 18 21 24
-}
-- | Returns the offset in the eytzinger encoding of the b-tree to the start of the (b-1) roots.
offsetForPath :: Branches -> TreePath -> Int
offsetForPath _ [] = 0
offsetForPath b path =
  levelOffset + --base offset to get to correct level in tree
  foldr (\(o, ri) acc -> 
           acc +
           intraLevelOffset o ri
           ) 0 (zip path [pathLen, pathLen - 1 .. 1])
  where
    pathLen = length path
    kBlockSize l = (0 : availableSizes b) !! l
    kRootSize l = (b - 1) * (b ^ (l - 1)) -- 0, 3, 3*4, 3*4^2
    levelOffset = kBlockSize pathLen
    intraLevelOffset o ri = --traceShow ("o", o, "ri", ri, "i", i, "kRootSize", kRootSize ri)
      o * kRootSize ri
                       
-- | uses a sorted list to create a static Eytzinger representation of the b-tree- this structure does not support mutability. Since Project:M36 writes data using a WORM strategy, the Eytzinger is ideal since the structure emphasizes cache locality, reduces pointer indirection, and is quite compact with no need for extraneous padding.

--should build take a streamly stream or a list instead so that we won't need the sorted list in memory?
build :: Branches -> V.Vector Int -> BTree
build b sortedList = --input list should be de-duplicated to reduce b-tree size, but is otherwise a non-fatal construction of a b-tree (with some duplicate nodes)
  BTree {
    branches = b,
    elemCount = V.length sortedList,
    vec = vout}
  where
    n = V.length sortedList
    ksize = ceiling (logBase @Double (fromIntegral (b - 1)) (fromIntegral n))
    newArrSize = availableSizes b !! (ksize - 1)
    vout =
     V.create $ do
      --traceShowM ("ksize" :: String, ksize, "newArrSize" :: String, newArrSize, "n" :: String, n)
      v <- VM.replicate newArrSize sparseVal
      --split input list into `b` slices, extract the b roots by slicing the array into three equal-sized slices
      let writelevel path items = do
            let (roots, childSlices) = treeSlicer b items
                childSliceSizes = map V.length childSlices
                almostEqualSized = foldr (\x acc ->
                                            case acc of
                                              Nothing -> Nothing
                                              Just acc' -> if abs (acc' - x) > 2 then Nothing else Just x) (Just (head childSliceSizes)) childSliceSizes
            --first lay out all root-k values, then, k+1, etc.
            --space needed for each k level is (b-1) * b^k elements
            --traceShowM ("vsize" :: String, V.length items, "k" :: String, (k:: Int), "ksize" :: String, ksize, "roots" :: String, roots, "childSizes" :: String , map V.length childSlices, "childVecs" :: String, childSlices, "path" :: String, path)
            when (isNothing almostEqualSized) $ error ("uneven slices" <> show childSliceSizes)
            --write roots
            forM_ (zip roots [0..]) $ \(r,i) -> do
              let pos = offsetForPath b path + i
                  --dbgStr :: String
                  -- dbgStr = printf "%d + %d * (%d - 1) * (%d ^ %d) + %d" kOffset offset b b k i
                  -- dbgStr = printf "offset %d blockOffset %d k %d" offset blockOffset k
                  --dbgStr = printf "path %s pos %d" (show path) pos
              --traceShowM ("write" :: String, dbgStr)
              oldval <- VM.read v pos
              when (oldval /= sparseVal) $ traceShowM ("overwrite pos" :: String,
                                                        pos, "oldval" :: String,
                                                        oldval, "newval" :: String,
                                                        r)
              --traceShowM ("write val " <> show r <> " at " <> show pos)
              VM.write v pos r
            forM_ (zip childSlices [0..]) $ \(childVec, i) -> do
{-              let childRootOffset = kOffset + i * (b - 1) ^ k
                  kOffset = (0 : availableSizes b) !! (k + 1)-}
              unless (V.null childVec) $
                writelevel (i : path) childVec
      writelevel [] sortedList
      pure v

{- 0..16
5 should be written to pos
   [   ]/ [     ]  [     ]  [       ]  [       ] / [      ]  [      ] [      ] [      ] [      ]
k  0 1 2  3  4  5  6  7  8  9  10  11  12 13 14    15 16 17  18 19 20 21 22 23 24 25 26 27 28 29
0  4 9 13 
1         0  1  2  5  6  7  10 11  12  14 15 16
2                                                            3yes

4 * 3 ^ k

                          0                           n=1
     3              6          9            12        n=4
15 18 21 24    27 30 33 36  39 42 45 48  51 54 57 60  n=16 k+1 offset + (3 * 4 ^ 1) * parentOffset + 3 * childOffset


                          0                           k=0
     1              2          3            4         k=1 offset
5  6  7  8       9 10 11 12  13 14 15 16  17 18 19 20 k=2 
-}

{-indexForRoots :: Branches -> Level -> Index -> Int
indexForRoots b k i = ((0 : availableSizes b) !! k) + i * (b - 1) 
  -}
  
member :: Int -> BTree -> Bool
member needle bt = memberk 0 []
  where
    b = branches bt
    vin = vec bt
    (_,maxk) = vectorSize b (V.length vin) -- we don't care about the actual number of items in the tree
    memberk :: Level -> TreePath -> Bool
    memberk k _ | k > maxk = False
    memberk k path =
      let startIndex = offsetForPath b path
          roots = V.slice startIndex (b - 1) vin
      in 
        case gteBinarySearch needle roots of
          Nothing -> -- follow right-most branch
            memberk (k+1) (b - 1 : path)
          Just gteIndex ->
            if roots V.! gteIndex == needle then
              --found it!
              True
            else
              --search left branch of this root
              memberk (k + 1) (gteIndex : path)

-- | Search a sorted vector using binary search to find the first index >= to the needle, but takes into account the sparseVal.
gteBinarySearch :: Int -> V.Vector Int -> Maybe Int
gteBinarySearch needle haystack =
  if V.null haystack then
    Nothing
    else
    bsearch 0 (V.length haystack - 1)
  where
    bsearch low high
      | low == high =
        if haystack V.! low >= needle then
          Just low
          else
          Nothing
      | otherwise = 
        let index = (high + low) `div` 2
            item = haystack V.! index
        in
          if item == needle then
            Just index
          else if item < needle && item /= sparseVal then do
            --we need to search to higher indexes
            bsearch (index + 1) high
          else
            --search lower indexes
            bsearch low index

size :: BTree -> Int
size = elemCount

totalBytes :: BTree -> Int
totalBytes bt = V.length (vec bt)
{-
-- | Useful for joins using two btrees.
intersect :: BTree -> BTree -> [Int]
intersect bta btb =
  go = 
-}
