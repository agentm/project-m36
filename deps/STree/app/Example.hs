{-# LANGUAGE DataKinds #-}

-- | A worked example: build a few trees, query one, and dump each as Graphviz.
module Main (main) where

import Data.Int (Int32)
import qualified Data.Vector.Unboxed as U

import Data.STree
import Data.STree.BTree (exceedingLeaves, layout)
import qualified Data.STree.BPlus as BP
import Data.STree.Dot (DotStyle (..), dotStyle, toDot, toDotBPlus, toDotStyled)

main :: IO ()
main = do
  let dat = U.fromList [-3, 2, 4, 11, 35, 60] :: U.Vector Int32
  case build dat :: Either BuildError (BTree 16 Int32) of
    Left err -> putStrLn $ "Could not build the tree: " ++ show err
    Right tree -> do
      putStrLn "Data: -3, 2, 4, 11, 35, 60"
      putStrLn $
        "The lower bound of 11 is at index "
          ++ show (lowerBoundIdx tree (11 :: Int32))
          ++ ", the lower bound of 12 is at index "
          ++ show (lowerBoundIdx tree (12 :: Int32))
      putStrLn $ "The key at index 3 is " ++ show (tree ! 3)
      putStrLn $
        "Height: "
          ++ show (height tree)
          ++ ", keys: "
          ++ show (size tree)
          ++ ", bytes: "
          ++ show (sizeInBytes tree)
      writeFile "example-tree.dot" (toDot tree)
      putStrLn "Wrote example-tree.dot (render with: dot -Tsvg example-tree.dot -o tree.svg)"

  -- Six keys fit in a single 16-key node, so the graph above is one box. To see
  -- the layout as a tree it needs a narrower fanout and more keys. Fanout 3 is
  -- the narrowest supported, and the two trees below are the two cases worth
  -- understanding: a partial deepest level, and a perfectly complete tree.
  narrowExample
    "example-tree-deep.dot"
    "Fourteen keys at fanout 3 -- the general case, with a partial deepest level:"
    (U.fromList [-42, -3, 0, 2, 4, 7, 11, 19, 23, 35, 41, 48, 60, 77])

  -- Eighty is 3^4-1, the exact capacity of a four-level ternary-fanout tree, so
  -- this one comes out perfectly complete: every node holds two keys, every
  -- internal node has three children, and there is no padding and no dummy
  -- child anywhere. The values are an arbitrary sorted assortment rather than a
  -- run of consecutive integers, so that the separators the build chooses are
  -- visibly a property of the layout and not of the data.
  narrowExample
    "example-tree-4-levels.dot"
    "Eighty scattered keys at fanout 3 -- 3^4-1, so the tree is exactly complete:"
    ( U.fromList
        [ -985, -975, -974, -961, -954, -927, -923, -920, -889, -886, -851, -824, -794,
          -780, -699, -684, -647, -637, -608, -598, -596, -584, -568, -552, -544, -529,
          -496, -485, -446, -440, -432, -428, -423, -380, -378, -348, -339, -328, -327,
          -278, -229, -221, -204, -178, -169, -147, -130, -116, -40, 9, 15, 34, 39, 128,
          169, 182, 268, 300, 407, 408, 413, 416, 420, 424, 501, 510, 537, 579, 593, 614,
          650, 704, 735, 755, 767, 875, 876, 896, 901, 931
        ]
    )

  mapExample

-- | The same keys again, but as a map: values attached, and every key down in
-- the leaves with the index holding only copies of some of them. Fifteen keys
-- in leaves of two make eight leaves and so seven separators, which at fanout 3
-- is a two-level index -- three levels in all, the same depth as the B-tree
-- above. The two graphs are worth opening together.
--
-- 19 is stored twice, with different values, and it is placed to show two things
-- at once. The pair straddles the boundary between leaf 3 and leaf 4, so a
-- lookup for it spans two leaves; and because it starts a leaf, it is also one
-- of the separators, which is the case where the lower and upper bounds must
-- descend the index differently. An odd number of keys also leaves one padding
-- slot in the last leaf, drawn as @~@.
mapExample :: IO ()
mapExample = do
  putStrLn ""
  putStrLn "The same keys as a map, fanout 3, with a payload per key and 19 stored twice:"
  let ks = U.fromList [-42, -3, 0, 2, 4, 7, 11, 19, 19, 23, 35, 41, 48, 60, 77] :: U.Vector Int32
      vs = U.fromList [100, 200 .. 1500] :: U.Vector Int32
  case BP.build ks vs :: Either BP.BuildError (BP.BPlusTree 2 2 Int32 Int32) of
    Left err -> putStrLn ("  could not build: " ++ show err)
    Right t -> do
      putStrLn $
        "  "
          ++ show (BP.size t)
          ++ " keys, "
          ++ show (BP.height t)
          ++ " levels, leaves of "
          ++ show (BP.leafWidth t)
          ++ ", "
          ++ show (size (BP.indexTree t))
          ++ " separators in the index"
      putStrLn $ "  separators: " ++ show (U.toList (toVector (BP.indexTree t)))
      putStrLn $ "  every key is still in the leaves: " ++ show (U.toList (BP.keys t))
      putStrLn $ "  lookup 23    -> " ++ show (U.toList (BP.lookup t 23))
      putStrLn $ "  lookup 24    -> " ++ show (U.toList (BP.lookup t 24)) ++ " (absent)"
      putStrLn $
        "  lookup 19    -> "
          ++ show (U.toList (BP.lookup t 19))
          ++ " (stored twice, at positions "
          ++ show (BP.lowerBoundIdx t 19)
          ++ " and "
          ++ show (BP.upperBoundIdx t 19 - 1)
          ++ ", either side of a leaf boundary)"
      putStrLn $ "  keys in [0,20) -> " ++ show (U.toList (BP.rangeKeys t 0 20))
      putStrLn $ "  their values   -> " ++ show (U.toList (BP.rangeValues t 0 20))
      writeFile "example-bplus.dot" (toDotBPlus t)
      putStrLn "  wrote example-bplus.dot"

-- | Build at fanout 3, describe the shape, and dump the graph.
--
-- The level-by-level breakdown is the point of this: the tree is stored as one
-- flat array with level @d@ beginning at offset @3^d - 1@ and holding @3^d@
-- nodes of two keys each, and printing it that way next to the graph shows how
-- the picture and the array correspond. Node names in the graph are offsets into
-- the same array, so @n8@ is the node starting at @layout ! 8@.
narrowExample :: FilePath -> String -> U.Vector Int32 -> IO ()
narrowExample path note keys = do
  putStrLn ""
  putStrLn note
  case build keys :: Either BuildError (BTree 2 Int32) of
    Left err -> putStrLn $ "  could not build: " ++ show err
    Right t -> do
      let arr = layout t
          e = exceedingLeaves t
          internalNodes = (size t - e) `div` 2
          leafNodes = (e + 1) `div` 2
      putStrLn $
        "  "
          ++ show (size t)
          ++ " keys, "
          ++ show (height t)
          ++ " levels, "
          ++ show (internalNodes + leafNodes)
          ++ " nodes"
          ++ (if U.length arr == size t then ", no padding" else ", " ++ show (U.length arr - size t) ++ " padding slot(s)")
      mapM_ putStrLn
        [ "  level "
          ++ show d
          ++ " (offset "
          ++ show start
          ++ "): "
          ++ show (U.toList (U.slice start len arr))
        | d <- [0 .. height t - 1]
        , let start = 3 ^ d - 1
        , let len = min (U.length arr - start) (2 * 3 ^ d)
        ]
      putStrLn $ "  lower bound of 20 is at index " ++ show (lowerBoundIdx t (20 :: Int32))
      writeFile path (toDotStyled dotStyle {dotArray = True} t)
      putStrLn $ "  wrote " ++ path
