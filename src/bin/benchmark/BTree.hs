{-# LANGUAGE ScopedTypeVariables #-}
import ProjectM36.BTree
import Criterion.Main
import qualified Data.Vector.Unboxed as V
import Control.DeepSeq
import Data.ByteUnits

-- | creates a completely BTree from a uniform distribution of positive integers.

createBTree :: Branches -> Int -> BTree
createBTree branches itemCount = build branches (V.fromList [0..itemCount - 1])
  
main :: IO ()
main = do
  let sizes = map (10 ^) [1..7 :: Int]
      prettyBytes x = getShortHand (getAppropriateUnits (ByteValue (fromIntegral x) Bytes))
  btrees16 <- pure $ force $ map (createBTree 16) sizes
  btrees64 <- pure $ force $ map (createBTree 64) sizes
  btrees100 <- pure $ force $ map (createBTree 100) sizes  
  btrees128 <- pure $ force $ map (createBTree 128) sizes
--  gen <- getStdGen
--  let rands = randomRs gen (0,100)
  
  defaultMain [{-bgroup "create branch-level 4 x " $
               map (\count ->
                       bench (show count) (nf (createBTree 4) count)) sizes,-}
    bgroup "member branch-level 16 x " $
      map (\btree -> bench (show (size btree) <> " " <> prettyBytes (totalBytes btree)) (nf (member 91) btree)) btrees16,

    bgroup "member branch-level 64 x " $
      map (\btree -> bench (show (size btree) <> " " <> prettyBytes (totalBytes btree)) (nf (member 91) btree)) btrees64,

    bgroup "member branch-level 100 x " $
      map (\btree -> bench (show (size btree) <> " " <> prettyBytes (totalBytes btree)) (nf (member 91) btree)) btrees100,
      

    bgroup "member branch-level 128 x " $
      map (\btree -> bench (show (size btree) <> " " <> prettyBytes (totalBytes btree)) (nf (member 91) btree)) btrees128
      
      
    ]
              
