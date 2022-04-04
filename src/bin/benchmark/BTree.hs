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
  let sizes = map (10 ^) [8] 
  btrees <- pure $ force $ map (createBTree 4) sizes
  defaultMain [{-bgroup "create branch-level 4 x " $
               map (\count ->
                       bench (show count) (nf (createBTree 4) count)) sizes,-}
    bgroup "member branch-level 4 x " $
      map (\btree -> bench (show (size btree) <> " " <> getShortHand (ByteValue (fromIntegral (size btree * 8)) Bytes)) (nf (member 90) btree)) btrees
                                                          ]
              
