import ProjectM36.BTree
import Criterion.Main
import qualified Data.Vector.Unboxed as V

-- | creates a completely BTree from a uniform distribution of positive integers.

createBTree :: Branches -> Int -> BTree
createBTree branches itemCount = build branches (V.fromList [0..itemCount])
  
main :: IO ()
main = do
  defaultMain [bgroup "create branch-level 4 x " $ map (\count ->
                bench (show count) (nf (createBTree 4) count)) [100, 1000, 10000, 100000]]
              
