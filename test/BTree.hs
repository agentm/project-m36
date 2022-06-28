import Test.HUnit
import ProjectM36.BTree
import System.Exit
import qualified Data.Vector.Unboxed as V
import Control.Monad

testList :: Test
testList = TestList [testSmallBTrees]

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testSmallBTrees :: Test
testSmallBTrees = TestCase $ do
  forM_ [2..10] $ \b ->
    forM_ [5..50] $ \n -> 
      assertBool ("missing values " <> show b <> ":" <> show n)  (validate b n)

missingVals :: [Int] -> BTree -> [Int]
missingVals vals (BTree (_,vin)) = filter (\v -> v `V.notElem` vin) vals
    
validate :: Branches -> Int -> Bool
validate b n = null $ missingVals [0..n] (build b (V.fromList [0..n]))
