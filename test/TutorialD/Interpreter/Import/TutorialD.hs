import Test.HUnit
import ProjectM36.Base
import TutorialD.Interpreter.Import.TutorialD
import System.Exit
import System.IO.Temp
import qualified Data.Map as M
import System.IO

main :: IO ()
main = do 
  tcounts <- runTestTT $ TestList [testTutdImport]
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testTutdImport :: Test
testTutdImport = TestCase $ 
  withSystemTempFile "m.tutd" $ \tempPath handle -> do
    hPutStrLn handle "x:=relation{tuple{a 5,b \"spam\"}}"
    hClose handle
    let expectedExpr = MultipleExpr [Assign "x" (MakeRelationFromExprs Nothing [TupleExpr (M.fromList [("a",NakedAtomExpr $ IntegerAtom 5),("b",NakedAtomExpr $ TextAtom "spam")])])]
    imported <- importTutorialD tempPath
    assertEqual "import tutd" (Right expectedExpr) imported
