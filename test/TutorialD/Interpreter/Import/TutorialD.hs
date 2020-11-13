import Test.HUnit
import ProjectM36.Base
import TutorialD.Interpreter.Import.TutorialD
import System.Exit
import System.IO.Temp
import qualified Data.Map as M
import System.IO
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

main :: IO ()
main = do 
  tcounts <- runTestTT $ TestList [testTutdImport]
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testTutdImport :: Test
testTutdImport = TestCase $ 
  withSystemTempFile "m.tutd" $ \tempPath handle -> do
    BS.hPut handle (TE.encodeUtf8 "x:=relation{tuple{a 5,b \"spam\"}}; y:=relation{tuple{b \"漢字\"}}")
    hClose handle
    let expectedExpr = MultipleExpr [
          Assign "x" (MakeRelationFromExprs Nothing 
                      $ TupleExprs () [TupleExpr (M.fromList [("a", NakedAtomExpr $ IntegerAtom 5),
                                              ("b", NakedAtomExpr $ TextAtom "spam")])]),
          Assign "y" (MakeRelationFromExprs Nothing 
                      $ TupleExprs () [TupleExpr (M.fromList [("b", NakedAtomExpr (TextAtom "漢字"))])])]
    imported <- importTutorialD tempPath
    assertEqual "import tutd" (Right expectedExpr) imported

