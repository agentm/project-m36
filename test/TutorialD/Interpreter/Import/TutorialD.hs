import Test.HUnit
import ProjectM36.Base
import TutorialD.Interpreter.Import.TutorialD
import System.Exit
import qualified Data.Text as T
import System.IO.Temp
import qualified Data.Map as M
import System.IO
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Text.URI

main :: IO ()
main = do 
  tcounts <- runTestTT $ TestList [testTutdFileImport, testTutdHTTPSImport]
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testTutdFileImport :: Test
testTutdFileImport = TestCase $ 
  withSystemTempFile "m.tutd" $ \tempPath handle -> do
    BS.hPut handle (TE.encodeUtf8 "x:=relation{tuple{a 5,b \"spam\"}}; y:=relation{tuple{b \"漢字\"}}")
    hClose handle
    let expectedExpr = MultipleExpr [
          Assign "x" (MakeRelationFromExprs Nothing 
                      $ TupleExprs () [TupleExpr (M.fromList [("a", NakedAtomExpr $ IntegerAtom 5),
                                              ("b", NakedAtomExpr $ TextAtom "spam")])]),
          Assign "y" (MakeRelationFromExprs Nothing 
                      $ TupleExprs () [TupleExpr (M.fromList [("b", NakedAtomExpr (TextAtom "漢字"))])])]
    fileURI <- mkURI (T.pack ("file://" <> tempPath))
    imported <- importTutorialDFromFile fileURI Nothing
    assertEqual "import tutd" (Right expectedExpr) imported

testTutdHTTPSImport :: Test
testTutdHTTPSImport = TestCase $ do
  uri <- mkURI "https://raw.githubusercontent.com/agentm/project-m36/master/test/TutorialD/Interpreter/Import/httpimporttest.tutd"
  let hash = "bae2e29414d9d72e920182185d2b62aefc12aed1cc47b3b330b0e7de7432d831"
      expected = Right (MultipleExpr [Assign "x" (RelationVariable "true" ()),Assign "y" (RelationVariable "false" ())])
  actual <- importTutorialDViaHTTP uri (Just hash)
  assertEqual "github https" expected actual
  
  
