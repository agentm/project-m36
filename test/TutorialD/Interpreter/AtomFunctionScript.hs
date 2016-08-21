import TutorialD.Interpreter.TestBase
import System.Exit
import Test.HUnit
import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.Base
import qualified Data.Vector as V
import qualified Data.Map as M

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testBasicAtomFunction])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testBasicAtomFunction :: Test
testBasicAtomFunction = TestCase $ do
  --add an atom function and run it
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Int -> Int \"\\(IntAtom x:xs) -> IntAtom x\""
  let attrs = [Attribute "x" IntAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [NakedAtomExpr (IntAtom 3)]
      tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = mkRelationFromList (V.fromList attrs) [[IntAtom 3]]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)

  assertEqual "simple atom function equality" expectedResult result