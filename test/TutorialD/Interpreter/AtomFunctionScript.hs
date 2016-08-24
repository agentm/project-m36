import TutorialD.Interpreter.TestBase
import System.Exit
import Test.HUnit
import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Vector as V
import qualified Data.Map as M

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testBasicAtomFunction,
                                  testErrorAtomFunction])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

--add an atom function and run it
testBasicAtomFunction :: Test
testBasicAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Int -> Int \"\\\\(IntAtom x:xs) -> IntAtom x\""
  let attrs = [Attribute "x" IntAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [NakedAtomExpr (IntAtom 3)]
      tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = mkRelationFromList (V.fromList attrs) [[IntAtom 3]]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)

  assertEqual "simple atom function equality" expectedResult result
  
--add an atom function which bombs out  
testErrorAtomFunction :: Test
testErrorAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Int -> Int \"\"\"\\(IntAtom x:xs) -> (error (show 1) ) :: Atom\"\"\""
  let attrs = [Attribute "x" IntAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [NakedAtomExpr (IntAtom 3)]
      tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = Left (UnhandledExceptionError "1")
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)
  assertEqual "catch error exception from script" expectedResult result
  
testNoArgumentAtomFunction :: Test
testNoArgumentAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Int -> Int \"\"\"\\(x :: [Atom]) -> IntAtom 5\"\"\""
  let attrs = [Attribute "x" IntAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" []
      tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = mkRelationFromList (V.fromList attrs) [[IntAtom 5]]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)
  assertEqual "no argument scripted function" expectedResult result
  
  