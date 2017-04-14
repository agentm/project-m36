import TutorialD.Interpreter.TestBase
import System.Exit
import Test.HUnit
import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.Error
import ProjectM36.Attribute
import qualified Data.Vector as V
import qualified Data.Map as M

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testBasicAtomFunction,
                                  testExceptionAtomFunction,
                                  testErrorAtomFunction,
                                  testNoArgumentAtomFunction,
                                  testArgumentTypeMismatch
                                  ])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

--add an atom function and run it
testBasicAtomFunction :: Test
testBasicAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Int -> Either AtomFunctionError Int \"(\\\\(IntAtom x:xs) -> pure (IntAtom x)) :: [Atom] -> Either AtomFunctionError Atom\""
  let attrs = [Attribute "x" IntAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [NakedAtomExpr (IntAtom 3)] ()
      tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = mkRelationFromList (V.fromList attrs) [[IntAtom 3]]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)

  assertEqual "simple atom function equality" expectedResult result
  
--add an atom function which bombs out  
testExceptionAtomFunction :: Test
testExceptionAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Int -> Either AtomFunctionError Int \"\"\"(\\(IntAtom x:xs) -> (error (show 1))) :: [Atom] -> Either AtomFunctionError Atom\"\"\""
  let attrs = [Attribute "x" IntAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [NakedAtomExpr (IntAtom 3)] ()
      tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)
  assertBool "catch error exception from script" (case result of
                                                     Left (UnhandledExceptionError _) -> True
                                                     _ -> False)
    
testErrorAtomFunction :: Test    
testErrorAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"errorAtom\" Int -> Either AtomFunctionError Int \"\"\"(\\(IntAtom x:xs) -> Left (AtomFunctionUserError \"user\")) :: [Atom] -> Either AtomFunctionError Atom\"\"\""
  
testNoArgumentAtomFunction :: Test
testNoArgumentAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Either AtomFunctionError Int \"\"\"(\\x -> pure (IntAtom 5)) :: [Atom] -> Either AtomFunctionError Atom\"\"\""
  let attrs = [Attribute "x" IntAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [] ()
      tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = mkRelationFromList (V.fromList attrs) [[IntAtom 5]]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)
  assertEqual "no argument scripted function" expectedResult result
  
testArgumentTypeMismatch :: Test
testArgumentTypeMismatch = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Int -> Either AtomFunctionError Int \"\"\"(\\(IntAtom x:_) -> pure $ TextAtom \"wrong type\") :: [Atom] -> Either AtomFunctionError Atom\"\"\""
  let tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [NakedAtomExpr (IntAtom 3)] ()
      attrs = [Attribute "x" IntAtomType]
      expectedResult = Left (AtomTypeMismatchError IntAtomType TextAtomType)
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)
  assertEqual "type mismatch not detected" expectedResult result