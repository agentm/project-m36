import TutorialD.Interpreter.TestBase
import System.Exit
import Test.HUnit
import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.Error
import ProjectM36.DataTypes.Maybe
import qualified Data.Vector as V
import qualified Data.Map as M

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testBasicAtomFunction,
                                  testExceptionAtomFunction,
                                  testErrorAtomFunction,
                                  testNoArgumentAtomFunction,
                                  testArgumentTypeMismatch,
                                  testPolymorphicReturnType,
                                  testScriptedTypeVariable
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
  
testPolymorphicReturnType :: Test  
testPolymorphicReturnType = TestCase $ do
  --test that polymorphic function return type resolves to concrete type using fromMaybe builtin atom function
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback  
  let funcAtomExpr = FunctionAtomExpr "fromMaybe" [NakedAtomExpr (IntAtom 5),
                                                   NakedAtomExpr maybeAtom] ()
      maybeAtom = ConstructedAtom "Just" (maybeAtomType IntAtomType) [IntAtom 3]
      relExpr = MakeRelationFromExprs Nothing [TupleExpr (M.singleton "x" funcAtomExpr)]
  mRelType <- typeForRelationalExpr sess conn relExpr
  case mRelType of
    Left err -> assertFailure (show err)
    Right relType -> do
      let expectedRetAttrs = V.fromList [Attribute "x" IntAtomType]
      assertEqual "fromMaybe type" expectedRetAttrs (attributes relType)
      mRes <- executeRelationalExpr sess conn relExpr
      let expectedRel = mkRelationFromList (V.fromList [Attribute "x" IntAtomType]) [[IntAtom 3]]
      assertEqual "fromMaybe result" expectedRel mRes
  --test that type mismatch occurs for different types appearing in same type variable
  let failFuncAtomExpr = FunctionAtomExpr "fromMaybe" [NakedAtomExpr (IntAtom 5),
                                                       NakedAtomExpr mismatchAtom] ()
      mismatchAtom = ConstructedAtom "Just" (maybeAtomType TextAtomType) [TextAtom "fail"]
      failRelExpr = MakeRelationFromExprs Nothing [TupleExpr (M.singleton "x" failFuncAtomExpr)]
  mFailRes <- executeRelationalExpr sess conn failRelExpr
  let expectedErr = Left (AtomFunctionTypeVariableMismatch "a" IntAtomType TextAtomType)
  assertEqual "expected type variable mismatch" expectedErr mFailRes
                                                       
--test that a user can create a function with a type variable argument
testScriptedTypeVariable :: Test
testScriptedTypeVariable = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"idTest\" a -> Either AtomFunctionError a \"(\\\\(x:_) -> pure x) :: [Atom] -> Either AtomFunctionError Atom\""
  let attrs = [Attribute "x" IntAtomType]
      funcAtomExpr = FunctionAtomExpr  "idTest" [NakedAtomExpr (IntAtom 3)] ()
      tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = mkRelationFromList (V.fromList attrs) [[IntAtom 3]]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)

  assertEqual "id function equality" expectedResult result
  