{-# LANGUAGE CPP #-}
import System.Exit
import Test.HUnit
#ifdef PM36_HASKELL_SCRIPTING
import TutorialD.Interpreter.TestBase
import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.DataTypes.Maybe
import qualified Data.Vector as V
import qualified Data.Map as M
#endif

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
main :: IO ()
main = do
  tcounts <- runTestTT (TestList [
#ifdef PM36_HASKELL_SCRIPTING
    testBasicAtomFunction,
    testExceptionAtomFunction,
    testErrorAtomFunction,
    testNoArgumentAtomFunction,
    testArgumentTypeMismatch,
    testPolymorphicReturnType,
    testScriptedTypeVariable
#endif    
    ])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

#ifdef PM36_HASKELL_SCRIPTING
--add an atom function and run it
testBasicAtomFunction :: Test
testBasicAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Integer -> Either AtomFunctionError Integer \"(\\\\(IntegerAtom x:xs) -> pure (IntegerAtom x)) :: [Atom] -> Either AtomFunctionError Atom\""
  let attrs = [Attribute "x" IntegerAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [NakedAtomExpr (IntegerAtom 3)] ()
      tupleExprs = [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = mkRelationFromList (V.fromList attrs) [[IntegerAtom 3]]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) (TupleExprs () tupleExprs))

  assertEqual "simple atom function equality" expectedResult result
  
--add an atom function which bombs out  
testExceptionAtomFunction :: Test
testExceptionAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Integer -> Either AtomFunctionError Integer \"\"\"(\\(IntegerAtom x:xs) -> (error (show 1))) :: [Atom] -> Either AtomFunctionError Atom\"\"\""
  let attrs = [Attribute "x" IntegerAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [NakedAtomExpr (IntegerAtom 3)] ()
      tupleExprs = TupleExprs () [TupleExpr (M.singleton "x" funcAtomExpr)]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)
  assertBool "catch error exception from script" (case result of
                                                     Left (UnhandledExceptionError _) -> True
                                                     _ -> False)
    
testErrorAtomFunction :: Test    
testErrorAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"errorAtom\" Integer -> Either AtomFunctionError Integer \"\"\"(\\(IntegerAtom x:xs) -> Left (AtomFunctionUserError \"user\")) :: [Atom] -> Either AtomFunctionError Atom\"\"\""
  
testNoArgumentAtomFunction :: Test
testNoArgumentAtomFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Either AtomFunctionError Integer \"\"\"(\\x -> pure (IntegerAtom 5)) :: [Atom] -> Either AtomFunctionError Atom\"\"\""
  let attrs = [Attribute "x" IntegerAtomType]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [] ()
      tupleExprs = TupleExprs () [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = mkRelationFromList (V.fromList attrs) [[IntegerAtom 5]]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)
  assertEqual "no argument scripted function" expectedResult result
  
testArgumentTypeMismatch :: Test
testArgumentTypeMismatch = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"mkTest\" Integer -> Either AtomFunctionError Integer \"\"\"(\\(IntegerAtom x:_) -> pure $ TextAtom \"wrong type\") :: [Atom] -> Either AtomFunctionError Atom\"\"\""
  let tupleExprs = TupleExprs () [TupleExpr (M.singleton "x" funcAtomExpr)]
      funcAtomExpr = FunctionAtomExpr  "mkTest" [NakedAtomExpr (IntegerAtom 3)] ()
      attrs = [Attribute "x" IntegerAtomType]
      expectedResult = Left (AtomTypeMismatchError IntegerAtomType TextAtomType)
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)
  assertEqual "type mismatch not detected" expectedResult result
  
testPolymorphicReturnType :: Test  
testPolymorphicReturnType = TestCase $ do
  --test that polymorphic function return type resolves to concrete type using fromMaybe builtin atom function
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback  
  let funcAtomExpr = FunctionAtomExpr "fromMaybe" [NakedAtomExpr (IntegerAtom 5),
                                                   NakedAtomExpr maybeAtom] ()
      maybeAtom = ConstructedAtom "Just" (maybeAtomType IntegerAtomType) [IntegerAtom 3]
      relExpr = MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (M.singleton "x" funcAtomExpr)])
  mRelType <- typeForRelationalExpr sess conn relExpr
  case mRelType of
    Left err -> assertFailure (show err)
    Right relType -> do
      let expectedRetAttrs = V.fromList [Attribute "x" IntegerAtomType]
      assertEqual "fromMaybe type" expectedRetAttrs (attributes relType)
      mRes <- executeRelationalExpr sess conn relExpr
      let expectedRel = mkRelationFromList (V.fromList [Attribute "x" IntegerAtomType]) [[IntegerAtom 3]]
      assertEqual "fromMaybe result" expectedRel mRes
  --test that type mismatch occurs for different types appearing in same type variable
  let failFuncAtomExpr = FunctionAtomExpr "fromMaybe" [NakedAtomExpr (IntegerAtom 5),
                                                       NakedAtomExpr mismatchAtom] ()
      mismatchAtom = ConstructedAtom "Just" (maybeAtomType TextAtomType) [TextAtom "fail"]
      failRelExpr = MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (M.singleton "x" failFuncAtomExpr)])
  mFailRes <- executeRelationalExpr sess conn failRelExpr
  let expectedErr = Left (AtomFunctionTypeVariableMismatch "a" IntegerAtomType TextAtomType)
  assertEqual "expected type variable mismatch" expectedErr mFailRes
                                                       
--test that a user can create a function with a type variable argument
testScriptedTypeVariable :: Test
testScriptedTypeVariable = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sess conn "addatomfunction \"idTest\" a -> Either AtomFunctionError a \"(\\\\(x:_) -> pure x) :: [Atom] -> Either AtomFunctionError Atom\""
  let attrs = [Attribute "x" IntegerAtomType]
      funcAtomExpr = FunctionAtomExpr  "idTest" [NakedAtomExpr (IntegerAtom 3)] ()
      tupleExprs = TupleExprs () [TupleExpr (M.singleton "x" funcAtomExpr)]
      expectedResult = mkRelationFromList (V.fromList attrs) [[IntegerAtom 3]]
  result <- executeRelationalExpr sess conn (MakeRelationFromExprs (Just (map NakedAttributeExpr attrs)) tupleExprs)

  assertEqual "id function equality" expectedResult result
  
#endif
