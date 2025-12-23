{-# LANGUAGE CPP #-}
import System.Exit
import Test.HUnit
#ifdef PM36_HASKELL_SCRIPTING
import TutorialD.Interpreter.TestBase
import ProjectM36.Client
import ProjectM36.Relation
import qualified Data.Text as T
#endif

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [
#ifdef PM36_HASKELL_SCRIPTING
    testBasicDBCFunction,
    testErrorDBCFunction,
    testExceptionDBCFunction,
    testDBCFunctionWithAtomArguments
#endif                                  
                                  ])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

#ifdef PM36_HASKELL_SCRIPTING
testBasicDBCFunction :: Test
testBasicDBCFunction = TestCase $ do  
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let addfunc = "adddatabasecontextfunction \"addTrue2\" DatabaseContext -> Either RelationalError DatabaseContext  \"\"\"(\\util [] ctx -> (executeDatabaseContextExpr util) ctx (Assign \"true2\" (ExistingRelation relationTrue))) :: DatabaseContextFunctionBodyType\"\"\""
  executeTutorialD sess conn addfunc
  executeTutorialD sess conn "execute addTrue2()"
  {-
  let true2Expr = RelationVariable "true2" ()
  result <- executeRelationalExpr sess conn true2Expr
  assertEqual "simple atom function equality" (Right relationTrue) result-}

testErrorDBCFunction :: Test
testErrorDBCFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback  
  let addfunc = "adddatabasecontextfunction \"retErr\" DatabaseContext -> Either RelationalError DatabaseContext \"\"\"(\\util [] ctx -> Left (DatabaseContextFunctionUserError \"err\")) :: DatabaseContextFunctionBodyType\"\"\""
  executeTutorialD sess conn addfunc
  expectTutorialDErr sess conn (T.isPrefixOf "DatabaseContextFunctionUserError \"err\"") "execute retErr()"
  
testExceptionDBCFunction :: Test
testExceptionDBCFunction = TestCase $ do
  -- throw an error, make sure it is caught- it might not be caught, for example, if the function is not forced
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let addfunc = "adddatabasecontextfunction \"bomb\" DatabaseContext -> Either RelationalError DatabaseContext \"\"\"(\\util [] _ -> error \"boom\") :: DatabaseContextFunctionBodyType\"\"\""
  executeTutorialD sess conn addfunc
  expectTutorialDErr sess conn ("UnhandledExceptionError" `T.isPrefixOf`) "execute bomb()"
  

testDBCFunctionWithAtomArguments :: Test
testDBCFunctionWithAtomArguments = TestCase $ do
  --test function with creation of a relvar with some arguments
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let addfunc = "adddatabasecontextfunction \"multiArgFunc\" Integer -> Text -> DatabaseContext -> Either RelationalError DatabaseContext \"\"\"(\\util (age:name:_) ctx -> (executeDatabaseContextExpr util) ctx (Assign \"person\" (MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (fromList [(\"name\", NakedAtomExpr name), (\"age\", NakedAtomExpr age)])])))) :: DatabaseContextFunctionBodyType\"\"\""
  executeTutorialD sess conn addfunc
  executeTutorialD sess conn "execute multiArgFunc(30,\"Steve\")"
  result <- executeRelationalExpr sess conn (RelationVariable "person" ())
  let expectedPerson = mkRelationFromList (attributesFromList [Attribute "name" TextAtomType,
                                                               Attribute "age" IntegerAtomType]) [
        [TextAtom "Steve", IntegerAtom 30]]
  assertEqual "person relation" expectedPerson result
  expectTutorialDErr sess conn (T.isPrefixOf "AtomTypeMismatchError") "execute multiArgFunc(\"fail\", \"fail\")"

#endif
