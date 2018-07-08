import TutorialD.Interpreter.TestBase
import System.Exit
import Test.HUnit
import ProjectM36.Client
import ProjectM36.Relation
import qualified Data.Text as T

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testBasicDBCFunction
                                  --testErrorDBCFunction,
                                  --testExceptionDBCFunction,
                                 --testDBCFunctionWithAtomArguments
                                  ])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testBasicDBCFunction :: Test
testBasicDBCFunction = TestCase $ do  
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let addfunc = "adddatabasecontextfunction \"addTrue2\" DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext  \"\"\"(\\[] ctx -> executeDatabaseContextExpr (Assign \"true2\" (ExistingRelation relationTrue)) ctx) :: [Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext\"\"\""
  executeTutorialD sess conn addfunc
  putStrLn "spam1"
  executeTutorialD sess conn "execute addTrue2()"
  putStrLn "spam2"
  {-
  let true2Expr = RelationVariable "true2" ()
  result <- executeRelationalExpr sess conn true2Expr
  assertEqual "simple atom function equality" (Right relationTrue) result-}

testErrorDBCFunction :: Test
testErrorDBCFunction = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback  
  let addfunc = "adddatabasecontextfunction \"retErr\" DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext \"\"\"(\\[] ctx -> Left (DatabaseContextFunctionUserError \"err\")) :: [Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext\"\"\""
  executeTutorialD sess conn addfunc
  expectTutorialDErr sess conn (T.isPrefixOf "DatabaseContextFunctionUserError (DatabaseContextFunctionUserError \"err\")") "execute retErr()"
  
testExceptionDBCFunction :: Test
testExceptionDBCFunction = TestCase $ do
  -- throw an error, make sure it is caught- it might not be caught, for example, if the function is not forced
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let addfunc = "adddatabasecontextfunction \"bomb\" DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext \"\"\"(\\[] _ -> error \"boom\") :: [Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext\"\"\""
  executeTutorialD sess conn addfunc
  expectTutorialDErr sess conn (\err -> "UnhandledExceptionError" `T.isPrefixOf` err) "execute bomb()"
  

testDBCFunctionWithAtomArguments :: Test
testDBCFunctionWithAtomArguments = TestCase $ do
  --test function with creation of a relvar with some arguments
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let addfunc = "adddatabasecontextfunction \"multiArgFunc\" Integer -> Text -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext \"\"\"(\\(age:name:_) ctx -> executeDatabaseContextExpr (Assign \"person\" (MakeRelationFromExprs Nothing [TupleExpr (fromList [(\"name\", NakedAtomExpr name), (\"age\", NakedAtomExpr age)])])) ctx) :: [Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext\"\"\""
  executeTutorialD sess conn addfunc
  executeTutorialD sess conn "execute multiArgFunc(30,\"Steve\")"
  result <- executeRelationalExpr sess conn (RelationVariable "person" ())
  let expectedPerson = mkRelationFromList (attributesFromList [Attribute "name" TextAtomType,
                                                               Attribute "age" IntegerAtomType]) [
        [TextAtom "Steve", IntegerAtom 30]]
  assertEqual "person relation" expectedPerson result
  expectTutorialDErr sess conn (T.isPrefixOf "AtomTypeMismatchError") "execute multiArgFunc(\"fail\", \"fail\")"


