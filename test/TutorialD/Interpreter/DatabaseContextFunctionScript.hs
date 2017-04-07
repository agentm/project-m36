import TutorialD.Interpreter.TestBase
import System.Exit
import Test.HUnit
import ProjectM36.Client
import ProjectM36.Relation
import qualified Data.Text as T

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testBasicDBCFunction,
                                  testErrorDBCFunction,
                                  testDBCFunctionWithAtomArguments
                                  ])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

{-
adddatabasecontextfunction "addtruerv" DatabaseContext -> DatabaseContext """(\[] ctx -> execState (evalContextExpr (Assign "true2" (ExistingRelation relationTrue))) ctx) :: [Atom] -> DatabaseContext -> DatabaseContext"""
-}
testBasicDBCFunction :: Test
testBasicDBCFunction = TestCase $ do  
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let addfunc = "adddatabasecontextfunction \"addTrue2\" DatabaseContext -> DatabaseContext  \"\"\"(\\[] ctx -> execState (evalContextExpr (Assign \"true2\" (ExistingRelation relationTrue))) ctx) :: [Atom] -> DatabaseContext -> DatabaseContext\"\"\""
  executeTutorialD sess conn addfunc
  executeTutorialD sess conn "execute addTrue2()"
  let true2Expr = RelationVariable "true2" ()
  result <- executeRelationalExpr sess conn true2Expr
  assertEqual "simple atom function equality" (Right relationTrue) result

testErrorDBCFunction :: Test
testErrorDBCFunction = TestCase $ do
  -- throw an error, make sure it is caught- it might not be caught, for example, if the function is not forced
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let addfunc = "adddatabasecontextfunction \"bomb\" DatabaseContext -> DatabaseContext \"\"\"(\\[] _ -> error \"boom\") :: [Atom] -> DatabaseContext -> DatabaseContext\"\"\""
  executeTutorialD sess conn addfunc
  expectTutorialDErr sess conn (\err -> "UnhandledExceptionError" `T.isPrefixOf` err) "execute bomb()"

{-
adddatabasecontextfunction "multiArgFunc" Int -> Text -> DatabaseContext -> DatabaseContext """(\(age:name:_) ctx -> execState (evalContextExpr (Assign "person" (MakeRelationFromExprs Nothing [TupleExpr (fromList [("name", NakedAtomExpr name), ("age", NakedAtomExpr age)])]))) ctx)"""
-}
testDBCFunctionWithAtomArguments :: Test
testDBCFunctionWithAtomArguments = TestCase $ do
  --test function with creation of a relvar with some arguments
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let addfunc = "adddatabasecontextfunction \"multiArgFunc\" Int -> Text -> DatabaseContext -> DatabaseContext \"\"\"(\\(age:name:_) ctx -> execState (evalContextExpr (Assign \"person\" (MakeRelationFromExprs Nothing [TupleExpr (fromList [(\"name\", NakedAtomExpr name), (\"age\", NakedAtomExpr age)])]))) ctx) :: [Atom] -> DatabaseContext -> DatabaseContext\"\"\""
  executeTutorialD sess conn addfunc
  executeTutorialD sess conn "execute multiArgFunc(30,\"Steve\")"
  result <- executeRelationalExpr sess conn (RelationVariable "person" ())
  let expectedPerson = mkRelationFromList (attributesFromList [Attribute "name" TextAtomType,
                                                               Attribute "age" IntAtomType]) [
        [TextAtom "Steve", IntAtom 30]]
  assertEqual "person relation" expectedPerson result
  expectTutorialDErr sess conn (T.isPrefixOf "AtomTypeMismatchError") "execute multiArgFunc(\"fail\", \"fail\")"

{-
adddatabasecontextfunction "addperson" Int -> Text -> DatabaseContext -> DatabaseContext """(\(age:name:_) ctx -> let newrel = MakeRelationFromExprs Nothing [TupleExpr (fromList [("name", name),("age",age))]] in if isRight (runState (executeRelationalExpr (RelationVariable "person" ()) ctx)) then execState (executeContextExpr (Insert "person" newrel)) ctx else execState (executeContextExpr (Assign "person" newrel)) ctx) :: [Atom] -> DatabaseContext -> DatabaseContext"""
-}

f :: [Atom] -> DatabaseContext -> DatabaseContext
f = \(age:name:_) ctx -> let newrel = MakeRelationFromExprs Nothing [TupleExpr (fromList [("name", name),("age",age))]] in if isRight (runState (executeRelationalExpr (RelationVariable "person" ()))ctx) then execState (executeContextExpr (Insert "person" newrel)) ctx else execState (executeContextExpr (Assign "person" newrel)) ctx