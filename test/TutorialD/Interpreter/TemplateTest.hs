{-# LANGUAGE QuasiQuotes #-}
import System.Exit
import Test.HUnit
import TutorialD.Interpreter.Template
import ProjectM36.Base
import ProjectM36.TransactionGraph

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testRelationalExpr,
                                 testDatabaseContextExpr,
                                 testTransGraphRelationalExpr,
                                 testRelationalExprReplacement])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testRelationalExpr :: Test
testRelationalExpr = TestCase $ do
  let actual = [relationalExpr|x union x|]
      expected = RelationVariable "x" () `Union` RelationVariable "x" ()
  assertEqual "relational expr" expected actual

testDatabaseContextExpr :: Test
testDatabaseContextExpr = TestCase $ do
  let actual :: DatabaseContextExpr -- resolved ACL type variable
      actual = [databaseContextExpr|x:=true|]
      expected = Assign "x" (RelationVariable "true" ())
  assertEqual "database context expr" expected actual

testTransGraphRelationalExpr :: Test
testTransGraphRelationalExpr = TestCase $ do
  let actual = [transGraphRelationalExpr|t@master^|]
      expected = RelationVariable "t" (TransactionIdHeadNameLookup "master" [TransactionIdHeadBranchBacktrack 1])
  assertEqual "trans graph relational expr" expected actual

testRelationalExprReplacement :: Test
testRelationalExprReplacement = TestCase $ do
  let actual = [relationalExpr|s where city="$1"|]
      expected = Restrict (AttributeEqualityPredicate "city" (NakedAtomExpr (TextAtom "London"))) (RelationVariable "s" ())
      actual' = replaceTextAtom "$1" "London" actual
  assertEqual "replace relational expr" expected actual'
