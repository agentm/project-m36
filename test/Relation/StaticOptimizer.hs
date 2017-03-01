import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.RelationalExpression
import ProjectM36.DateExamples
import ProjectM36.TupleSet
import ProjectM36.StaticOptimizer
import ProjectM36.Key
import ProjectM36.InclusionDependency
import System.Exit
import Control.Monad.State
import Test.HUnit
import qualified Data.Set as S

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = relationOptTests ++ databaseOptTests ++ [testIncDepOptimizer]
    optTest optfunc estate testparams = map (\(name, expr, unopt) -> TestCase $ assertEqual name expr (evalState (optfunc unopt) estate)) $ testparams
    relvar nam = RelationVariable nam ()
    relationOptTests = optTest applyStaticRelationalOptimization  (mkRelationalExprState dateExamples) [
      ("StaticProject", Right $ relvar "s", Project (AttributeNames (attributeNames suppliersRel)) (relvar "s")),
      ("StaticUnion", Right $ relvar "s", Union (relvar "s") (relvar "s")),
      ("StaticJoin", Right $ relvar "s", Join (relvar "s") (relvar "s")),
      ("StaticRestrictTrue", Right $ relvar "s", Restrict TruePredicate (relvar "s")),
      ("StaticRestrictFalse", Right $ MakeStaticRelation (attributes suppliersRel) emptyTupleSet, Restrict (NotPredicate TruePredicate) (relvar "s"))
      ]
    databaseOptTests = optTest applyStaticDatabaseOptimization dateExamples [
      ("StaticAssign", 
       Right $ Assign "z" (relvar "s"),
       Assign "z" (Restrict TruePredicate (relvar "s"))
       ),
      ("StaticMultipleExpr", 
       Right $ MultipleExpr [Assign "z" (relvar "s"), 
                             Assign "z" (relvar "s")],
       MultipleExpr [Assign "z" (Restrict TruePredicate (relvar "s")),
                     Assign "z" (Restrict TruePredicate (relvar "s"))])
      ]

-- test that the static optimizer doesn't eliminate inclusion dependencies improperly
testIncDepOptimizer :: Test
testIncDepOptimizer = TestCase $ do
  --simple case: an insert not mentioning the incdep need not be validated
  let incDep1 = inclusionDependencyForKey (AttributeNames (S.singleton "attrX")) (RelationVariable "rv" ())
      (InclusionDependency i1 _) = incDep1
  putStrLn (show i1)
  putStrLn (show (relationVariableNames i1))
  assertEqual "relevant relvar names" (S.singleton "rv") (relvarReferences incDep1)
  assertEqual "irrelevant insert" NoValidationNeeded (inclusionDependencyValidation (Insert "rv2" (RelationVariable "x" ())) incDep1)
  assertEqual "relevant insert" ValidationNeeded (inclusionDependencyValidation (Insert "rv" (RelationVariable "x" ())) incDep1)
  