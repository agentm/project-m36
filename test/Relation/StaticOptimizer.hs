import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.RelationalExpression
import ProjectM36.DateExamples
import ProjectM36.TupleSet
import ProjectM36.StaticOptimizer
import System.Exit
import Control.Monad.State
import Control.Monad.Trans.Reader
import Test.HUnit

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = relationOptTests ++ databaseOptTests
    optDBCTest = map (\(name, expr, unopt) -> TestCase $ assertEqual name expr (evalState (applyStaticDatabaseOptimization unopt) (freshDatabaseState dateExamples)))
    relvar nam = RelationVariable nam ()
    startState = mkRelationalExprState dateExamples
    optRelTest = map (\(name, expr, unopt) -> TestCase $ assertEqual name expr (runReader (applyStaticRelationalOptimization unopt) startState))
    relationOptTests = optRelTest [
      ("StaticProject", Right $ relvar "s", Project (AttributeNames (attributeNames suppliersRel)) (relvar "s")),
      ("StaticUnion", Right $ relvar "s", Union (relvar "s") (relvar "s")),
      ("StaticJoin", Right $ relvar "s", Join (relvar "s") (relvar "s")),
      ("StaticRestrictTrue", Right $ relvar "s", Restrict TruePredicate (relvar "s")),
      ("StaticRestrictFalse", Right $ MakeStaticRelation (attributes suppliersRel) emptyTupleSet, Restrict (NotPredicate TruePredicate) (relvar "s"))
      ]
    databaseOptTests = optDBCTest [
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
