import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.DateExamples
import ProjectM36.TupleSet
import ProjectM36.StaticOptimizer
import System.Exit
import Control.Monad.State
import Test.HUnit

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = relationOptTests ++ databaseOptTests
    optTest optfunc testparams = map (\(name, expr, unopt) -> TestCase $ assertEqual name expr (evalState (optfunc unopt) dateExamples)) $ testparams
    relvar nam = RelationVariable nam ()
    relationOptTests = optTest applyStaticRelationalOptimization [
      ("StaticProject", Right $ relvar "s", Project (AttributeNames (attributeNames suppliersRel)) (relvar "s")),
      ("StaticUnion", Right $ relvar "s", Union (relvar "s") (relvar "s")),
      ("StaticJoin", Right $ relvar "s", Join (relvar "s") (relvar "s")),
      ("StaticRestrictTrue", Right $ relvar "s", Restrict TruePredicate (relvar "s")),
      ("StaticRestrictFalse", Right $ MakeStaticRelation (attributes suppliersRel) emptyTupleSet, Restrict (NotPredicate TruePredicate) (relvar "s"))
      ]
    databaseOptTests = optTest applyStaticDatabaseOptimization [
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
