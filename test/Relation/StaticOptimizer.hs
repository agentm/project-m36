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
    relationOptTests = optTest applyStaticRelationalOptimization [
      ("StaticProject", Right $ RelationVariable "s", Project (AttributeNames (attributeNames suppliersRel)) (RelationVariable "s")),
      ("StaticUnion", Right $ RelationVariable "s", Union (RelationVariable "s") (RelationVariable "s")),
      ("StaticJoin", Right $ RelationVariable "s", Join (RelationVariable "s") (RelationVariable "s")),
      ("StaticRestrictTrue", Right $ RelationVariable "s", Restrict TruePredicate (RelationVariable "s")),
      ("StaticRestrictFalse", Right $ MakeStaticRelation (attributes suppliersRel) emptyTupleSet, Restrict (NotPredicate TruePredicate) (RelationVariable "s"))
      ]
    databaseOptTests = optTest applyStaticDatabaseOptimization [
      ("StaticAssign", 
       Right $ Assign "z" (RelationVariable "s"),
       Assign "z" (Restrict TruePredicate (RelationVariable "s"))
       ),
      ("StaticMultipleExpr", 
       Right $ MultipleExpr [Assign "z" (RelationVariable "s"), 
                             Assign "z" (RelationVariable "s")],
       MultipleExpr [Assign "z" (Restrict TruePredicate (RelationVariable "s")),
                     Assign "z" (Restrict TruePredicate (RelationVariable "s"))])
      ]
