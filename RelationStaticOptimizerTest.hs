import RelationType
import Relation
import RelationExpr
import RelationTupleSet
import RelationStaticOptimizer
import System.Exit
import Control.Monad.State
import Test.HUnit
import qualified Data.Set as S

main = do
  counts <- runTestTT (TestList tests)
  if errors counts + failures counts > 0 then exitFailure else exitSuccess
  where
    tests = relationOptTests ++ databaseOptTests
    optTest optfunc testparams = map (\(name, exp, unopt) -> TestCase $ assertEqual name exp (evalState (optfunc unopt) dateExamples)) $ testparams
    relationOptTests = optTest applyStaticRelationalOptimization [
      ("StaticProject", Right $ RelationVariable "S", Project (attributeNames s) (RelationVariable "S")),
      ("StaticUnion", Right $ RelationVariable "S", Union (RelationVariable "S") (RelationVariable "S")),
      ("StaticJoin", Right $ RelationVariable "S", Join (RelationVariable "S") (RelationVariable "S")),
      ("StaticRestrictTrue", Right $ RelationVariable "S", Restrict TruePredicate (RelationVariable "S")),
      ("StaticRestrictFalse", Right $ MakeStaticRelation (attributes s) emptyTupleSet, Restrict (NotPredicate TruePredicate) (RelationVariable "S"))
      ]
    databaseOptTests = optTest applyStaticDatabaseOptimization [
      ("StaticAssign", 
       Right $ Assign "z" (RelationVariable "S"),
       Assign "z" (Restrict TruePredicate (RelationVariable "S"))
       ),
      ("StaticMultipleExpr", 
       Right $ MultipleExpr [Assign "z" (RelationVariable "S"), 
                             Assign "z" (RelationVariable "S")],
       MultipleExpr [Assign "z" (Restrict TruePredicate (RelationVariable "S")),
                     Assign "z" (Restrict TruePredicate (RelationVariable "S"))])
      ]
    staticRelOptEqual name expected unoptimized = assertEqual name expected (evalState (applyStaticRelationalOptimization unoptimized) dateExamples)
