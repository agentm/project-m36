import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.RelationalExpression
import ProjectM36.DateExamples
import ProjectM36.Error
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
    tests = relationOptTests ++ databaseOptTests ++ [testTransitiveOptimization,
                                                     testImpossiblePredicates]
    optDBCTest = map (\(name, expr, unopt) -> TestCase $ assertEqual name expr (evalState (applyStaticDatabaseOptimization unopt) (freshDatabaseState dateExamples)))
    relvar nam = RelationVariable nam ()
    startState = mkRelationalExprState dateExamples
    optRelTest = map (\(name, expr, unopt) -> TestCase $ assertEqual name expr (runReader (applyStaticRelationalOptimization unopt) startState))
    relationOptTests = optRelTest [
      ("StaticProject", Right $ relvar "s", Project (AttributeNames (attributeNames suppliersRel)) (relvar "s")),
      ("StaticUnion", Right $ relvar "s", Union (relvar "s") (relvar "s")),
      ("StaticJoin", Right $ relvar "s", Join (relvar "s") (relvar "s"))
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

testTransitiveOptimization :: Test
testTransitiveOptimization = TestCase $ do
  let expr atomexpr = AndPredicate (AttributeEqualityPredicate "a" (NakedAtomExpr (IntegerAtom 300))) (AttributeEqualityPredicate "b" atomexpr)
      exprIn = expr (AttributeAtomExpr "a")
      exprOut = expr (NakedAtomExpr (IntegerAtom 300))
  assertEqual "transitive property" (Right exprOut) (runReader (applyStaticPredicateOptimization exprIn) (RelationalExprStateElems dateExamples))
  
optRelExpr :: RelationalExpr -> Either RelationalError RelationalExpr
optRelExpr expr = runReader (applyStaticRelationalOptimization expr) (RelationalExprStateElems dateExamples)

testImpossiblePredicates :: Test  
testImpossiblePredicates = TestCase $ do
  let tautTrue = Restrict (AtomExprPredicate (NakedAtomExpr (BoolAtom True))) rel
      tautTrue2 = Restrict TruePredicate rel
      tautFalse = Restrict (AtomExprPredicate (NakedAtomExpr (BoolAtom False))) rel
      tautFalse2 = Restrict (NotPredicate TruePredicate) rel
      emptyRel = MakeStaticRelation (attributes suppliersRel) emptyTupleSet
      rel = RelationVariable "s" ()
  assertEqual "remove tautology where true" (Right rel) (optRelExpr tautTrue)
  assertEqual "remove tautology where true2" (Right rel) (optRelExpr tautTrue2)
  assertEqual "remove tautology where false" (Right emptyRel) (optRelExpr tautFalse)
  assertEqual "remove tautology where false2" (Right emptyRel) (optRelExpr tautFalse2)