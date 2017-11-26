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
import qualified Data.Set as S

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = relationOptTests ++ databaseOptTests ++ [testTransitiveOptimization,
                                                     testImpossiblePredicates,
                                                     testJoinElimination]
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
  
testJoinElimination :: Test  
testJoinElimination = TestCase $ do
  -- (s join sp){s#,qty,p#} == sp
  let unoptExpr = Project (AttributeNames (S.fromList ["p#", "qty", "s#"])) (Join (RelationVariable "sp" ()) (RelationVariable "s" ()))
  assertEqual "remove extraneous join" (Right (RelationVariable "sp" ())) (optRelExpr unoptExpr)
  
-- (sp join s){s#,qty,p#} == sp --flip relvar names
  let unoptExpr2 = Project (AttributeNames (S.fromList ["p#", "qty", "s#"])) (Join (RelationVariable "s" ()) (RelationVariable "sp" ()))  
  assertEqual "remove extraneous join2" (Right (RelationVariable "sp" ())) (optRelExpr unoptExpr2)
  
-- (sp join s){s#,qty,sname} != sp - attribute from s included
  let expr3 = Project (AttributeNames (S.fromList ["p#", "qty", "sname"])) (Join (RelationVariable "sp" ()) (RelationVariable "s" ()))
  assertEqual "retain join" (Right expr3) (optRelExpr expr3)

  -- (sp join s){qty} != sp --projection does not include foreign key attribute
  let expr4 = Project (AttributeNames (S.singleton "qty")) (Join (RelationVariable "sp" ()) (RelationVariable "s" ()))
  assertEqual "retain join2" (Right expr4) (optRelExpr expr4)
  
  -- (sp join s){s#,qty} == sp - a subset of attributes in the projection are in the foreign key
  let expr5 = Project (AttributeNames (S.fromList ["s#", "qty"])) (Join (RelationVariable "sp" ()) (RelationVariable "s" ()))
  let expect5 = Project (AttributeNames (S.fromList ["s#", "qty"])) (RelationVariable "sp" ())
  assertEqual "remove extraneous join (attribute subset)" (Right expect5) (optRelExpr expr5)
  
  -- (s join p){s#,p#} != sp -- no foreign key
  let expr6 = Project (AttributeNames (S.fromList ["s#", "p#"])) (Join (RelationVariable "s" ()) (RelationVariable "p" ()))
  assertEqual "retain join- no foreign key" (Right expr6) (optRelExpr expr6)

  -- (s join p){s#,qty,p#,sname} != sp -- all sp attributes plus one s attributes
  let expr7 = Project (AttributeNames (S.fromList ["s#", "p#", "qty", "sname"])) (Join (RelationVariable "s" ()) (RelationVariable "sp" ()))
  assertEqual "retain join- attributes from both relvars" (Right expr7) (optRelExpr expr7)      
