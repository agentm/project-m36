import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.TransactionGraph
import ProjectM36.DateExamples
import ProjectM36.TupleSet as TS
import ProjectM36.StaticOptimizer
import ProjectM36.DatabaseContext as DBC
import System.Exit
import Control.Monad.State
import Test.HUnit
import qualified Data.Set as S

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = [testTransitiveOptimization,
              testImpossiblePredicates,
              testJoinElimination,
              testRestrictionPredicateCollapse,
              testPushDownRestrictionPredicate,
              testOptimizeRelationalExpr,
              testOptimizeDatabaseContextExpr]

testOptimizeDatabaseContextExpr :: Test
testOptimizeDatabaseContextExpr = TestCase $ do
  (graph, transId) <- freshTransactionGraph DBC.empty
  let relvar nam = RelationVariable nam ()
      relvarId nam = RelationVariable nam UncommittedContextMarker
      testInfos = [
        ("StaticAssign", 
         Right $ Assign "z" (relvarId "s"),
         Assign "z" (Restrict TruePredicate (relvar "s"))
        ),
        ("StaticMultipleExpr", 
         Right $ MultipleExpr [Assign "z" (relvarId "s"), 
                               Assign "z" (relvarId "s")],
         MultipleExpr [Assign "z" (Restrict TruePredicate (relvar "s")),
                       Assign "z" (Restrict TruePredicate (relvar "s"))])
        ]
  forM_ testInfos $ \(name, expectedExpr, unoptExpr) -> do
    let optExpr = runGraphRefSOptDatabaseContextExprM transId dateExamples graph (optimizeDatabaseContextExpr unoptExpr)
    assertEqual name expectedExpr optExpr

testTransitiveOptimization :: Test
testTransitiveOptimization = TestCase $ do
  (graph, _) <- freshTransactionGraph dateExamples
  let expr atomexpr = AndPredicate (AttributeEqualityPredicate "a" (NakedAtomExpr (IntegerAtom 300))) (AttributeEqualityPredicate "b" atomexpr)
      exprIn = expr (AttributeAtomExpr "a")
      exprOut = expr (NakedAtomExpr (IntegerAtom 300))
      optExpr = runGraphRefSOptRelationalExprM Nothing graph (applyStaticPredicateOptimization exprIn)
  assertEqual "transitive property" (Right exprOut) optExpr


testOptimizeRelationalExpr :: Test
testOptimizeRelationalExpr = TestCase $ do
  (graph, _) <- freshTransactionGraph dateExamples
  let relvarId nam = RelationVariable nam UncommittedContextMarker
      relationOptTests = [
        ("StaticProject",
         Right $ relvarId "s",
         Project (AttributeNames (attributeNames suppliersRel)) (relvarId "s")),
        ("StaticUnion",
         Right $ relvarId "s",
         Union (relvarId "s") (relvarId "s")),
        ("StaticJoin",
         Right $ relvarId "s",
         Join (relvarId "s") (relvarId "s"))
        ]
  forM_ relationOptTests $ \(name, expr, unoptExpr) -> do
    let optExpr = runGraphRefSOptRelationalExprM (Just dateExamples) graph (optimizeGraphRefRelationalExpr unoptExpr)
    assertEqual name expr optExpr

testImpossiblePredicates :: Test  
testImpossiblePredicates = TestCase $ do
  (graph, _) <- freshTransactionGraph dateExamples  
  let tautTrue = Restrict (AtomExprPredicate (NakedAtomExpr (BoolAtom True))) rel
      tautTrue2 = Restrict TruePredicate rel
      tautFalse = Restrict (AtomExprPredicate (NakedAtomExpr (BoolAtom False))) rel
      tautFalse2 = Restrict (NotPredicate TruePredicate) rel
      emptyRel = MakeStaticRelation (attributes suppliersRel) TS.empty
      rel = RelationVariable "s" UncommittedContextMarker
      optRelExpr = optimizeGraphRefRelationalExpr' (Just dateExamples) graph
  assertEqual "remove tautology where true" (Right rel) (optRelExpr tautTrue)
  assertEqual "remove tautology where true2" (Right rel) (optRelExpr tautTrue2)
  assertEqual "remove tautology where false" (Right emptyRel) (optRelExpr tautFalse)
  assertEqual "remove tautology where false2" (Right emptyRel) (optRelExpr tautFalse2)

testJoinElimination :: Test  
testJoinElimination = TestCase $ do
  (graph, _) <- freshTransactionGraph dateExamples  
  let marker = UncommittedContextMarker
      optJoinExpr expr = runGraphRefSOptRelationalExprM (Just dateExamples) graph (applyStaticJoinElimination expr)
      relvar rv = RelationVariable rv marker

  -- (s join sp){s#,qty,p#} == sp
  let unoptExpr = Project (AttributeNames (S.fromList ["p#", "qty", "s#"])) (Join (relvar "sp") (relvar "s"))
  assertEqual "remove extraneous join" (Right (relvar "sp")) (optJoinExpr unoptExpr)
  
-- (sp join s){s#,qty,p#} == sp --flip relvar names
  let unoptExpr2 = Project (AttributeNames (S.fromList ["p#", "qty", "s#"])) (Join (relvar "s") (relvar "sp"))  
  assertEqual "remove extraneous join2" (Right (relvar "sp")) (optJoinExpr unoptExpr2)
  
-- (sp join s){s#,qty,sname} != sp - attribute from s included
  let expr3 = Project (AttributeNames (S.fromList ["p#", "qty", "sname"])) (Join (relvar "sp") (relvar "s"))
  assertEqual "retain join" (Right expr3) (optJoinExpr expr3)

  -- (sp join s){qty} != sp --projection does not include foreign key attribute
  let expr4 = Project (AttributeNames (S.singleton "qty")) (Join (relvar "sp") (relvar "s"))
  assertEqual "retain join2" (Right expr4) (optJoinExpr expr4)
  
  -- (sp join s){s#,qty} == sp - a subset of attributes in the projection are in the foreign key
  let expr5 = Project (AttributeNames (S.fromList ["s#", "qty"])) (Join (relvar "sp") (relvar "s"))
  let expect5 = Project (AttributeNames (S.fromList ["s#", "qty"])) (relvar "sp")
  assertEqual "remove extraneous join (attribute subset)" (Right expect5) (optJoinExpr expr5)
  
  -- (s join p){s#,p#} != sp -- no foreign key
  let expr6 = Project (AttributeNames (S.fromList ["s#", "p#"])) (Join (relvar "s") (relvar "p"))
  assertEqual "retain join- no foreign key" (Right expr6) (optJoinExpr expr6)

  -- (s join p){s#,qty,p#,sname} != sp -- all sp attributes plus one s attributes
  let expr7 = Project (AttributeNames (S.fromList ["s#", "p#", "qty", "sname"])) (Join (relvar "s") (relvar "sp"))
  assertEqual "retain join- attributes from both relvars" (Right expr7) (optJoinExpr expr7)      

testRestrictionPredicateCollapse :: Test
testRestrictionPredicateCollapse = TestCase $ do
  let expr1 = Restrict (NotPredicate TruePredicate) rv
      rv = RelationVariable "a" UncommittedContextMarker
      relvar name = RelationVariable name UncommittedContextMarker
  assertEqual "one restriction" expr1 (applyStaticRestrictionCollapse expr1)
  
  let attrEqualsPred = AttributeEqualityPredicate "x" (NakedAtomExpr (IntAtom 3))
      expr2 = Restrict TruePredicate (Restrict attrEqualsPred (relvar "a"))
  assertEqual "two restrictions" (Restrict (AndPredicate TruePredicate attrEqualsPred) (relvar "a")) (applyStaticRestrictionCollapse expr2)
  
  let expr3 = Restrict pred1 (Restrict pred2 (Restrict pred3 (relvar "a")))
      pred1 = NotPredicate TruePredicate
      pred2 = attrEqualsPred
      pred3 = AtomExprPredicate (NakedAtomExpr (BoolAtom True))
  assertEqual "three restrictions" (Restrict (AndPredicate (AndPredicate pred1 pred3) pred2) rv) (applyStaticRestrictionCollapse expr3)
  
  let expr4 = Restrict pred1 (Restrict pred2 (Project projAttrs (Restrict pred3 (Restrict pred3 rv))))
      projAttrs = AttributeNames (S.singleton "spam")
      expectedExpr = Restrict (AndPredicate pred1 pred2) (Project projAttrs (Restrict (AndPredicate pred3 pred3) rv))
  assertEqual "nested subexpr restrictions" expectedExpr (applyStaticRestrictionCollapse expr4)

testPushDownRestrictionPredicate :: Test
testPushDownRestrictionPredicate = TestCase $ do
  -- (s union relation{tuple{city "Boston", s# "S6", sname "Stevens", status 50}}) where status = 30 == (s where status=30) union (relation{tuple{city "Boston", s# "S6", sname "Stevens", status 50}} where status=30)
  let rvs = RelationVariable "s" UncommittedContextMarker
  let status30 = AttributeEqualityPredicate "status" (NakedAtomExpr (IntAtom 30))
      relationsOrError = do
            stevens <- mkRelationFromList (attributes suppliersRel) [[TextAtom "S6", TextAtom "Stevens", IntegerAtom 50, TextAtom "Boston"]]
            let expectedExpr1 = Union (Restrict status30 rvs) (Restrict status30 (ExistingRelation stevens))
                expr1 = Restrict status30 (Union rvs (ExistingRelation stevens))

            pure (expectedExpr1, applyStaticRestrictionPushdown expr1)
  either (assertFailure . (++) "stevens relation: " . show) (uncurry $ assertEqual "union restriction pushdown") relationsOrError

  -- s{sname,city} where city="London" == (s where city="London"){sname,city}
  let expr2 = Restrict whereLondon (Project projAttrs2 rvs)
      projAttrs2 = AttributeNames (S.fromList ["sname", "city"])
      whereLondon = AttributeEqualityPredicate "city" (NakedAtomExpr (TextAtom "London"))
      expectedExpr2 = Project projAttrs2 (Restrict whereLondon rvs)
  assertEqual "projection restriction pushdown" expectedExpr2 (applyStaticRestrictionPushdown expr2)
  
  -- (s where city="London"){sname} != s{sname} where city="London" (no "city" attribute at projection time)
  let expr3 = Project projAttrs3 (Restrict whereLondon rvs)
      projAttrs3 = AttributeNames (S.singleton "sname")
  assertEqual "projection restriction no-pushdown" expr3 (applyStaticRestrictionPushdown expr3)
