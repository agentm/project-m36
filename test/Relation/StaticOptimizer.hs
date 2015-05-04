{-# LANGUAGE OverloadedStrings #-}
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.RelationalExpression
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
      ("StaticProject", Right $ RelationVariable "S", Project (attributeNames suppliersRel) (RelationVariable "S")),
      ("StaticUnion", Right $ RelationVariable "S", Union (RelationVariable "S") (RelationVariable "S")),
      ("StaticJoin", Right $ RelationVariable "S", Join (RelationVariable "S") (RelationVariable "S")),
      ("StaticRestrictTrue", Right $ RelationVariable "S", Restrict TruePredicate (RelationVariable "S")),
      ("StaticRestrictFalse", Right $ MakeStaticRelation (attributes suppliersRel) emptyTupleSet, Restrict (NotPredicate TruePredicate) (RelationVariable "S"))
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
