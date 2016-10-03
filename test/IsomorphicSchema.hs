import Test.HUnit
import ProjectM36.Base
import ProjectM36.IsomorphicSchema
import ProjectM36.Relation
import ProjectM36.Relation.Show.Term
import ProjectM36.RelationalExpression
import qualified ProjectM36.DatabaseContext as DBC
import qualified ProjectM36.Attribute as A
import System.Exit
import qualified Data.Map as M
import Control.Monad.State

import qualified Data.Text as T

testList :: Test
testList = TestList [testIsoRelVarName, testIsoRestrict]

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

assertEither :: (Show a) => Either a b -> IO b
assertEither x = case x of
  Left err -> assertFailure (show err) >> undefined
  Right val -> pure val
  
testIsoRelVarName :: Test
testIsoRelVarName = TestCase $ do
  -- create a schema with two relvars and rename one while the other remains the same in the isomorphic schema
  let schemaA = DBC.empty { 
        relationVariables = M.fromList [("employee", relationTrue), 
                                        ("department", relationFalse)]
        }
      isomorphsAtoB = [isomorphRename "emp" "employee", 
                       isomorphRename "department" "department"]
      unionExpr = Union (RelationVariable "emp" ()) (RelationVariable "department" ())      
  relExpr <- assertEither (applyRelationalExprSchemaIsomorphs isomorphsAtoB unionExpr)
  let relResult = evalState (evalRelationalExpr relExpr) schemaA                             
  assertEqual "employee relation morph" (Right relationTrue) relResult
  
testIsoRestrict :: Test
testIsoRestrict = TestCase $ do
  -- create a emp relation which is restricted into two boss, nonboss rel vars
  -- the virtual schema has an employee
  emprel <- assertEither $ mkRelationFromList (A.attributesFromList [Attribute "name" TextAtomType,
                                                                    Attribute "boss" TextAtomType])
            [[TextAtom "Steve", TextAtom ""],
             [TextAtom "Cindy", TextAtom "Steve"],
             [TextAtom "Sam", TextAtom "Steve"]]
  let predicate = AttributeEqualityPredicate "boss" (NakedAtomExpr (TextAtom ""))
  bossRel <- assertEither $ evalState (evalRelationalExpr (Restrict predicate (ExistingRelation emprel))) DBC.empty
  nonBossRel <- assertEither $ evalState (evalRelationalExpr (Restrict (NotPredicate predicate) (ExistingRelation emprel))) DBC.empty
            
  let schemaA = DBC.empty {
        relationVariables = M.fromList [("nonboss", nonBossRel),
                                        ("boss", bossRel)]
        }

      isomorphsAtoB = [IsoRestrict "employee" predicate (Just "boss", Just "nonboss")]
      bossq = RelationVariable "boss" ()
      nonbossq = RelationVariable "nonboss" ()
      employeeq = RelationVariable "employee" ()
      unionq = Union bossq nonbossq
  empExpr <- assertEither (applyRelationalExprSchemaIsomorphs isomorphsAtoB employeeq)
  let empResult = evalState (evalRelationalExpr empExpr) schemaA
      unionResult = evalState (evalRelationalExpr unionq) schemaA
  case empResult of
    Right rel -> putStrLn $ T.unpack (showRelation rel)
  putStrLn $ T.unpack (showRelation nonBossRel)    
  assertEqual "boss/nonboss isorestrict" unionResult empResult