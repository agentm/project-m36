import Test.HUnit
import ProjectM36.Base
import ProjectM36.IsomorphicSchema
import ProjectM36.Relation
import ProjectM36.RelationalExpression
import qualified ProjectM36.DatabaseContext as DBC
import qualified ProjectM36.Attribute as A
import System.Exit
import qualified Data.Map as M
import Control.Monad.State

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
      isomorphsAtoB = [IsoRelVarName "emp" "employee", 
                       IsoRelVarName "department" "department"]
      unionExpr = Union (RelationVariable "emp" ()) (RelationVariable "department" ())      
      relResult = evalState (evalRelationalExpr (applyRelationalExprSchemaIsomorphs isomorphsAtoB unionExpr)) schemaA
  assertEqual "employee relation morph" (Right relationTrue) relResult
  
testIsoRestrict :: Test
testIsoRestrict = TestCase $ do
  -- create a emp relation which is restricted into two boss, nonboss rel vars
  emprel <- assertEither $ mkRelationFromList (A.attributesFromList [Attribute "name" TextAtomType,
                                                                    Attribute "boss" TextAtomType])
            [[TextAtom "Steve", TextAtom ""],
             [TextAtom "Cindy", TextAtom "Steve"],
             [TextAtom "Sam", TextAtom "Steve"]]
  let schemaA = DBC.empty {
        relationVariables = M.singleton "employee" emprel 
        }
      isomorphsAtoB = [IsoRestrict "employee" (AttributeEqualityPredicate "boss" (NakedAtomExpr (TextAtom ""))) ("nonboss", "boss")]
      bossq = RelationVariable "boss" ()
      nonbossq = RelationVariable "nonboss" ()
      unionq = Union (RelationVariable "boss" ()) (RelationVariable "nonboss" ())
      relResult = evalState (evalRelationalExpr (applyRelationalExprSchemaIsomorphs isomorphsAtoB unionq)) schemaA
  assertEqual "boss/nonboss union" (Right emprel) relResult