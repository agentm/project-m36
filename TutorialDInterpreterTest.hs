import TutorialDInterpreter
import Test.HUnit
import RelationExpr
import Relation
import RelationType
import qualified Data.HashSet as HS
import qualified Data.Map as M
import System.Exit

main = do 
  counts <- runTestTT (TestList tests)
  if errors counts + failures counts > 0 then exitFailure else exitSuccess
  where
    tests = map (\(tutd, expected) -> TestCase $ assertTutdEqual basicDatabaseContext tutd expected) simpleRelTests
    simpleRelTests = [("x:=true", Right relationTrue),
                      ("x:=false", Right relationFalse),
                      ("x:=true union false", Right relationTrue),
                      ("x:=true; x:=false", Right relationFalse),
                      ("x:=relation{a int}", mkRelation simpleAAttributes HS.empty),
                      ("x:=relation{c int} rename {c as d}", mkRelation simpleBAttributes HS.empty),
                      ("y:=relation{b int, c int}; x:=y{c}", mkRelation simpleProjectionAttributes HS.empty)
                      ]
    simpleAAttributes = M.fromList [("a", Attribute "a" IntAtomType)]
    simpleBAttributes = M.fromList [("d", Attribute "d" IntAtomType)]
    simpleProjectionAttributes = M.fromList [("c", Attribute "c" IntAtomType)]


testSimple1 = TestCase $ assertTutdEqual basicDatabaseContext "true" (Right relationTrue)

assertTutdEqual databaseContext tutd expected = assertEqual tutd interpreted expected
  where
    interpreted = case interpret databaseContext tutd of 
      (Just err, _) -> Left err
      (Nothing, context) -> Right $ (relationVariables context) M.! "x"