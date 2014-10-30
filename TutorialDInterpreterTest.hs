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
    tests = map (\(tutd, expected) -> TestCase $ assertTutdEqual basicRelVarContext tutd expected) simpleRelTests
    simpleRelTests = [("true", Right relationTrue),
                      ("false", Right relationFalse),
                      ("true union false", Right relationTrue),
                      ("true; false", Right relationFalse),
                      ("a:=relation{a int}", mkRelation simpleAAttributes HS.empty),
                      ("b:=relation{c int} rename {c as d}", mkRelation simpleBAttributes HS.empty),
                      ("a:=relation{b int, c int}; a{c}", mkRelation simpleProjectionAttributes HS.empty)
                      ]
    simpleAAttributes = M.fromList [("a", Attribute "a" IntAtomType)]
    simpleBAttributes = M.fromList [("d", Attribute "d" IntAtomType)]
    simpleProjectionAttributes = M.fromList [("c", Attribute "c" IntAtomType)]


testSimple1 = TestCase $ assertTutdEqual basicRelVarContext "true" (Right relationTrue)

assertTutdEqual relVarContext tutd expected = assertEqual tutd interpreted expected
  where
    interpreted = fst (interpret relVarContext tutd)