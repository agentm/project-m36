import TutorialDInterpreter
import Test.HUnit
import RelationExpr
import Relation
import RelationType
import qualified Data.HashSet as HS
import qualified Data.Map as M

main = runTestTT (TestList tests)
  where
    tests = map (\(tutd, expected)->TestCase $ assertTutdEqual tutd expected) simpleRelTests
    simpleRelTests = [("true", Right relationTrue),
                      ("false", Right relationFalse),
                      ("true union false", Right relationTrue),
                      ("true; false", Right relationFalse),
                      ("a:=relation{a int}", mkRelation simpleAAttributes HS.empty),
                      ("b:=relation{c int} rename {c as d}", mkRelation simpleBAttributes HS.empty)
                      ]
    simpleAAttributes = M.fromList [("a", Attribute "a" IntAtomType)]
    simpleBAttributes = M.fromList [("d", Attribute "d" IntAtomType)]


testSimple1 = TestCase $ assertTutdEqual "true" (Right relationTrue)

assertTutdEqual tutd expected = assertEqual tutd interpreted expected
  where
    interpreted = fst (interpret basicRelVarContext tutd)