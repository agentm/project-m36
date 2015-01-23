import TutorialDInterpreter
import Test.HUnit
import RelationExpr
import Relation
import RelationalError
import RelationType
import qualified Data.HashSet as HS
import qualified Data.Map as M
import System.Exit

main = do 
  counts <- runTestTT (TestList tests)
  if errors counts + failures counts > 0 then exitFailure else exitSuccess
  where
    tests = map (\(tutd, expected) -> TestCase $ assertTutdEqual basicDatabaseContext tutd expected) simpleRelTests ++ map (\(tutd, expected) -> TestCase $ assertTutdEqual dateExamples tutd expected) dateExampleRelTests
    simpleRelTests = [("x:=true", Right relationTrue),
                      ("x:=false", Right relationFalse),
                      ("x:=true union false", Right relationTrue),
                      ("x:=true; x:=false", Right relationFalse),
                      ("x:=relation{a int}", mkRelation simpleAAttributes HS.empty),
                      ("x:=relation{c int} rename {c as d}", mkRelation simpleBAttributes HS.empty),
                      ("y:=relation{b int, c int}; x:=y{c}", mkRelation simpleProjectionAttributes HS.empty),
                      ("constraint failc true in false; x:=true", Left $ InclusionDependencyCheckError "failc"),
                      ("x:=true where true", Right relationTrue),
                      ("x:=true where false", Right relationFalse),
                      ("x:=true where true or false", Right relationTrue),
                      ("x:=true where false or false", Right relationFalse),
                      ("x:=true where true and false", Right relationFalse),
                      ("x:=true where true and true", Right relationTrue)
                     ]
    simpleAAttributes = M.fromList [("a", Attribute "a" IntAtomType)]
    simpleBAttributes = M.fromList [("d", Attribute "d" IntAtomType)]
    simpleProjectionAttributes = M.fromList [("c", Attribute "c" IntAtomType)]
    dateExampleRelTests = [("x:=S where true", Right s),
                           ("x:=S where CITY = \"London\"", restrict (\(RelationTuple tupMap) -> tupMap M.! "CITY" == StringAtom "London") s),
                           ("x:=S where false", Right $ Relation (attributes s) HS.empty)]
                           --("x:=S where SNO in S{SNO}", Right s)]


testSimple1 = TestCase $ assertTutdEqual basicDatabaseContext "true" (Right relationTrue)

assertTutdEqual databaseContext tutd expected = assertEqual tutd interpreted expected
  where
    interpreted = case interpret databaseContext tutd of 
      (Just err, _) -> Left err
      (Nothing, context) -> Right $ (relationVariables context) M.! "x"