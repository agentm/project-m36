{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import ProjectM36.Base
import ProjectM36.PrettyPrinter
import qualified Data.Vector as V
import System.Exit
import Data.Text
import ProjectM36.Relation
import Data.Text.Prettyprint.Doc 
testList :: Test
testList = TestList [
  testPretty "true" (ExistingRelation (Relation V.empty (RelationTupleSet [RelationTuple V.empty V.empty]))), 
  testPretty "false"  (ExistingRelation (Relation V.empty (RelationTupleSet []))),
  testPretty "true:{a:=1, b:=1}" (Extend (AttributeExtendTupleExpr "b" (NakedAtomExpr (IntAtom 1))) (Extend (AttributeExtendTupleExpr "a" (NakedAtomExpr (IntAtom 1))) (ExistingRelation relationTrue)))]

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testPretty :: String -> RelationalExpr -> Test
testPretty relExprText relExprADT = TestCase $ assertEqual relExprText (show (pretty relExprADT)) (show (pretty relExprText))
