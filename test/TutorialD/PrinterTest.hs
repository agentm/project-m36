{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import ProjectM36.Base
import System.Exit
import Prettyprinter
import Data.Map (fromList)
import TutorialD.Printer ()
import TutorialD.Interpreter.RelationalExpr
import Text.Megaparsec
import Data.Text (pack)

testList :: Test
testList = TestList [
  testPretty "true" (RelationVariable "true" ()),
  testPretty "relation{tuple{a 3, b \"x\"}, tuple{a 4, b \"y\"}}" (MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (fromList [("a",NakedAtomExpr (IntegerAtom 3)),("b",NakedAtomExpr (TextAtom "x"))]),TupleExpr (fromList [("a",NakedAtomExpr (IntegerAtom 4)),("b",NakedAtomExpr (TextAtom "y"))])])),
  testPretty "true:{a:=1, b:=1}" (Extend (AttributeExtendTupleExpr "b" (NakedAtomExpr (IntegerAtom 1))) (Extend (AttributeExtendTupleExpr "a" (NakedAtomExpr (IntegerAtom 1))) (RelationVariable "true" ()))),
  testPretty "relation{tuple{a fromGregorian(2014, 2, 4)}}" (MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (fromList [("a",FunctionAtomExpr "fromGregorian" [NakedAtomExpr (IntegerAtom 2014),NakedAtomExpr (IntegerAtom 2),NakedAtomExpr (IntegerAtom 4)] ())])])),
  testPretty "relation{tuple{a bytestring(\"dGVzdGRhdGE=\")}}" (MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (fromList [("a",FunctionAtomExpr "bytestring" [NakedAtomExpr (TextAtom "dGVzdGRhdGE=")] ())])])),
  testPretty "relation{tuple{a True}}" (MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (fromList [("a",ConstructedAtomExpr "True" [] ())])])),
  testPretty "relation{tuple{a Cons 4 (Cons 5 Empty)}}" (MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (fromList [("a",ConstructedAtomExpr "Cons" [NakedAtomExpr (IntegerAtom 4),ConstructedAtomExpr "Cons" [NakedAtomExpr (IntegerAtom 5),ConstructedAtomExpr "Empty" [] ()] ()] ())])])),
  testPretty "relation{a Int, b Text, c Bool}{}" (MakeRelationFromExprs (Just [AttributeAndTypeNameExpr "a" (ADTypeConstructor "Int" []) (),AttributeAndTypeNameExpr "b" (ADTypeConstructor "Text" []) (),AttributeAndTypeNameExpr "c" (ADTypeConstructor "Bool" []) ()]) (TupleExprs () [])),
  testPretty "relation{a relation{b Int}}{}" (MakeRelationFromExprs (Just [AttributeAndTypeNameExpr "a" (RelationAtomTypeConstructor [AttributeAndTypeNameExpr "b" (ADTypeConstructor "Int" []) ()]) ()]) (TupleExprs () []))
  ]

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testPretty :: String -> RelationalExpr -> Test
testPretty tutdStr relExprADT =
  TestCase $ do
    let relExprStr = show (pretty relExprADT)
    print relExprStr
    roundTrip <- parseRelExpr relExprStr
    {-tutdIn <- parseRelExpr tutdStr
     print ("tutd parsed", tutdIn)-}
    assertEqual ("pretty ADT " <> tutdStr) tutdStr relExprStr
    assertEqual ("round-trip " <> tutdStr) relExprADT roundTrip

--round trip tutoriald
parseRelExpr :: String -> IO RelationalExpr
parseRelExpr tutdStr =
  case parse relExprP "test" (pack tutdStr) of
    Left err ->
      assertFailure (show err)
    Right parsed -> pure parsed
