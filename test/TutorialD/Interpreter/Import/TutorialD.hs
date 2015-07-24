{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import ProjectM36.Base
import qualified ProjectM36.Attribute as A
import TutorialD.Interpreter.Import.TutorialD
import qualified Data.HashSet as HS
import System.Exit
import System.IO.Temp
import qualified ProjectM36.DatabaseContext as DC
import qualified Data.Vector as V
import System.IO


main :: IO ()
main = do 
  tcounts <- runTestTT $ TestList [testTutdImport]
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testTutdImport :: Test
testTutdImport = TestCase $ do
  withSystemTempFile "m.tutd" $ \path handle -> do
    hPutStrLn handle "x:=relation{tuple{a 5,b \"spam\"}}"
    hClose handle
    let expectedExpr = MultipleExpr [Assign "x" (ExistingRelation (Relation expectedAttrs $ HS.singleton (RelationTuple expectedAttrs $ V.fromList [IntAtom 5, StringAtom "spam"])))]
        expectedAttrs = A.attributesFromList [Attribute "a" IntAtomType, Attribute "b" StringAtomType]
    imported <- importTutorialD path DC.empty  
    assertEqual "import tutd" (Right expectedExpr) imported