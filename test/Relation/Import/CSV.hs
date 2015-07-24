{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import ProjectM36.Base
import ProjectM36.Relation.Parse.CSV
import ProjectM36.Relation
import qualified ProjectM36.Attribute as A
import System.Exit
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding

main :: IO ()           
main = do 
  tcounts <- runTestTT $ TestList [testCSVSuccess]
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testCSVSuccess :: Test
testCSVSuccess = TestCase $ do
  let sampleCSV = (encodeUtf8 . T.pack) "S#,CITY,STATUS,SNAME\n\"S8\",\"Boston\",150,\"Mike\"\nS9,Londonderry,170,Perry"
      expectedAttrs = A.attributesFromList [Attribute "S#" StringAtomType, 
                                            Attribute "SNAME" StringAtomType, 
                                            Attribute "STATUS" IntAtomType, 
                                            Attribute "CITY" StringAtomType]
      expectedRel = mkRelationFromList expectedAttrs [
        [StringAtom "S9", StringAtom "Perry", IntAtom 170, StringAtom "Londonderry"],
        [StringAtom "S8", StringAtom "Mike", IntAtom 150, StringAtom "Boston"]]
  case csvAsRelation sampleCSV expectedAttrs of
    Left err -> assertFailure $ show err
    Right csvRel -> assertEqual "csv->relation" expectedRel (Right csvRel)

