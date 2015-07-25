{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import ProjectM36.Base
import System.Exit
import ProjectM36.Relation.Show.CSV
import ProjectM36.Relation.Parse.CSV
import qualified ProjectM36.Attribute as A
import ProjectM36.Relation

main :: IO ()           
main = do 
  tcounts <- runTestTT $ TestList [testCSVExport]
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

--round-trip a basic relation through CSV
testCSVExport :: Test
testCSVExport = TestCase $ do
  let attrs = A.attributesFromList [Attribute "S#" StringAtomType, 
                                    Attribute "SNAME" StringAtomType, 
                                    Attribute "STATUS" IntAtomType, 
                                    Attribute "CITY" StringAtomType]
      relOrErr = mkRelationFromList attrs [
        [StringAtom "S9", StringAtom "Perry", IntAtom 170, StringAtom "Londonderry"],
        [StringAtom "S8", StringAtom "Mike", IntAtom 150, StringAtom "Boston"]]
  case relOrErr of 
    Left err -> assertFailure $ "export relation creation failure: " ++ (show err)
    Right rel -> do
      case relationAsCSV rel of
        Left err -> assertFailure $ "export failed: " ++ (show err)
        Right csvData -> case csvAsRelation csvData attrs of -- import csv data back to relation
          Left err -> assertFailure $ "re-import failed: " ++ (show err)
          Right rel' -> assertEqual "relation CSV comparison" rel rel'

