import Test.HUnit
import ProjectM36.Base
import System.Exit
import ProjectM36.Relation.Show.CSV
import ProjectM36.Relation.Parse.CSV
import qualified ProjectM36.Attribute as A
import ProjectM36.Relation
import ProjectM36.Atom
import ProjectM36.DataTypes.Primitive

main :: IO ()           
main = do 
  tcounts <- runTestTT $ TestList [testCSVExport]
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

--round-trip a basic relation through CSV
testCSVExport :: Test
testCSVExport = TestCase $ do
  let attrs = A.attributesFromList [Attribute "S#" textAtomType, 
                                    Attribute "SNAME" textAtomType, 
                                    Attribute "STATUS" intAtomType, 
                                    Attribute "CITY" textAtomType]
      relOrErr = mkRelationFromList attrs [
        [textAtom "S9", textAtom "Perry", intAtom 170, textAtom "Londonderry"],
        [textAtom "S8", textAtom "Mike", intAtom 150, textAtom "Boston"]]
  case relOrErr of 
    Left err -> assertFailure $ "export relation creation failure: " ++ (show err)
    Right rel -> do
      case relationAsCSV rel of
        Left err -> assertFailure $ "export failed: " ++ (show err)
        Right csvData -> case csvAsRelation csvData attrs of -- import csv data back to relation
          Left err -> assertFailure $ "re-import failed: " ++ (show err)
          Right rel' -> assertEqual "relation CSV comparison" rel rel'

