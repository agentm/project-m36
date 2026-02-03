-- | Test import via ProjectM36.Module which allows a user to import any number of atom functions or database context functions.
{-# LANGUAGE CPP #-}
import System.Exit
import Test.HUnit
#ifdef PM36_HASKELL_SCRIPTING
import TutorialD.Interpreter.TestBase
import ProjectM36.Client
import qualified ProjectM36.Attribute as A
import ProjectM36.Relation
#endif

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [
#ifdef PM36_HASKELL_SCRIPTING
    testImportModule,
    testImportTypes
#endif                                  
    ])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
    
    
#ifdef PM36_HASKELL_SCRIPTING
testImportModule :: Test
testImportModule = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let importmodule = "loadmodulefromfile \"test/TutorialD/Interpreter/TestModule.hs\""
  executeTutorialD sess conn importmodule
  --execute the atom function in the module
  executeTutorialD sess conn "x:=relation{tuple{a apply_discount(8,20)}}"
  eres <- executeRelationalExpr sess conn (RelationVariable "x" ())
  let expectedres = mkRelationFromList (A.attributesFromList [Attribute "a" IntegerAtomType]) [[IntegerAtom 10]]
  assertEqual "x apply_discount" expectedres eres
  --execute the dbc function in the module

-- test a variety of types in the module import process  
#endif
