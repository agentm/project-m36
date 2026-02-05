-- | Test import via ProjectM36.Module which allows a user to import any number of atom functions or database context functions.
{-# LANGUAGE CPP #-}
import System.Exit
import Test.HUnit
#ifdef PM36_HASKELL_SCRIPTING
import Data.Time.Calendar
import TutorialD.Interpreter.TestBase
import ProjectM36.Client
import qualified ProjectM36.Attribute as A
import ProjectM36.Relation
#endif

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [
#ifdef PM36_HASKELL_SCRIPTING
    testImportModule
#endif                                  
    ])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
    
    
#ifdef PM36_HASKELL_SCRIPTING
testImportModule :: Test
testImportModule = TestCase $ do
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  let importmodule = "loadmodulefromfile \"test/TutorialD/Interpreter/TestModule.hs\""
  executeTutorialD sess conn importmodule
  -- install relvar
  executeTutorialD sess conn "ticket_sales:=relation{ticketId Integer, visitorAge Integer, basePrice Integer, visitDate Day}"
  --execute the atom function in the module
  executeTutorialD sess conn "x:=relation{tuple{a applyDiscount(8,20)}}"
  eres <- executeRelationalExpr sess conn (RelationVariable "x" ())
  let expectedres = mkRelationFromList (A.attributesFromList [Attribute "a" IntegerAtomType]) [[IntegerAtom 10]]
  assertEqual "x applyDiscount" expectedres eres

  --execute the dbc function in the module
  executeTutorialD sess conn "execute addSale(1,8,20,fromGregorian(2025, 10, 3))"
  let salesAttrs = A.attributesFromList [Attribute "ticketId" IntegerAtomType,
                                         Attribute "visitorAge" IntegerAtomType,
                                         Attribute "basePrice" IntegerAtomType,
                                         Attribute "visitDate" DayAtomType]
  let expectedres' = mkRelationFromList salesAttrs [[IntegerAtom 1, IntegerAtom 8, IntegerAtom 10, DayAtom (fromGregorian 2025 10 03)]]
  eres' <- executeRelationalExpr sess conn (RelationVariable "ticket_sales" ())
  assertEqual "ticket_sales addSale" expectedres' eres'
#endif
