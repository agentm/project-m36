--Test Atomable typeclass which allows users to use existing Haskell datatypes to marshal them to and from the database as ConstructedAtoms.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
import Test.HUnit
import ProjectM36.Client
import Data.Binary
import Control.DeepSeq
import System.Exit
import TutorialD.Interpreter.TestBase
import GHC.Generics
import ProjectM36.Relation
import ProjectM36.Base
import Data.Time.Calendar (fromGregorian)
import Data.Text
import qualified Data.Map as M

{-# ANN module ("Hlint: ignore Use newtype instead of data" :: String) #-}
data Test1T = Test1C Integer
            deriving (Generic, Show, Eq, Binary, NFData, Atomable)
                    
data Test2T x = Test2C x
              deriving (Show, Generic, Eq, Binary, NFData, Atomable)
                       
data Test3T = Test3C Integer Integer                        
              deriving (Show, Generic, Eq, Binary, NFData, Atomable)
                       
data Test4T = Test4Ca Integer |                       
              Test4Cb Integer 
              deriving (Show, Generic, Eq, Binary, NFData, Atomable)
                       
data TestListT = TestListC [Integer]
              deriving (Show, Generic, Eq, Binary, NFData, Atomable)
                       
data Test5T = Test5C {
  con1 :: Integer,
  con2 :: Integer
  } deriving (Show, Generic, Eq, Binary, NFData, Atomable)
             
data Test6T = Test6C (Maybe Integer)             
            deriving (Show, Generic, Eq, Binary, NFData, Atomable)

data Test7T = Test7C (Either Integer Integer)
            deriving (Show, Generic, Eq, Binary, NFData, Atomable)
                       
main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testList :: Test
testList = TestList [testHaskell2DB, testADT1, testADT2, testADT3, testADT4, testADT5, testBasicMarshaling, testListInstance, testADT6Maybe, testADT7Either]

-- test some basic data types like int, day, etc.
testBasicMarshaling :: Test
testBasicMarshaling = TestCase $ do
    assertEqual "to IntAtom" (IntAtom 5) (toAtom (5 :: Integer))
    assertEqual "from IntAtom" (5 :: Integer) (fromAtom (IntAtom 5))

    assertEqual "to BoolAtom" (BoolAtom False) (toAtom False)
    assertEqual "from BoolAtom" False (fromAtom (BoolAtom False))

    let day = fromGregorian 2012 10 19
    assertEqual "to DayAtom" (DayAtom day) (toAtom day)
    assertEqual "from DayAtom" day (fromAtom (DayAtom day))
  
--test marshaling of Generics-derived Atom to database ADT
testHaskell2DB :: Test
testHaskell2DB = TestCase $ do
  --validate generated database context expression
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  let test1TExpr = toAddTypeExpr (undefined :: Test1T)
      expectedTest1TExpr = AddTypeConstructor (ADTypeConstructorDef "Test1T" []) [DataConstructorDef "Test1C" [DataConstructorDefTypeConstructorArg (PrimitiveTypeConstructor "Int" IntAtomType)]]
  assertEqual "simple ADT1" expectedTest1TExpr test1TExpr
  checkExecuteDatabaseContextExpr sessionId dbconn test1TExpr
  --execute some expressions involving Atomable data types
 
  let createRelExpr = Assign "x" rel
            
      rel = MakeRelationFromExprs Nothing
            [TupleExpr (M.singleton "a1" (NakedAtomExpr atomVal))]
      atomVal = toAtom exampleVal
      exampleVal = Test1C 10
  checkExecuteDatabaseContextExpr sessionId dbconn createRelExpr
  
  let retrieveValExpr = Restrict (AttributeEqualityPredicate "a1" (NakedAtomExpr atomVal)) (RelationVariable "x" ())
  ret <- executeRelationalExpr sessionId dbconn retrieveValExpr
  let expectedRel = mkRelationFromList (attributesFromList [Attribute "a1" (toAtomType exampleVal)]) [[atomVal]]
  assertEqual "retrieve atomable atom" expectedRel ret
  
testADT1 :: Test
testADT1 = TestCase $ do
  let example = Test1C 3
  assertEqual "one arg constructor" example (fromAtom (toAtom example))
  
testADT2 :: Test  
testADT2 = TestCase $ do
  let sampletext = "text" :: Text
      example = Test2C sampletext
  assertEqual "polymorphic type constructor" example (fromAtom (toAtom example))
  
testADT3 :: Test  
testADT3 = TestCase $ do
  let example = Test3C 3 4
  assertEqual "product type" example (fromAtom (toAtom example))
  
testADT4 :: Test  
testADT4 = TestCase $ do
  let example = Test4Ca 3
      example2 = Test4Cb 4
  assertEqual "sum type + one constructor arg 1" example (fromAtom (toAtom example))
  assertEqual "sum type + one constructor arg 2" example2 (fromAtom (toAtom example2))
  
testADT5 :: Test  
testADT5 = TestCase $ do
  let example = Test5C {con1=3, con2=4}
  assertEqual "record-based ADT" example (fromAtom (toAtom example))  
  
testADT6Maybe :: Test  
testADT6Maybe = TestCase $ do
  let example = Test6C (Just 4)
  assertEqual "maybe type" example (fromAtom (toAtom example))
  
testADT7Either :: Test 
testADT7Either = TestCase $ do
  let example = Test7C (Right 10)
  assertEqual "either type" example (fromAtom (toAtom example))
  
checkExecuteDatabaseContextExpr :: SessionId -> Connection -> DatabaseContextExpr -> IO ()
checkExecuteDatabaseContextExpr sessionId dbconn expr = executeDatabaseContextExpr sessionId dbconn expr >>= either (assertFailure . show) (const (pure ()))

testListInstance :: Test
testListInstance = TestCase $ do
  let example = TestListC [3,4,5]
  assertEqual "List instance" example (fromAtom (toAtom example))