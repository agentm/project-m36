--Test Atomable typeclass which allows users to use existing Haskell datatypes to marshal them to and from the database as ConstructedAtoms.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, TypeApplications, DerivingVia #-}
import Test.HUnit
import ProjectM36.Client
import Control.DeepSeq
import System.Exit
import TutorialD.Interpreter.TestBase
import GHC.Generics
import ProjectM36.Relation
import ProjectM36.Base
import Data.Time.Calendar (fromGregorian)
import Data.Text (Text)
import qualified Data.Map as M
import Data.Proxy
import Codec.Winery
import Data.List.NonEmpty

{-# ANN module ("Hlint: ignore Use newtype instead of data" :: String) #-}
data Test1T = Test1C Integer
            deriving (Generic, Show, Eq, NFData, Atomable)
            deriving Serialise via WineryVariant Test1T
                    
data Test2T x = Test2C x
              deriving (Show, Generic, Eq, NFData, Atomable)
              deriving Serialise via WineryVariant (Test2T x)
                       
data Test3T = Test3C Integer Integer                        
              deriving (Show, Generic, Eq, NFData, Atomable)
              deriving Serialise via WineryVariant Test3T
                       
data Test4T = Test4Ca Integer |                       
              Test4Cb Integer 
              deriving (Show, Generic, Eq, NFData, Atomable)
              deriving Serialise via WineryVariant Test4T
                       
data TestListT = TestListC [Integer]
              deriving (Show, Generic, Eq, NFData, Atomable)
              deriving Serialise via WineryVariant TestListT
                       
data TestNonEmptyT = TestNonEmptyC (NonEmpty Integer)
              deriving (Show, Generic, Eq, NFData, Atomable)
              deriving Serialise via WineryVariant TestNonEmptyT

data Test5T = Test5C {
  con1 :: Integer,
  con2 :: Integer
  } deriving (Show, Generic, Eq, NFData, Atomable)
  deriving Serialise via WineryRecord Test5T
             
data Test6T = Test6C (Maybe Integer)             
            deriving (Show, Generic, Eq, NFData, Atomable)
            deriving Serialise via WineryVariant Test6T

data Test7T = Test7C (Either Integer Integer)
            deriving (Show, Generic, Eq, NFData, Atomable)
            deriving Serialise via WineryVariant Test7T

data Test8T = Test8C Test1T
            deriving (Show, Generic, Eq, NFData, Atomable)
            deriving Serialise via WineryVariant Test8T

data User = User
  { userFirstName :: Text
  , userLastName :: Text
  } deriving (Eq, Ord, Show, Generic, NFData, Atomable)
    deriving Serialise via WineryRecord User

data Test9_4T = Test9_4C {
  f9_41 :: Int,
  f9_42 :: Int,
  f9_43 :: Int,
  f9_44 :: Int }
              deriving (Show, Generic, Eq, NFData, Atomable)
              deriving Serialise via WineryRecord Test9_4T

data Test9_5T = Test9_5C {
    f9_51 :: Int,
    f9_52 :: Int,
    f9_53 :: Int,
    f9_54 :: Int,
    f9_55 :: Int
    }
              deriving (Show, Generic, Eq, NFData, Atomable)
              deriving Serialise via WineryRecord Test9_5T

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testList :: Test
testList = TestList [testHaskell2DB, testADT1, testADT2, testADT3, testADT4, testADT5, testBasicMarshaling, testSimpleList, testListInstance, testNonEmptyInstance, testADT6Maybe, testADT7Either, testNonPrimitiveValues, testRecordType, testManyFields]

-- test some basic data types like int, day, etc.
testBasicMarshaling :: Test
testBasicMarshaling = TestCase $ do
    assertEqual "to IntAtom" (IntegerAtom 5) (toAtom (5 :: Integer))
    assertEqual "from IntAtom" (5 :: Integer) (fromAtom (IntegerAtom 5))

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
  let test1TExpr = toAddTypeExpr (Proxy :: Proxy Test1T)
      expectedTest1TExpr = AddTypeConstructor (ADTypeConstructorDef "Test1T" []) [DataConstructorDef "Test1C" [DataConstructorDefTypeConstructorArg (PrimitiveTypeConstructor "Integer" IntegerAtomType)]]
  assertEqual "simple ADT1" expectedTest1TExpr test1TExpr
  checkExecuteDatabaseContextExpr sessionId dbconn test1TExpr
  --execute some expressions involving Atomable data types
 
  let createRelExpr = Assign "x" rel
            
      rel = MakeRelationFromExprs Nothing
            (TupleExprs () [TupleExpr (M.singleton "a1" (NakedAtomExpr atomVal))])
      atomVal = toAtom exampleVal
      exampleVal = Test1C 10
  checkExecuteDatabaseContextExpr sessionId dbconn createRelExpr
  
  let retrieveValExpr = Restrict (AttributeEqualityPredicate "a1" (NakedAtomExpr atomVal)) (RelationVariable "x" ())
  ret <- executeRelationalExpr sessionId dbconn retrieveValExpr
  let expectedRel = mkRelationFromList (attributesFromList [Attribute "a1" (toAtomType (Proxy :: Proxy Test1T))]) [[atomVal]]
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

testSimpleList :: Test
testSimpleList = TestCase $ do
  let example = toAtom @[Integer] [1,2,3]
      expected = ConstructedAtom "Cons" (ConstructedAtomType "List" (M.fromList [("a",IntegerAtomType)])) [IntegerAtom 1,ConstructedAtom "Cons" (ConstructedAtomType "List" (M.fromList [("a",IntegerAtomType)])) [IntegerAtom 2,ConstructedAtom "Cons" (ConstructedAtomType "List" (M.fromList [("a",IntegerAtomType)])) [IntegerAtom 3,ConstructedAtom "Empty" (ConstructedAtomType "List" (M.fromList [("a",IntegerAtomType)])) []]]]
  assertEqual "simple list" expected example

testNonEmptyInstance :: Test
testNonEmptyInstance = TestCase $ do
  let example = TestNonEmptyC (3 :| [4,5])
  assertEqual "NonEmpty instance" example (fromAtom (toAtom example))

testNonPrimitiveValues :: Test
testNonPrimitiveValues = TestCase $ do
  let example = Test8C (Test1C 3)
  assertEqual "non-primitive values" example (fromAtom (toAtom example))

testRecordType :: Test
testRecordType = TestCase $ do
  let example = User { userFirstName = "Bob"
                       ,userLastName = "Smith"
                     }
  assertEqual "User record" example (fromAtom (toAtom example))
  let expected = AddTypeConstructor (ADTypeConstructorDef "User" []) [DataConstructorDef "User" [DataConstructorDefTypeConstructorArg (PrimitiveTypeConstructor "Text" TextAtomType),DataConstructorDefTypeConstructorArg (PrimitiveTypeConstructor "Text" TextAtomType)]]

  assertEqual "User record to database context expr" expected (toAddTypeExpr (Proxy :: Proxy User))

--test both odd and even product types with more than 2 fields
testManyFields :: Test
testManyFields = TestCase $ do
  let example4 = Test9_4C 1 2 3 4
  assertEqual "four fields product type" example4 (fromAtom (toAtom example4))

  let example5 = Test9_5C 1 2 3 4 5
  assertEqual "five fields product type" example5 (fromAtom (toAtom example5))
