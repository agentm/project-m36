{-# LANGUAGE DeriveGeneric, FlexibleInstances, FlexibleContexts, TypeOperators, DeriveAnyClass #-}
import Test.HUnit
import ProjectM36.Tupleable
import ProjectM36.Atomable
import ProjectM36.Attribute
import ProjectM36.Error
import Data.Binary
import ProjectM36.Base
import Control.DeepSeq (NFData)
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import System.Exit
import Data.Proxy

{-# ANN module ("Hlint: ignore Use newtype instead of data" :: String) #-}

data Test1T = Test1C {
  attrA :: Integer
  }
  deriving (Generic, Eq, Show)
           
data Test2T = Test2C {
  attrB :: Integer,
  attrC :: Integer
  }
  deriving (Generic, Eq, Show)
           
data Test3T = Test3C Integer            
            deriving (Generic, Eq, Show)
                     
data Test4T = Test4C                     
              deriving (Generic, Eq, Show)
                       
data Test5T = Test5C1 Integer |                       
              Test5C2 Integer
              deriving (Generic, Eq, Show)
                       
data Test6T = Test6C T.Text Integer Double
              deriving (Generic, Eq, Show)
                       
data Test7A = Test7AC Integer
            deriving (Generic, Show, Eq, Binary, NFData, Atomable)
                       
                       
data Test7T = Test7C Test7A                       
              deriving (Generic, Show, Eq)
                       
data Test8T = Test8C                       
              deriving (Generic, Show, Eq)
                       
data Test9T = Test9C 
              {                      
                attr9A :: Integer,
                attr9B :: T.Text,
                attr9C :: Double
              }
            deriving (Generic, Show, Eq)
           
instance Tupleable Test1T

instance Tupleable Test2T

instance Tupleable Test3T

instance Tupleable Test4T

--instance Tupleable Test5T -- should fail at compile time- sum types not supported

instance Tupleable Test6T

instance Tupleable Test7T

instance Tupleable Test8T

instance Tupleable Test9T

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testList :: Test
testList = TestList [testADT1, testADT2, testADT3, testADT4, testADT6, testADT7, testADT8, testInsertExpr, testDefineExpr, testUpdateExpr, testUpdateExprEmptyAttrs, testDeleteExpr, testUpdateExprWrongAttr, testReorderedTuple]

testADT1 :: Test
testADT1 = TestCase $ assertEqual "one record constructor" (Right example) (fromTuple (toTuple example))
  where 
    example = Test1C {attrA = 3}

testADT2 :: Test
testADT2 = TestCase $ do
  let example = Test2C { attrB = 4, attrC = 6 }
  assertEqual "two record constructor" (Right example) (fromTuple (toTuple example))
  --this was a tricky case that was throwing the undefined exception because of insufficient laziness in the :*: matching- see ProjectM36.Tupleable
  let expectedAttributes = attributesFromList [Attribute "attrB" IntegerAtomType,
                                               Attribute "attrC" IntegerAtomType]
  assertEqual "two record constructor toAttributes" expectedAttributes (toAttributes (Proxy :: Proxy Test2T))
  
    
testADT3 :: Test
testADT3 = TestCase $ assertEqual "one arg constructor" (Right example) (fromTuple (toTuple example))
  where
    example = Test3C 4
    
testADT4 :: Test    
testADT4 = TestCase $ assertEqual "zero arg constructor" (Right example) (fromTuple (toTuple example))
  where
    example = Test4C 
    
--testADT5 should not compile    
    
testADT6 :: Test    
testADT6 = TestCase $ assertEqual "mixed types" (Right example) (fromTuple (toTuple example))
  where
    example = Test6C "testo" 3 4.0
    
testADT7 :: Test    
testADT7 = TestCase $ do
  let example = Test7C (Test7AC 3)
  assertEqual "atom type" (Right example) (fromTuple (toTuple example))
  let expectedAttrs = V.singleton (Attribute "" (ConstructedAtomType "Test7A" M.empty))
  assertEqual "adt atomtype" expectedAttrs (toAttributes (Proxy :: Proxy Test7T))
    
testADT8 :: Test    
testADT8 = TestCase $ assertEqual "single value" (Right example) (fromTuple (toTuple example))
  where
    example = Test8C
    
testInsertExpr :: Test    
testInsertExpr = TestCase $ assertEqual "insert expr" expected (toInsertExpr [Test9C {
                                                                                 attr9A = 4, 
                                                                                 attr9B = "a", 
                                                                                 attr9C = 3.4}] "rv")
  where expected = Right (Insert "rv" (MakeStaticRelation attrs tuples))
        attrs = attributesFromList [Attribute "attr9A" IntegerAtomType,
                                    Attribute "attr9B" TextAtomType,
                                    Attribute "attr9C" DoubleAtomType]
        tuples = RelationTupleSet [RelationTuple attrs (V.fromList [IntegerAtom 4,
                                                                    TextAtom "a",
                                                                    DoubleAtom 3.4])]
                 
testDefineExpr :: Test                 
testDefineExpr = TestCase $ assertEqual "define expr" expected actual
  where
    expected = Define "rv" (map NakedAttributeExpr attrs)
    attrs = [Attribute "attr9A" IntegerAtomType,
             Attribute "attr9B" TextAtomType,
             Attribute "attr9C" DoubleAtomType]
    actual = toDefineExpr (Proxy :: Proxy Test9T) "rv"
    
testUpdateExpr :: Test    
testUpdateExpr = TestCase $ do
  let expected = Right (Update "rv" updateMap restriction)
      updateMap = M.fromList [("attr9B", NakedAtomExpr (TextAtom "b")),
                              ("attr9C", NakedAtomExpr (DoubleAtom 5.5))]
      restriction = AttributeEqualityPredicate "attr9A" (NakedAtomExpr (IntegerAtom 5))
      actual = toUpdateExpr "rv" ["attr9A"] Test9C {attr9A = 5,
                                                     attr9B = "b",
                                                     attr9C = 5.5}
                                                              
  assertEqual "update expr1" expected actual
  
testUpdateExprEmptyAttrs :: Test  
testUpdateExprEmptyAttrs = TestCase $ do
  let expected = Left EmptyAttributesError
      actual = toUpdateExpr "rv" [] Test9C {attr9A = 5,
                                             attr9B = "b",
                                             attr9C = 5.5}
  assertEqual "update with empty attrs" expected actual
  
testUpdateExprWrongAttr :: Test  
testUpdateExprWrongAttr = TestCase $ do
  --currently, passing in the wrong attribute replaces the whole relvar with a single tuple- is this what we want?
  let expected = Left (NoSuchAttributeNamesError (S.singleton "nonexistentattr"))
      actual = toUpdateExpr "rv" ["nonexistentattr"] Test9C {attr9A = 5,
                                                              attr9B = "b",
                                                              attr9C = 5.5}
  assertEqual "update with wrong attr" expected actual               
  
testDeleteExpr :: Test  
testDeleteExpr = TestCase $ do
  let expected = Right (Delete "rv" (AndPredicate (AttributeEqualityPredicate "attr9A" 
                                            (NakedAtomExpr (IntegerAtom 5)))
                                           (AttributeEqualityPredicate "attr9B"
                                            (NakedAtomExpr (TextAtom "b")))))
      actual = toDeleteExpr "rv" ["attr9A", "attr9B"] Test9C {attr9A = 5,
                                                               attr9B = "b",
                                                               attr9C = 5.5}
  assertEqual "delete expr" expected actual
                              
--discovered in #184 by ruhatch    
testReorderedTuple :: Test
testReorderedTuple = TestCase $ do
  let tupleRev :: RelationTuple -> RelationTuple
      tupleRev (RelationTuple v1 v2) = RelationTuple (V.reverse v1) (V.reverse v2)
      expected = Test2C {attrB = 3, 
                         attrC = 4}
      actual = fromTuple . tupleRev . toTuple $ expected
  assertEqual "reordered tuple" (Right expected) actual
