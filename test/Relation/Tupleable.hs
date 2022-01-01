{-# LANGUAGE
    DeriveGeneric
  , FlexibleInstances
  , FlexibleContexts
  , DeriveAnyClass
  , ScopedTypeVariables
  , TypeApplications
  , DataKinds
  , TypeOperators
  , AllowAmbiguousTypes
  , DerivingVia
  #-}
import Test.HUnit
import ProjectM36.Tupleable.Deriving
import ProjectM36.Atomable
import ProjectM36.Attribute as A
import ProjectM36.Error
import ProjectM36.Base
import Control.DeepSeq (NFData)
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import System.Exit
import Data.Proxy
import Codec.Winery

{-# ANN module ("Hlint: ignore Use newtype instead of data" :: String) #-}

data Test1T = Test1C {
  attrA :: Integer
  }
  deriving (Generic, Eq, Show)
  deriving Serialise via WineryRecord Test1T
           
data Test2T = Test2C {
  attrB :: Integer,
  attrC :: Integer
  }
  deriving (Generic, Eq, Show)
  deriving Serialise via WineryRecord Test2T
           
data Test3T = Test3C Integer            
            deriving (Generic, Eq, Show)
            deriving Serialise via WineryVariant Test3T
                     
data Test4T = Test4C                     
              deriving (Generic, Eq, Show)
              deriving Serialise via WineryVariant Test4T
                       
data Test5T = Test5C1 Integer |                       
              Test5C2 Integer
              deriving (Generic, Eq, Show)
              deriving Serialise via WineryVariant Test5T
                       
data Test6T = Test6C T.Text Integer Double
              deriving (Generic, Eq, Show)
                       
data Test7A = Test7AC Integer
            deriving (Generic, Show, Eq, NFData, Atomable)
            deriving Serialise via WineryVariant Test7A
                       
                       
data Test7T = Test7C Test7A                       
              deriving (Generic, Show, Eq)
              deriving Serialise via WineryVariant Test7T
                       
data Test8T = Test8C                       
              deriving (Generic, Show, Eq)
              deriving Serialise via WineryVariant Test8T
                       
data Test9T = Test9C 
              {                      
                attr9A :: Integer,
                attr9B :: T.Text,
                attr9C :: Double
              }
            deriving (Generic, Show, Eq)
            deriving Serialise via WineryRecord Test9T

data Test10T = Test10C {
  longAttrNameA10 :: Integer,
  long_attr_name_b10 :: Integer,
  longMixed_attr_NameC10 :: Integer
  }
  deriving (Generic, Show, Eq)
  deriving Serialise via WineryRecord Test10T
           
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
testList = TestList [testADT1, testADT2, testADT3, testADT4, testADT6, testADT7, testADT8  ,testInsertExpr, testDefineExpr, testUpdateExpr, testUpdateExprEmptyAttrs, testDeleteExpr, testUpdateExprWrongAttr, testReorderedTuple, testAddPrefixField, testDropPrefixField, testAddSuffixField, testDropSuffixField, testUpperCaseField, testLowerCaseField, testTitleCaseField, testCamelCaseField, testPascalCaseField, testSnakeCaseField, testSpinalCaseField, testTrainCaseField, testAsIsCodec, testAsIsField, testComposeRLCodec, testComposeRLField, testComposeLRCodec, testComposeLRField]

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
  let expectedAttrs = A.singleton (Attribute "" (ConstructedAtomType "Test7A" M.empty))
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
      tupleRev (RelationTuple v1 v2) = RelationTuple (revAttrs v1) (V.reverse v2)
      revAttrs attrs = A.attributesFromList (reverse (A.toList attrs))
      expected = Test2C {attrB = 3, 
                         attrC = 4}
      actual = fromTuple . tupleRev . toTuple $ expected
  assertEqual "reordered tuple" (Right expected) actual

testCodec :: forall tag. ModifyOptions tag => String -> [AttributeName] -> Test
testCodec msg expected = TestCase $
    assertEqual msg (S.fromList expected) actual
  where
    actual = attributeNameSet $ toAttributes (Proxy :: Proxy (Codec tag Test10T))

testAddPrefixField :: Test
testAddPrefixField = testCodec
  @(Field (AddPrefix "prefix_"))
  "codec field add prefix attributes"
  ["prefix_longAttrNameA10",
   "prefix_long_attr_name_b10",
   "prefix_longMixed_attr_NameC10"]

testDropPrefixField :: Test
testDropPrefixField = testCodec
  @(Field (DropPrefix "long"))
  "codec field drop prefix"
  ["AttrNameA10",
   "_attr_name_b10",
   "Mixed_attr_NameC10"]

testAddSuffixField :: Test
testAddSuffixField = testCodec
  @(Field (AddSuffix "_suffix"))
  "codec field add suffix"
  ["longAttrNameA10_suffix",
   "long_attr_name_b10_suffix",
   "longMixed_attr_NameC10_suffix"]

testDropSuffixField :: Test
testDropSuffixField = testCodec
  @(Field (DropSuffix "10"))
  "codec field drop suffix"
  ["longAttrNameA",
   "long_attr_name_b",
   "longMixed_attr_NameC"]

testUpperCaseField :: Test
testUpperCaseField = testCodec
  @(Field UpperCase)
  "codec field uppercase"
  ["LONGATTRNAMEA10",
   "LONG_ATTR_NAME_B10",
   "LONGMIXED_ATTR_NAMEC10"]

testLowerCaseField :: Test
testLowerCaseField = testCodec
  @(Field LowerCase)
  "codec field lowercase"
  ["longattrnamea10",
   "long_attr_name_b10",
   "longmixed_attr_namec10"]

testTitleCaseField :: Test
testTitleCaseField = testCodec
  @(Field TitleCase)
  "codec field title case"
  ["Long Attr Name A10",
   "Long Attr Name B10",
   "Long Mixed Attr Name C10"]

testCamelCaseField :: Test
testCamelCaseField = testCodec
  @(Field CamelCase)
  "codec field camel case"
  ["longAttrNameA10",
   "longAttrNameB10",
   "longMixedAttrNameC10"]

testPascalCaseField :: Test
testPascalCaseField = testCodec
  @(Field PascalCase)
  "codec field pascal case"
  ["LongAttrNameA10",
   "LongAttrNameB10",
   "LongMixedAttrNameC10"]

testSnakeCaseField :: Test
testSnakeCaseField = testCodec
  @(Field SnakeCase)
  "codec field snake case"
  ["long_attr_name_a10",
   "long_attr_name_b10",
   "long_mixed_attr_name_c10"]

testSpinalCaseField :: Test
testSpinalCaseField = testCodec
  @(Field SpinalCase)
  "codec field spinal case"
  ["long-attr-name-a10",
   "long-attr-name-b10",
   "long-mixed-attr-name-c10"]

testTrainCaseField :: Test
testTrainCaseField = testCodec
  @(Field TrainCase)
  "codec field train case"
  ["Long-Attr-Name-A10",
   "Long-Attr-Name-B10",
   "Long-Mixed-Attr-Name-C10"]

testAsIsCodec :: Test
testAsIsCodec = testCodec
  @AsIs
  "codec as is"
  ["longAttrNameA10",
   "long_attr_name_b10",
   "longMixed_attr_NameC10"]

testAsIsField :: Test
testAsIsField = testCodec
  @(Field AsIs)
  "codec field as is"
  ["longAttrNameA10",
   "long_attr_name_b10",
   "longMixed_attr_NameC10"]

testComposeRLCodec :: Test
testComposeRLCodec = testCodec
  @(Field UpperCase <<< Field (DropPrefix "long"))
  "codec (<<<)"
  ["ATTRNAMEA10",
   "_ATTR_NAME_B10",
   "MIXED_ATTR_NAMEC10"]

testComposeRLField :: Test
testComposeRLField = testCodec
  @(Field (UpperCase <<< DropPrefix "long"))
  "codec field (<<<)"
  ["ATTRNAMEA10",
   "_ATTR_NAME_B10",
   "MIXED_ATTR_NAMEC10"]

testComposeLRCodec :: Test
testComposeLRCodec = testCodec
  @(Field (DropPrefix "long") >>> Field UpperCase)
  "codec (>>>)"
  ["ATTRNAMEA10",
   "_ATTR_NAME_B10",
   "MIXED_ATTR_NAMEC10"]

testComposeLRField :: Test
testComposeLRField = testCodec
  @(Field (DropPrefix "long" >>> UpperCase))
  "codec field (>>>)"
  ["ATTRNAMEA10",
   "_ATTR_NAME_B10",
   "MIXED_ATTR_NAMEC10"]
