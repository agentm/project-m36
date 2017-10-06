{-# LANGUAGE DeriveGeneric, FlexibleInstances, FlexibleContexts, TypeOperators, DeriveAnyClass #-}
import Test.HUnit
import ProjectM36.Tupleable
import ProjectM36.Atomable
import ProjectM36.Attribute
import Data.Binary
import ProjectM36.Base
import Control.DeepSeq (NFData)
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Text as T
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
           
instance Tupleable Test1T

instance Tupleable Test2T

instance Tupleable Test3T

instance Tupleable Test4T

--instance Tupleable Test5T -- should fail at compile time- sum types not supported

instance Tupleable Test6T

instance Tupleable Test7T

instance Tupleable Test8T

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testList :: Test
testList = TestList [testADT1, testADT2, testADT3, testADT4, testADT6, testADT7, testADT8]

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