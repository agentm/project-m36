{-# LANGUAGE DeriveGeneric, DefaultSignatures, FlexibleInstances, FlexibleContexts, TypeOperators #-}
import Test.HUnit
import ProjectM36.Tupleable
import ProjectM36.Base
import TutorialD.Interpreter.TestBase
import GHC.Generics

data Test1T = Test1C {
  attrA :: Int
  }
  deriving (Generic)
           
data Test2T = Test2C {
  attrB :: Int,
  attrC :: Int
  }
  deriving (Generic)
           
data Test3T = Test3C Int            
            deriving Generic
                     
data Test4T = Test4C                     
              deriving Generic
                       
data Test5T = Test5C1 Int |                       
              Test5C2 Int
              deriving Generic
                       
data Test6T = Test6C T.Text Int Double
              deriving Generic
           
instance Tupleable Test1T

instance Tupleable Test2T

instance Tupleable Test3T

instance Tupleable Test4T

--instance Tupleable Test5T -- should fail at compile time- sum types not supported

instance Tupleable Test6T

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testList :: Test
testList = TestList [testADT1]

testADT1 :: Test
testADT1 = TestCase $ do
  let example = Test1C {attr1 = 3}
  assertEqual "one arg constructor" example (fromTuple (toTuple example))
