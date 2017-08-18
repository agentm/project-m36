{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DefaultSignatures, FlexibleInstances, FlexibleContexts, TypeOperators #-}
module ProjectM36.Tupleable where
import ProjectM36.Base
import ProjectM36.Atomable
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Monoid

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
              
class Tupleable a where
  toTuple :: a -> RelationTuple

  default toTuple :: (Generic a, TupleableG (Rep a)) => a -> RelationTuple
  toTuple v = toTupleG (from v)
  
class TupleableG g where  
  toTupleG :: g a -> RelationTuple
  toAttributesG :: g a -> Attributes
  
--data type metadata
instance (Datatype c, TupleableG a) => TupleableG (M1 D c a) where
  toTupleG (M1 v) = toTupleG v
  toAttributesG = undefined

--constructor metadata
instance (Constructor c, TupleableG a, AtomableG a) => TupleableG (M1 C c a) where
  toTupleG (M1 v) = RelationTuple attrs atoms
    where
      attrsToCheck = toAttributesG v
      counter = V.generate (V.length attrsToCheck) id
      attrs = V.zipWith (\num attr@(Attribute name typ) -> if T.null name then 
                                                             Attribute ("attr" <> (T.pack (show (num + 1)))) typ  
                                                           else
                                                             attr) counter attrsToCheck 
      atoms = V.fromList (toAtomsG v)
  toAttributesG = undefined 
  
-- product types
instance (TupleableG a, TupleableG b) => TupleableG (a :*: b) where
  toTupleG = undefined
  toAttributesG (x :*: y) = toAttributesG x V.++ toAttributesG y

--selector/record
instance (Selector c, AtomableG a) => TupleableG (M1 S c a) where
  toTupleG = undefined
  toAttributesG m@(M1 v) = V.singleton (Attribute name aType)
   where
     name = T.pack (selName m)
     aType = toAtomTypeG v
  
--constructors with no arguments  
--basically useless but orthoganal to relationTrue
instance TupleableG U1 where
  toTupleG = undefined
  toAttributesG _ = V.empty