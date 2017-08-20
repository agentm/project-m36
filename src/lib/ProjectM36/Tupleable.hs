{-# LANGUAGE DeriveGeneric, DefaultSignatures, FlexibleInstances, FlexibleContexts, TypeOperators, UndecidableInstances #-}
module ProjectM36.Tupleable where
import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.Atomable
import ProjectM36.Attribute
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Monoid
import Data.Proxy
import Data.Maybe
import ProjectM36.DataTypes.Primitive
import Debug.Trace
              
data Test1T = Test1C {
  attrA :: Int
  }
            deriving (Generic, Show)
                     
data Test2T a b = Test2C {
  attrB :: a,
  attrC :: b
  }
  deriving (Generic, Show)
           
instance (Atomable a, Atomable b, Show a, Show b) => Tupleable (Test2T a b)

instance Tupleable Test1T


class Tupleable a where
  toTuple :: a -> RelationTuple
  
  fromTuple :: RelationTuple -> a
  
  toAttributes :: a -> Attributes

  default toTuple :: (Generic a, TupleableG (Rep a)) => a -> RelationTuple
  toTuple v = toTupleG (from v)
  
  default fromTuple :: (Generic a, TupleableG (Rep a)) => RelationTuple -> a
  fromTuple tup = to (fromTupleG tup)
    
  default toAttributes :: (Generic a, TupleableG (Rep a)) => a -> Attributes
  toAttributes v = toAttributesG (from v)
  
class TupleableG g where  
  toTupleG :: g a -> RelationTuple
  toAttributesG :: g a -> Attributes
  fromTupleG :: RelationTuple -> g a
  
--data type metadata
instance (Datatype c, TupleableG a) => TupleableG (M1 D c a) where
  toTupleG (M1 v) = toTupleG v
  toAttributesG = undefined
  fromTupleG v = M1 (fromTupleG v)

--constructor metadata
instance (Constructor c, TupleableG a, AtomableG a) => TupleableG (M1 C c a) where
  toTupleG (M1 v) = RelationTuple attrs atoms
    where
      attrsToCheck = toAttributesG v
      counter = V.generate (V.length attrsToCheck) id
      attrs = V.zipWith (\num attr@(Attribute name typ) -> if T.null name then 
                                                             Attribute ("attr" <> T.pack (show (num + 1))) typ  
                                                           else
                                                             attr) counter attrsToCheck 
      atoms = V.fromList (toAtomsG v)
  toAttributesG = undefined
  fromTupleG tup = M1 (fromTupleG tup)
  
-- product types
instance (TupleableG a, TupleableG b) => TupleableG (a :*: b) where
  toTupleG = undefined
  toAttributesG (x :*: y) = toAttributesG x V.++ toAttributesG y
  fromTupleG = undefined

--selector/record
instance (Selector c, AtomableG a) => TupleableG (M1 S c a) where
  toTupleG = undefined
  toAttributesG m@(M1 v) = V.singleton (Attribute name aType)
   where
     name = T.pack (selName m)
     aType = toAtomTypeG v
  fromTupleG tup = case atomForAttributeName (T.pack name) tup of
    Left _ -> error "no such record name"
    Right atom -> M1 (atomv atom)
   where
     atomv atom = case fromAtomG atom [atom] of
       Nothing -> error "no such atom conversion"
       Just v -> v
     name = selName (undefined :: M1 S c a x)
     --name = "attrA"


--constructors with no arguments  
--basically useless but orthoganal to relationTrue
instance TupleableG U1 where
  toTupleG = undefined
  toAttributesG _ = emptyAttributes
  fromTupleG = undefined