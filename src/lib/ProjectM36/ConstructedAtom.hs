{-# LANGUAGE DefaultSignatures, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, DeriveGeneric #-}
module ProjectM36.ConstructedAtom where
--http://stackoverflow.com/questions/13448361/type-families-with-ghc-generics-or-data-data
--instances to marshal Haskell ADTs to ConstructedAtoms and back
import ProjectM36.Base
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as M
--import Data.Typeable
--import Debug.Trace

data TestT = TestC Int
            deriving (Generic, Show)
                    
data Test2T x = Test2C x
              deriving (Show, Generic)
                    
instance Atomable TestT
instance Atomable a => Atomable (Test2T a)

class Atomable a where
  --type AtomT a
  toAtom :: a -> Atom
  default toAtom :: (Generic a, AtomableG (Rep a)) => a -> Atom
  toAtom v = toAtomG (from v)
  
  toAtomType :: a -> AtomType
  default toAtomType :: (Generic a, AtomableG (Rep a)) => a -> AtomType
  toAtomType v = toAtomTypeG (from v)
  
instance Atomable Int where  
  toAtom i = IntAtom i
  toAtomType _ = IntAtomType

instance Atomable Text where
  toAtom i = TextAtom i
  toAtomType _ = TextAtomType
  
-- Generics
class AtomableG g where
  --type AtomTG g
  toAtomG :: g a -> Atom
  toAtomTypeG :: g a -> AtomType --overall ConstructedAtomType
  toAtomTypesG :: g a -> [AtomType] --built from individual atom types
  
{-
instance (AtomableG a, AtomableG b) => AtomableG (a :*: b) where
  --type AtomTG (a :*: b) = (AtomTG a, AtomTG b)
  toAtomG v = undefined
  toAtomTypeG v = undefined
-}  
-- Right | Left  
  {-
instance (AtomableG (a p), AtomableG (b p)) => AtomableG ((a :+: b) p) where
  --type AtomTG (a :+: b) = (AtomTG a, AtomTG b)
  toAtomG v = undefined
  toAtomTypeG v = undefined
  
-- Test Int Int  
instance (Atomable (a p), Atomable (b p)) => AtomableG ((a :*: b) p) where
  toAtomG = undefined
  toAtomTypeG (x :*: y) = undefined
-}
  
--data type metadata
instance (Datatype c, AtomableG a) => AtomableG (M1 D c a) where  
  --type AtomTG (M1 D c a) = AtomTG a
  toAtomG = undefined
  toAtomTypesG = undefined
  toAtomTypeG (M1 v) = ConstructedAtomType (T.pack typeName) tvMap
    where
      aTypes = toAtomTypesG v
      a2z = map T.singleton ['a'..'z']
      tvMap = M.fromList (zip a2z aTypes)
      typeName = datatypeName (undefined :: M1 D c a x)
  

instance (Constructor c, AtomableG a) => AtomableG (M1 C c a) where
  --M.Map TypeVarName AtomType
  --constructor name needed for Atom but not for atomType
  toAtomG (M1 _) = ConstructedAtom (T.pack constructorName) AnyAtomType []
    where
      constructorName = conName (undefined :: M1 C c a x)
  toAtomTypeG = undefined
  toAtomTypesG (M1 v) = toAtomTypesG v

instance Atomable f => AtomableG (M1 S c (Rec0 f)) where
  toAtomG = undefined
  toAtomTypeG = undefined
  toAtomTypesG _ = [toAtomType (undefined :: f)]

{-
--constructor metadata
instance AtomableG a => AtomableG (M1 C c a) where
  --type AtomTG (M1 C c a) = AtomTG a
  toAtomG v = undefined
  toAtomTypeG v = 
  
--record selector metdata
instance (Selector c, AtomableG a) => AtomableG (M1 S c a) where  
  --type AtomTG (M1 S c a) = AtomTG a
  toAtomG v = undefined
  toAtomTypeG v = undefined

--constants or recursion
instance Atomable a => AtomableG (K1 i a) where  
  --type AtomTG (K1 i s) = AtomT s
  
toAtomG (K1 a) = toAtom a
  toAtomTypeG = undefined  
-}