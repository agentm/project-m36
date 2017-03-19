{-# LANGUAGE DefaultSignatures, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, DeriveGeneric #-}
module ProjectM36.ConstructedAtom where
--http://stackoverflow.com/questions/13448361/type-families-with-ghc-generics-or-data-data
--instances to marshal Haskell ADTs to ConstructedAtoms and back
import ProjectM36.Base
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Typeable

data TestT = TestC Int
            deriving Generic
                    
data Test2T x = Test2C x
              deriving Generic
                    
instance Atomable TestT

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

-- Generics
class AtomableG g where
  --type AtomTG g
  toAtomG :: g a -> Atom
  toAtomTypeG :: g a -> AtomType
  
class TypeVarMappableG g where  
  toTypeVarMap :: g a -> TypeVarMap
  
instance (AtomableG a, AtomableG b) => AtomableG (a :*: b) where
  --type AtomTG (a :*: b) = (AtomTG a, AtomTG b)
  toAtomG v = undefined
  toAtomTypeG v = undefined
  
instance (AtomableG a, AtomableG b) => AtomableG (a :+: b) where
  --type AtomTG (a :+: b) = (AtomTG a, AtomTG b)
  toAtomG v = undefined
  toAtomTypeG v = undefined
  
--data type metadata
instance (Datatype c, TypeVarMappableG a) => AtomableG (M1 D c a) where  
  --type AtomTG (M1 D c a) = AtomTG a
  toAtomG = undefined
  toAtomTypeG (M1 v) = ConstructedAtomType (T.pack typeName) tvMap
    where
      typeName = datatypeName (undefined :: M1 D c a x)
      tvMap = toTypeVarMap v
      
instance TypeVarMappableG (M1 C c a) where
  --M.Map TypeVarName AtomType
  toTypeVarMap (M1 v) = M.empty
      
{-
instance Atomable c => AtomableG (M1 S c (Rec0 f)) where
  toAtomG = undefined
  toAtomTypeG (M1 v) = toAtomType v
  
-}  
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