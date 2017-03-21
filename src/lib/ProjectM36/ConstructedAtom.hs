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
            deriving (Generic, Show, Eq)
                    
data Test2T x = Test2C x
              deriving (Show, Generic, Eq)
                       
data Test3T = Test3C Int Int                        
              deriving (Show, Generic, Eq)
                       
data Test4T = Test4Ca Int |                       
              Test4Cb Int 
              deriving (Show, Generic, Eq)
                       
data TyTest a = TyTest a                       

instance Atomable TestT
instance Atomable a => Atomable (Test2T a)
instance Atomable Test3T
--instance Atomable Test4T

class (Eq a, Show a) => Atomable a where
  --type AtomT a
  toAtom :: a -> Atom
  default toAtom :: (Generic a, AtomableG (Rep a)) => a -> Atom
  toAtom v = toAtomG (from v) (toAtomTypeG (from v))
  
  toAtomType :: a -> AtomType
  default toAtomType :: (Generic a, AtomableG (Rep a)) => a -> AtomType
  toAtomType v = toAtomTypeG (from v)
  
instance Atomable Int where  
  toAtom i = IntAtom i
  toAtomType _ = IntAtomType

instance Atomable T.Text where
  toAtom i = TextAtom i
  toAtomType _ = TextAtomType
  
-- Generics
class AtomableG g where
  --type AtomTG g
  toAtomG :: g a -> AtomType -> Atom
  toAtomTypeG :: g a -> AtomType --overall ConstructedAtomType
  toAtomsG :: g a -> [Atom]
  
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
  toAtomG (M1 v) t = toAtomG v t
  toAtomsG = undefined
  toAtomTypeG _ = ConstructedAtomType (T.pack typeName) M.empty -- generics don't allow us to get the type constructor variables- alternatives?
    where
      typeName = datatypeName (undefined :: M1 D c a x)
  

instance (Constructor c, AtomableG a) => AtomableG (M1 C c a) where
  --constructor name needed for Atom but not for atomType
  toAtomG (M1 v) t = ConstructedAtom (T.pack constructorName) t atoms
    where
      atoms = toAtomsG v
      constructorName = conName (undefined :: M1 C c a x)
  toAtomsG = undefined
  toAtomTypeG = undefined

instance (Selector c, AtomableG a) => AtomableG (M1 S c a) where
  toAtomG = undefined
  toAtomsG (M1 v) = toAtomsG v
  toAtomTypeG (M1 v) = toAtomTypeG v

instance (Atomable a) => AtomableG (K1 c a) where
  toAtomG (K1 v) _ = toAtom v
  toAtomsG (K1 v) = [toAtom v]
  toAtomTypeG (K1 v) = toAtomType (undefined :: a)

instance (AtomableG a, AtomableG b) => AtomableG (a :*: b) where
  toAtomG = undefined
  toAtomTypeG = undefined
  toAtomsG (x :*: y) = toAtomsG x ++ toAtomsG y
