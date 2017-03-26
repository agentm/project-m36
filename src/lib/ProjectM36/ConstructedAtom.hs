{-# LANGUAGE DefaultSignatures, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.ConstructedAtom where
--http://stackoverflow.com/questions/13448361/type-families-with-ghc-generics-or-data-data
--instances to marshal Haskell ADTs to ConstructedAtoms and back
import ProjectM36.Base
import ProjectM36.DataTypes.Primitive
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as M
import Control.DeepSeq (NFData)
import Data.Binary

--also add haskell scripting atomable support
--rename this module to Atomable along with test

data Test1T = Test1C Int
            deriving (Generic, Show, Eq, Binary, NFData, Atomable)
                     
data Test2T = Test2C Int Int                     
            deriving (Generic, Show, Eq, Binary, NFData, Atomable)

class (Eq a, NFData a, Binary a, Show a) => Atomable a where
  toAtom :: a -> Atom
  default toAtom :: (Generic a, AtomableG (Rep a)) => a -> Atom
  toAtom v = toAtomG (from v) (toAtomTypeG (from v))
  
  fromAtom :: Atom -> a
  default fromAtom :: (Generic a, AtomableG (Rep a)) => Atom -> a
  fromAtom = to `fmap` fromAtomG
  
  toAtomType :: a -> AtomType
  default toAtomType :: (Generic a, AtomableG (Rep a)) => a -> AtomType
  toAtomType v = toAtomTypeG (from v)
                      
  --creates DatabaseContextExpr necessary to load the type constructor and data constructor into the database
  toDatabaseContextExpr :: a -> DatabaseContextExpr
  default toDatabaseContextExpr :: (Generic a, AtomableG (Rep a)) => a -> DatabaseContextExpr
  toDatabaseContextExpr v = toDatabaseContextExprG (from v) (toAtomType v)
  
instance Atomable Int where  
  toAtom i = IntAtom i
  fromAtom (IntAtom i) = i
  fromAtom e = error ("improper fromAtom" ++ show e)
  toAtomType _ = IntAtomType
  toDatabaseContextExpr _ = NoOperation

instance Atomable T.Text where
  toAtom t = TextAtom t
  fromAtom (TextAtom t) = t
  fromAtom _ = error "improper fromAtom"  
  toAtomType _ = TextAtomType
  toDatabaseContextExpr _ = NoOperation
  
-- Generics
class AtomableG g where
  --type AtomTG g
  toAtomG :: g a -> AtomType -> Atom
  fromAtomG :: Atom -> g a
  toAtomTypeG :: g a -> AtomType --overall ConstructedAtomType
  toAtomsG :: g a -> [Atom]
  toDatabaseContextExprG :: g a -> AtomType -> DatabaseContextExpr
  getConstructorsG :: g a -> [DataConstructorDef]
  getConstructorArgsG :: g a -> [DataConstructorDefArg]
  
--data type metadata
instance (Datatype c, AtomableG a) => AtomableG (M1 D c a) where  
  toAtomG (M1 v) t = toAtomG v t
  fromAtomG = M1 <$> fromAtomG
  toAtomsG = undefined
  toAtomTypeG _ = ConstructedAtomType (T.pack typeName) M.empty -- generics don't allow us to get the type constructor variables- alternatives?
    where
      typeName = datatypeName (undefined :: M1 D c a x)
  toDatabaseContextExprG (M1 v) (ConstructedAtomType tcName _) = AddTypeConstructor tcDef dataConstructors
    where
      tcDef = ADTypeConstructorDef tcName []
      dataConstructors = getConstructorsG v
  toDatabaseContextExprG _ _ = NoOperation      
  getConstructorsG (M1 v) = getConstructorsG v
  getConstructorArgsG = undefined
  
--constructor metadata
instance (Constructor c, AtomableG a) => AtomableG (M1 C c a) where
  --constructor name needed for Atom but not for atomType
  toAtomG (M1 v) t = ConstructedAtom (T.pack constructorName) t atoms
    where
      atoms = toAtomsG v
      constructorName = conName (undefined :: M1 C c a x)
  fromAtomG = M1 <$> fromAtomG
  toAtomsG = undefined
  toAtomTypeG = undefined
  toDatabaseContextExprG = undefined  
  getConstructorsG (M1 v) = [DataConstructorDef (T.pack dName) dArgs]
    where
      dName = conName (undefined :: M1 C c a x)
      dArgs = getConstructorArgsG v
  getConstructorArgsG = undefined

--field metadata
instance (Selector c, AtomableG a) => AtomableG (M1 S c a) where
  toAtomG = undefined
  fromAtomG = M1 <$> fromAtomG
  toAtomsG (M1 v) = toAtomsG v
  toAtomTypeG (M1 v) = toAtomTypeG v
  toDatabaseContextExprG _ _ = undefined  
  getConstructorsG = undefined
  getConstructorArgsG (M1 v) = getConstructorArgsG v

-- field data metadata
instance (Atomable a) => AtomableG (K1 c a) where
  toAtomG (K1 v) _ = toAtom v
  fromAtomG = K1 <$> fromAtom
  toAtomsG (K1 v) = [toAtom v]
  toAtomTypeG _ = toAtomType (undefined :: a)
  toDatabaseContextExprG _ _ = undefined    
  getConstructorsG = undefined
  getConstructorArgsG (K1 v) = [DataConstructorDefTypeConstructorArg tCons]
    where
      tCons = PrimitiveTypeConstructor primitiveATypeName primitiveAType
      primitiveAType = toAtomType v
      primitiveATypeName = case foldr (\((PrimitiveTypeConstructorDef name typ), _) _ -> if typ == primitiveAType then Just name else Nothing) Nothing primitiveTypeConstructorMapping of
        Just x -> x
        Nothing -> error ("primitive type missing: " ++ show primitiveAType)
        
instance AtomableG U1 where
  toAtomG = undefined
  fromAtomG = pure U1
  toAtomsG _ = []
  toAtomTypeG = undefined
  toDatabaseContextExprG = undefined
  getConstructorsG = undefined
  getConstructorArgsG _ = []
  
-- product types
instance (AtomableG a, AtomableG b) => AtomableG (a :*: b) where
  toAtomG = undefined
  fromAtomG = (:*:) <$> fromAtomG <*> fromAtomG
  toAtomTypeG = undefined
  toAtomsG (x :*: y) = toAtomsG x ++ toAtomsG y
  toDatabaseContextExprG _ _ = undefined    
  getConstructorsG = undefined
  getConstructorArgsG (x :*: y) = getConstructorArgsG x ++ getConstructorArgsG y

-- sum types
instance (AtomableG a, AtomableG b) => AtomableG (a :+: b) where
  toAtomG (L1 x) = toAtomG x
  toAtomG (R1 x) = toAtomG x
  fromAtomG = undefined
  toAtomTypeG = undefined
  toAtomsG (L1 x) = toAtomsG x
  toAtomsG (R1 x) = toAtomsG x
  toDatabaseContextExprG _ _ = undefined
  getConstructorsG _ = getConstructorsG (undefined :: a x) ++ getConstructorsG (undefined :: b x)
  getConstructorArgsG = undefined  
  
--this represents the unimplemented generics traversals which should never be called
{-
missingError :: a
missingError = error "missing generics traversal"
-}

