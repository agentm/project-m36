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

data TestT = TestC Int
            deriving (Generic, Show, Eq, Binary, NFData)
                    
data Test2T x = Test2C x
              deriving (Show, Generic, Eq, Binary, NFData)
                       
data Test3T = Test3C Int Int                        
              deriving (Show, Generic, Eq, Binary, NFData)
                       
data Test4T = Test4Ca Int |                       
              Test4Cb Int 
              deriving (Show, Generic, Eq, Binary, NFData)
                       
data Test5T = Test5Ca | Test5Cb                       
            deriving (Show, Generic, Eq, Binary, NFData)
                       
data TyTest a = TyTest a                       

instance Atomable TestT
instance Atomable a => Atomable (Test2T a)
instance Atomable Test3T
instance Atomable Test4T
instance Atomable Test5T

class (Eq a, NFData a, Binary a, Show a) => Atomable a where
  toAtom :: a -> Atom
  default toAtom :: (Generic a, AtomableG (Rep a)) => a -> Atom
  toAtom v = toAtomG (from v) (toAtomTypeG (from v))
  
  toAtomType :: a -> AtomType
  default toAtomType :: (Generic a, AtomableG (Rep a)) => a -> AtomType
  toAtomType v = toAtomTypeG (from v)
  
  --creates DatabaseContextExpr necessary to load the type constructor and data constructor into the database
  toDatabaseContextExpr :: a -> DatabaseContextExpr
  default toDatabaseContextExpr :: (Generic a, AtomableG (Rep a)) => a -> DatabaseContextExpr
  toDatabaseContextExpr v = toDatabaseContextExprG (from v) (toAtomType v)
  
instance Atomable Int where  
  toAtom i = IntAtom i
  toAtomType _ = IntAtomType
  toDatabaseContextExpr _ = NoOperation

instance Atomable T.Text where
  toAtom i = TextAtom i
  toAtomType _ = TextAtomType
  toDatabaseContextExpr _ = NoOperation
  
-- Generics
class AtomableG g where
  --type AtomTG g
  toAtomG :: g a -> AtomType -> Atom
  toAtomTypeG :: g a -> AtomType --overall ConstructedAtomType
  toAtomsG :: g a -> [Atom]
  toDatabaseContextExprG :: g a -> AtomType -> DatabaseContextExpr
  getConstructorsG :: g a -> [DataConstructorDef]
  getConstructorArgsG :: g a -> [DataConstructorDefArg]
  
--data type metadata
instance (Datatype c, AtomableG a) => AtomableG (M1 D c a) where  
  toAtomG (M1 v) t = toAtomG v t
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
  toAtomsG (M1 v) = toAtomsG v
  toAtomTypeG (M1 v) = toAtomTypeG v
  toDatabaseContextExprG _ _ = undefined  
  getConstructorsG = undefined
  getConstructorArgsG (M1 v) = getConstructorArgsG v

-- field data metadata
instance (Atomable a) => AtomableG (K1 c a) where
  toAtomG (K1 v) _ = toAtom v
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
  toAtomsG _ = []
  toAtomTypeG = undefined
  toDatabaseContextExprG = undefined
  getConstructorsG = undefined
  getConstructorArgsG _ = []
  
-- product types
instance (AtomableG a, AtomableG b) => AtomableG (a :*: b) where
  toAtomG = undefined
  toAtomTypeG = undefined
  toAtomsG (x :*: y) = toAtomsG x ++ toAtomsG y
  toDatabaseContextExprG _ _ = undefined    
  getConstructorsG = undefined
  getConstructorArgsG (x :*: y) = getConstructorArgsG x ++ getConstructorArgsG y

-- sum types
instance (AtomableG a, AtomableG b) => AtomableG (a :+: b) where
  toAtomG (L1 x) = toAtomG x
  toAtomG (R1 x) = toAtomG x
  toAtomTypeG = undefined
  toAtomsG (L1 x) = toAtomsG x
  toAtomsG (R1 x) = toAtomsG x
  toDatabaseContextExprG _ _ = undefined
  getConstructorsG _ = getConstructorsG (undefined :: a x) ++ getConstructorsG (undefined :: b x)
  getConstructorArgsG = undefined  
  
--this represents the unimplemented generics traversals which should never be called
missingError :: a
missingError = error "missing generics traversal"
  
{-
appendExpr :: DatabaseContextExpr -> DatabaseContextExpr -> DatabaseContextExpr
appendExpr expr (MultipleExpr l) = MultipleExpr l ++ [expr]
appendExpr exprA exprB = MultipleExpr [exprA, exprB]
-}

