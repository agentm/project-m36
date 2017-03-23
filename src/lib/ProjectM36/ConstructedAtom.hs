{-# LANGUAGE DefaultSignatures, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, DeriveGeneric #-}
module ProjectM36.ConstructedAtom where
--http://stackoverflow.com/questions/13448361/type-families-with-ghc-generics-or-data-data
--instances to marshal Haskell ADTs to ConstructedAtoms and back
import ProjectM36.Base
import ProjectM36.DataTypes.Primitive
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as M

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
instance Atomable Test4T

class (Eq a, Show a) => Atomable a where
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
  toAtomsG = missingError
  toAtomTypeG _ = ConstructedAtomType (T.pack typeName) M.empty -- generics don't allow us to get the type constructor variables- alternatives?
    where
      typeName = datatypeName (missingError :: M1 D c a x)
  toDatabaseContextExprG (M1 v) (ConstructedAtomType tcName _) = AddTypeConstructor tcDef dataConstructors
    where
      tcDef = ADTypeConstructorDef tcName []
      dataConstructors = getConstructorsG v
  toDatabaseContextExprG _ _ = NoOperation      
  getConstructorsG = missingError
  getConstructorArgsG = missingError
  
--constructor metadata
instance (Constructor c, AtomableG a) => AtomableG (M1 C c a) where
  --constructor name needed for Atom but not for atomType
  toAtomG (M1 v) t = ConstructedAtom (T.pack constructorName) t atoms
    where
      atoms = toAtomsG v
      constructorName = conName (missingError :: M1 C c a x)
  toAtomsG = missingError
  toAtomTypeG = missingError
  toDatabaseContextExprG = missingError  
  getConstructorsG (M1 v) = [DataConstructorDef (T.pack dName) dArgs]
    where
      dName = conName (missingError :: M1 C c a x)
      dArgs = getConstructorArgsG v
  getConstructorArgsG = missingError

--field metadata
instance (Selector c, AtomableG a) => AtomableG (M1 S c a) where
  toAtomG = missingError
  toAtomsG (M1 v) = toAtomsG v
  toAtomTypeG (M1 v) = toAtomTypeG v
  toDatabaseContextExprG _ _ = missingError  
  getConstructorsG = error "hm"
  getConstructorArgsG (M1 v) = getConstructorArgsG v

-- field data metadata
instance (Atomable a) => AtomableG (K1 c a) where
  toAtomG (K1 v) _ = toAtom v
  toAtomsG (K1 v) = [toAtom v]
  toAtomTypeG _ = toAtomType (missingError :: a)
  toDatabaseContextExprG _ _ = missingError    
  getConstructorsG = missingError
  getConstructorArgsG (K1 v) = [DataConstructorDefTypeConstructorArg tCons]
    where
      tCons = PrimitiveTypeConstructor primitiveATypeName primitiveAType
      primitiveAType = toAtomType v
      primitiveATypeName = case foldr (\((PrimitiveTypeConstructorDef name typ), _) _ -> if typ == primitiveAType then Just name else Nothing) Nothing primitiveTypeConstructorMapping of
        Just x -> x
        Nothing -> error ("primitive type missing: " ++ show primitiveAType)
  
-- product types
instance (AtomableG a, AtomableG b) => AtomableG (a :*: b) where
  toAtomG = missingError
  toAtomTypeG = missingError
  toAtomsG (x :*: y) = toAtomsG x ++ toAtomsG y
  toDatabaseContextExprG _ _ = missingError    
  getConstructorsG = missingError
  getConstructorArgsG (x :*: y) = getConstructorArgsG x ++ getConstructorArgsG y

-- sum types
instance (AtomableG a, AtomableG b) => AtomableG (a :+: b) where
  toAtomG (L1 x) = toAtomG x
  toAtomG (R1 x) = toAtomG x
  toAtomTypeG = missingError
  toAtomsG (L1 x) = toAtomsG x
  toAtomsG (R1 x) = toAtomsG x
  toDatabaseContextExprG _ _ = missingError  
  getConstructorsG (L1 x) = getConstructorsG x
  getConstructorsG (R1 x) = getConstructorsG x
  getConstructorArgsG = missingError  
  
--this represents the unimplemented generics traversals which should never be called
missingError :: a
missingError = error "missing generics traversal"
  
{-
appendExpr :: DatabaseContextExpr -> DatabaseContextExpr -> DatabaseContextExpr
appendExpr expr (MultipleExpr l) = MultipleExpr l ++ [expr]
appendExpr exprA exprB = MultipleExpr [exprA, exprB]
-}

