{-# LANGUAGE DefaultSignatures, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}
module ProjectM36.Atomable where
--http://stackoverflow.com/questions/13448361/type-families-with-ghc-generics-or-data-data
--instances to marshal Haskell ADTs to ConstructedAtoms and back
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.DataTypes.Primitive
import ProjectM36.DataTypes.List
import ProjectM36.DataTypes.Maybe
import ProjectM36.DataTypes.Either
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Text as T
import Control.DeepSeq (NFData)
import Data.Binary
import Control.Applicative
import Data.Time.Calendar
import Data.ByteString (ByteString)
import Data.Time.Clock
import Data.Maybe

--also add haskell scripting atomable support
--rename this module to Atomable along with test

{-
data Test1T = Test1C Int
            deriving (Generic, Show, Eq, Binary, NFData, Atomable)
                     
data Test2T = Test2C Int Int                     
            deriving (Generic, Show, Eq, Binary, NFData, Atomable)
                     
data Test3T = Test3Ca | Test3Cb                     
            deriving (Generic, Show, Eq, Binary, NFData, Atomable)
-}
-- | All database values ("atoms") adhere to the 'Atomable' typeclass. This class is derivable allowing new datatypes to be easily marshaling between Haskell values and database values.
class (Eq a, NFData a, Binary a, Show a) => Atomable a where
  toAtom :: a -> Atom
  default toAtom :: (Generic a, AtomableG (Rep a)) => a -> Atom
  toAtom v = toAtomG (from v) (toAtomTypeG (from v))
  
  fromAtom :: Atom -> a
  default fromAtom :: (Generic a, AtomableG (Rep a)) => Atom -> a
  fromAtom v@(ConstructedAtom _ _ args) = case fromAtomG v args of
    Nothing -> error "no fromAtomG traversal found"
    Just x -> to x
  fromAtom v = case fromAtomG v [] of
    Nothing -> error "no fromAtomG for Atom found"
    Just x -> to x
    
  toAtomType :: a -> AtomType
  default toAtomType :: (Generic a, AtomableG (Rep a)) => a -> AtomType
  toAtomType v = toAtomTypeG (from v)
                      
  -- | Creates DatabaseContextExpr necessary to load the type constructor and data constructor into the database.
  toAddTypeExpr :: a -> DatabaseContextExpr
  default toAddTypeExpr :: (Generic a, AtomableG (Rep a)) => a -> DatabaseContextExpr
  toAddTypeExpr v = toAddTypeExprG (from v) (toAtomType v)
  
instance Atomable Int where  
  toAtom = IntAtom
  fromAtom (IntAtom i) = i
  fromAtom e = error ("improper fromAtom" ++ show e)
  toAtomType _ = IntAtomType
  toAddTypeExpr _ = NoOperation

instance Atomable Double where
  toAtom = DoubleAtom
  fromAtom (DoubleAtom d) = d
  fromAtom _ = error "improper fromAtom"
  toAtomType _ = DoubleAtomType
  toAddTypeExpr _ = NoOperation

instance Atomable T.Text where
  toAtom = TextAtom
  fromAtom (TextAtom t) = t
  fromAtom _ = error "improper fromAtom"  
  toAtomType _ = TextAtomType
  toAddTypeExpr _ = NoOperation

instance Atomable Day where
  toAtom = DayAtom
  fromAtom (DayAtom d) = d
  fromAtom _ = error "improper fromAtom"
  toAtomType _ = DayAtomType
  toAddTypeExpr _ = NoOperation

instance Atomable UTCTime where
  toAtom = DateTimeAtom
  fromAtom (DateTimeAtom t) = t
  fromAtom _ = error "improper fromAtom"
  toAtomType _ = DateTimeAtomType
  toAddTypeExpr _ = NoOperation

instance Atomable ByteString where
  toAtom = ByteStringAtom
  fromAtom (ByteStringAtom b) = b
  fromAtom _ = error "improper fromAtom"
  toAtomType _ = ByteStringAtomType
  toAddTypeExpr _ = NoOperation

instance Atomable Bool where
  toAtom = BoolAtom
  fromAtom (BoolAtom b) = b
  fromAtom _ = error "improper fromAtom"
  toAtomType _ = BoolAtomType
  toAddTypeExpr _ = NoOperation
  
instance Atomable Relation where
  toAtom = RelationAtom
  fromAtom (RelationAtom r) = r
  fromAtom _ = error "improper fromAtom"
  --warning: cannot be used with undefined "Relation"
  toAtomType rel = RelationAtomType (attributes rel) 
  toAddTypeExpr _ = NoOperation
  
instance Atomable a => Atomable (Maybe a) where
  toAtom (Just v) = ConstructedAtom "Just" (maybeAtomType (toAtomType v)) [toAtom v]
  toAtom Nothing = ConstructedAtom "Nothing" (maybeAtomType (toAtomType (undefined :: a))) []
  
  fromAtom (ConstructedAtom "Just" _ [val]) = Just (fromAtom val)
  fromAtom (ConstructedAtom "Nothing" _ []) = Nothing
  fromAtom _ = error "improper fromAtom (Maybe a)"
  
  toAtomType _ = ConstructedAtomType "Maybe" (M.singleton "a" (toAtomType (undefined :: a)))
  toAddTypeExpr _ = NoOperation
  
instance (Atomable a, Atomable b) => Atomable (Either a b) where
  toAtom (Left l) = ConstructedAtom "Left" (eitherAtomType (toAtomType (undefined :: a)) 
                                            (toAtomType (undefined :: b))) [toAtom l]
  toAtom (Right r) = ConstructedAtom "Right" (eitherAtomType (toAtomType (undefined :: a)) 
                                              (toAtomType (undefined :: b))) [toAtom r]
  
  fromAtom (ConstructedAtom "Left" _ [val]) = Left (fromAtom val)
  fromAtom (ConstructedAtom "Right" _ [val]) = Right (fromAtom val)
  fromAtom _ = error "improper fromAtom (Either a b)"
  
--convert to ADT list  
instance Atomable a => Atomable [a] where
  toAtom [] = ConstructedAtom "Empty" (listAtomType (toAtomType (undefined :: a))) []
  toAtom (x:xs) = ConstructedAtom "Cons" (listAtomType (toAtomType x)) (map toAtom (x:xs))
  
  fromAtom (ConstructedAtom "Empty" _ _) = []
  fromAtom (ConstructedAtom "Cons" _ (x:xs)) = fromAtom x:map fromAtom xs
  fromAtom _ = error "improper fromAtom [a]"
  
  toAtomType _ = ConstructedAtomType "List" (M.singleton "a" (toAtomType (undefined :: a)))
  toAddTypeExpr _ = NoOperation

-- Generics
class AtomableG g where
  --type AtomTG g
  toAtomG :: g a -> AtomType -> Atom
  fromAtomG :: Atom -> [Atom] -> Maybe (g a)
  toAtomTypeG :: g a -> AtomType --overall ConstructedAtomType
  toAtomsG :: g a -> [Atom]
  toAddTypeExprG :: g a -> AtomType -> DatabaseContextExpr
  getConstructorsG :: g a -> [DataConstructorDef]
  getConstructorArgsG :: g a -> [DataConstructorDefArg]
  
--data type metadata
instance (Datatype c, AtomableG a) => AtomableG (M1 D c a) where  
  toAtomG (M1 v) = toAtomG v
  fromAtomG atom args = M1 <$> fromAtomG atom args
  toAtomsG = undefined
  toAtomTypeG _ = ConstructedAtomType (T.pack typeName) M.empty -- generics don't allow us to get the type constructor variables- alternatives?
    where
      typeName = datatypeName (undefined :: M1 D c a x)
  toAddTypeExprG (M1 v) (ConstructedAtomType tcName _) = AddTypeConstructor tcDef dataConstructors
    where
      tcDef = ADTypeConstructorDef tcName []
      dataConstructors = getConstructorsG v
  toAddTypeExprG _ _ = NoOperation      
  getConstructorsG (M1 v) = getConstructorsG v
  getConstructorArgsG = undefined
  
--constructor metadata
instance (Constructor c, AtomableG a) => AtomableG (M1 C c a) where
  --constructor name needed for Atom but not for atomType
  toAtomG (M1 v) t = ConstructedAtom (T.pack constructorName) t atoms
    where
      atoms = toAtomsG v
      constructorName = conName (undefined :: M1 C c a x)
  fromAtomG atom@(ConstructedAtom dConsName _ _) args = if dName == dConsName then
                                                      M1 <$> fromAtomG atom args
                                                   else
                                                     Nothing
    where
      dName = T.pack (conName (undefined :: M1 C c a x))
  fromAtomG _ _ = error "unsupported generic traversal"
  toAtomsG = undefined
  toAtomTypeG = undefined
  toAddTypeExprG = undefined  
  getConstructorsG (M1 v) = [DataConstructorDef (T.pack dName) dArgs]
    where
      dName = conName (undefined :: M1 C c a x)
      dArgs = getConstructorArgsG v
  getConstructorArgsG = undefined

--field metadata
instance (Selector c, AtomableG a) => AtomableG (M1 S c a) where
  toAtomG = undefined
  fromAtomG atom args = M1 <$> fromAtomG atom args
  toAtomsG (M1 v) = toAtomsG v
  toAtomTypeG (M1 v) = toAtomTypeG v
  toAddTypeExprG _ _ = undefined  
  getConstructorsG = undefined
  getConstructorArgsG (M1 v) = getConstructorArgsG v

-- field data metadata
instance (Atomable a) => AtomableG (K1 c a) where
  toAtomG (K1 v) _ = toAtom v
  fromAtomG _ args = K1 <$> Just (fromAtom (headatom args))
                     where headatom (x:_) = x
                           headatom [] = error "no more atoms for constructor!"
  toAtomsG (K1 v) = [toAtom v]
  toAtomTypeG _ = toAtomType (undefined :: a)
  toAddTypeExprG _ _ = undefined    
  getConstructorsG = undefined
  getConstructorArgsG (K1 v) = [DataConstructorDefTypeConstructorArg tCons]
    where
      tCons = PrimitiveTypeConstructor primitiveATypeName primitiveAType
      primitiveAType = toAtomType v
      primitiveATypeName = fromMaybe (error ("primitive type missing: " ++ show primitiveAType)) (foldr (\(PrimitiveTypeConstructorDef name typ, _) _ -> if typ == primitiveAType then Just name else Nothing) Nothing primitiveTypeConstructorMapping)
        
instance AtomableG U1 where
  toAtomG = undefined
  fromAtomG _ _ = pure U1
  toAtomsG _ = []
  toAtomTypeG = undefined
  toAddTypeExprG = undefined
  getConstructorsG = undefined
  getConstructorArgsG _ = []
  
-- product types
instance (AtomableG a, AtomableG b) => AtomableG (a :*: b) where
  toAtomG = undefined
  fromAtomG atom args = (:*:) <$> fromAtomG atom [headatom args] <*> fromAtomG atom (tailatoms args)
    where headatom (x:_) = x
          headatom [] = error "no more atoms in head for product!"
          tailatoms (_:xs) = xs
          tailatoms [] = error "no more atoms in tail for product!"
  toAtomTypeG = undefined
  toAtomsG (x :*: y) = toAtomsG x ++ toAtomsG y
  toAddTypeExprG _ _ = undefined    
  getConstructorsG = undefined
  getConstructorArgsG (x :*: y) = getConstructorArgsG x ++ getConstructorArgsG y

-- sum types
instance (AtomableG a, AtomableG b) => AtomableG (a :+: b) where
  toAtomG (L1 x) = toAtomG x
  toAtomG (R1 x) = toAtomG x
  fromAtomG atom args = (L1 <$> fromAtomG atom args) <|> (R1 <$> fromAtomG atom args)
  toAtomTypeG = undefined
  toAtomsG (L1 x) = toAtomsG x
  toAtomsG (R1 x) = toAtomsG x
  toAddTypeExprG _ _ = undefined
  getConstructorsG _ = getConstructorsG (undefined :: a x) ++ getConstructorsG (undefined :: b x)
  getConstructorArgsG = undefined  
  
--this represents the unimplemented generics traversals which should never be called
{-
missingError :: a
missingError = error "missing generics traversal"
-}

