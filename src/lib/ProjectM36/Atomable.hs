{-# LANGUAGE DefaultSignatures, TypeFamilies, TypeOperators, PolyKinds, FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Atomable where
--http://stackoverflow.com/questions/13448361/type-families-with-ghc-generics-or-data-data
--instances to marshal Haskell ADTs to ConstructedAtoms and back
import ProjectM36.Base
import ProjectM36.DataTypes.List
import ProjectM36.DataTypes.NonEmptyList
import ProjectM36.DataTypes.Maybe
import ProjectM36.DataTypes.Either
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.DeepSeq (NFData)
import Data.Binary
import Control.Applicative
import Data.Time.Calendar
import Data.ByteString (ByteString)
import Data.Time.Clock
import Data.Proxy
import qualified Data.List.NonEmpty as NE

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
    
  toAtomType :: proxy a -> AtomType
  default toAtomType :: (Generic a, AtomableG (Rep a)) => proxy a -> AtomType
  toAtomType _ = toAtomTypeG (from (undefined :: a))
                      
  -- | Creates DatabaseContextExpr necessary to load the type constructor and data constructor into the database.
  toAddTypeExpr :: proxy a -> DatabaseContextExpr
  default toAddTypeExpr :: (Generic a, AtomableG (Rep a)) => proxy a -> DatabaseContextExpr
  toAddTypeExpr _ = toAddTypeExprG (from (error "insufficient laziness" :: a)) (toAtomType (Proxy :: Proxy a))
  
instance Atomable Integer where  
  toAtom = IntegerAtom
  fromAtom (IntegerAtom i) = i
  fromAtom e = error ("improper fromAtom" ++ show e)
  toAtomType _ = IntegerAtomType
  toAddTypeExpr _ = NoOperation
  
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
  
{-
instance Atomable Relation where
  toAtom = RelationAtom
  fromAtom (RelationAtom r) = r
  fromAtom _ = error "improper fromAtom"
  --warning: cannot be used with undefined "Relation"
  toAtomType rel = RelationAtomType (attributes rel) 
  toAddTypeExpr _ = NoOperation
-}
  
instance Atomable a => Atomable (Maybe a) where
  toAtom (Just v) = ConstructedAtom "Just" (maybeAtomType (toAtomType (Proxy :: Proxy a))) [toAtom v]
  toAtom Nothing = ConstructedAtom "Nothing" (maybeAtomType (toAtomType (Proxy :: Proxy a))) []
  
  fromAtom (ConstructedAtom "Just" _ [val]) = Just (fromAtom val)
  fromAtom (ConstructedAtom "Nothing" _ []) = Nothing
  fromAtom _ = error "improper fromAtom (Maybe a)"
  
  toAtomType _ = ConstructedAtomType "Maybe" (M.singleton "a" (toAtomType (Proxy :: Proxy a)))
  toAddTypeExpr _ = NoOperation
  
instance (Atomable a, Atomable b) => Atomable (Either a b) where
  toAtom (Left l) = ConstructedAtom "Left" (eitherAtomType (toAtomType (Proxy :: Proxy a)) 
                                            (toAtomType (Proxy :: Proxy b))) [toAtom l]
  toAtom (Right r) = ConstructedAtom "Right" (eitherAtomType (toAtomType (Proxy :: Proxy a)) 
                                              (toAtomType (Proxy :: Proxy b))) [toAtom r]
  
  fromAtom (ConstructedAtom "Left" _ [val]) = Left (fromAtom val)
  fromAtom (ConstructedAtom "Right" _ [val]) = Right (fromAtom val)
  fromAtom _ = error "improper fromAtom (Either a b)"
  
--convert to ADT list  
instance Atomable a => Atomable [a] where
  toAtom [] = ConstructedAtom "Empty" (listAtomType (toAtomType (Proxy :: Proxy a))) []
  toAtom (x:xs) = ConstructedAtom "Cons" (listAtomType (toAtomType (Proxy :: Proxy a))) (toAtom x: [toAtom xs])
  
  fromAtom (ConstructedAtom "Empty" _ _) = []
  fromAtom (ConstructedAtom "Cons" _ [x,y]) = fromAtom x:fromAtom y
  fromAtom _ = error "improper fromAtom [a]"
  
  toAtomType _ = ConstructedAtomType "List" (M.singleton "a" (toAtomType (Proxy :: Proxy a)))
  toAddTypeExpr _ = NoOperation

instance Atomable a => Atomable (NE.NonEmpty a) where
  toAtom (x NE.:| []) = ConstructedAtom "NECons" (nonEmptyListAtomType (toAtomType (Proxy :: Proxy a))) [toAtom x]
  toAtom (x NE.:| xs) = ConstructedAtom "NECons" (nonEmptyListAtomType (toAtomType (Proxy :: Proxy a))) (map toAtom (x:xs))
  fromAtom _ = error "improper fromAtom (NonEmptyList a)"

  toAtomType _ = ConstructedAtomType "NonEmptyList" (M.singleton "a" (toAtomType (Proxy :: Proxy a)))
  toAddTypeExpr _ = NoOperation

#if !MIN_VERSION_binary(0,8,4)
instance Binary a => Binary (NE.NonEmpty a)  
#endif

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
  toAtomsG = error "invalid toAtomsG in M1 D"
  toAtomTypeG _ = ConstructedAtomType (T.pack typeName) M.empty -- generics don't allow us to get the type constructor variables- alternatives?
    where
      typeName = datatypeName (undefined :: M1 D c a x)
  toAddTypeExprG ~(M1 v) (ConstructedAtomType tcName _) = AddTypeConstructor tcDef dataConstructors
    where
      tcDef = ADTypeConstructorDef tcName []
      dataConstructors = getConstructorsG v
  toAddTypeExprG _ _ = NoOperation      
  getConstructorsG ~(M1 v) = getConstructorsG v
  getConstructorArgsG = error "invalid getConstructorArgsG in M1 D"
  
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
  fromAtomG _ _ = error "invalid fromAtomG in M1 C"
  toAtomsG = error "invalid toAtomsG in M1 C"
  toAtomTypeG = error "invalid toAtomTypeG in M1 C"
  toAddTypeExprG = error "invalid toAddTypeExprG in M1 C"
  getConstructorsG ~(M1 v) = [DataConstructorDef (T.pack dName) dArgs]
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
  toAddTypeExprG = error "invalid toAddTypeExprG in M1 S"  
  getConstructorsG = error "invalid getConstructorsG in M1 S"
  getConstructorArgsG ~(M1 v) = getConstructorArgsG v

-- field data metadata
instance (Atomable a) => AtomableG (K1 c a) where
  toAtomG (K1 v) _ = toAtom v
  fromAtomG _ args = K1 <$> Just (fromAtom (headatom args))
                     where headatom (x:_) = x
                           headatom [] = error "no more atoms for constructor!"
  toAtomsG (K1 v) = [toAtom v]
  toAtomTypeG _ = toAtomType (Proxy :: Proxy a)
  toAddTypeExprG = error "invalid toAddTypeExprG in K1"    
  getConstructorsG = error "invalid getConstructorsG in K1"
  getConstructorArgsG ~(K1 _) = [DataConstructorDefTypeConstructorArg tCons]
    where
      tCons = typeToTypeConstructor $ toAtomType (Proxy :: Proxy a)

typeToTypeConstructor :: AtomType -> TypeConstructor
typeToTypeConstructor x@IntAtomType = PrimitiveTypeConstructor "Int" x
typeToTypeConstructor x@IntegerAtomType = PrimitiveTypeConstructor "Integer" x
typeToTypeConstructor x@DoubleAtomType = PrimitiveTypeConstructor "Double" x
typeToTypeConstructor x@TextAtomType = PrimitiveTypeConstructor "Text" x
typeToTypeConstructor x@DayAtomType = PrimitiveTypeConstructor "Day" x
typeToTypeConstructor x@DateTimeAtomType = PrimitiveTypeConstructor "DateTime" x
typeToTypeConstructor x@ByteStringAtomType = PrimitiveTypeConstructor "ByteString" x
typeToTypeConstructor x@BoolAtomType = PrimitiveTypeConstructor "Bool" x
typeToTypeConstructor x@RelationalExprAtomType = PrimitiveTypeConstructor "RelationalExpr" x
typeToTypeConstructor (RelationAtomType attrs)
  = RelationAtomTypeConstructor $ map attrToAttrExpr $ V.toList attrs
  where
    attrToAttrExpr (Attribute n t) = AttributeAndTypeNameExpr n (typeToTypeConstructor t) ()
typeToTypeConstructor (ConstructedAtomType tcName tvMap)
  = ADTypeConstructor tcName $ map typeToTypeConstructor (M.elems tvMap)
typeToTypeConstructor (TypeVariableType tvName) = TypeVariable tvName

instance AtomableG U1 where
  toAtomG = error "invalid toAtomG in U1"
  fromAtomG _ _ = pure U1
  toAtomsG _ = []
  toAtomTypeG = error "invalid toAtomTypeG in U1"
  toAddTypeExprG = error "invalid toAddTypeExprG in U1"
  getConstructorsG = error "invalid getConstructorsG in U1"
  getConstructorArgsG _ = []
  
-- product types
instance (AtomableG a, AtomableG b) => AtomableG (a :*: b) where
  toAtomG = undefined
  fromAtomG atom args = (:*:) <$> fromAtomG atom splitargs1 <*> fromAtomG atom splitargs2
    where splitargs1 = take splitpoint args 
          splitargs2 = drop splitpoint args
          splitpoint = length args `div` 2
  toAtomTypeG = error "invalid toAtomTypeG in :*:"
  toAtomsG (x :*: y) = toAtomsG x ++ toAtomsG y
  toAddTypeExprG = error "invalid toAddTypeExprG in :*:"
  getConstructorsG = error "invalid getConstructorsG in :*:"
  getConstructorArgsG ~(x :*: y) = getConstructorArgsG x ++ getConstructorArgsG y

-- sum types
instance (AtomableG a, AtomableG b) => AtomableG (a :+: b) where
  toAtomG (L1 x) = toAtomG x
  toAtomG (R1 x) = toAtomG x
  fromAtomG atom args = (L1 <$> fromAtomG atom args) <|> (R1 <$> fromAtomG atom args)
  toAtomTypeG = error "invalid toAtomTypeG in :+:"
  toAtomsG (L1 x) = toAtomsG x
  toAtomsG (R1 x) = toAtomsG x
  toAddTypeExprG = error "invalid toAddTypeExprG in :+:"
  getConstructorsG _ = getConstructorsG (undefined :: a x) ++ getConstructorsG (undefined :: b x)
  getConstructorArgsG = error "invalid getConstructorArgsG in :+:" 
  
--this represents the unimplemented generics traversals which should never be called
{-
missingError :: a
missingError = error "missing generics traversal"
-}

