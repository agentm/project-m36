{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeOperators, UndecidableInstances, ScopedTypeVariables, DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module ProjectM36.Tupleable where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Atomable
import ProjectM36.DataTypes.Primitive
import ProjectM36.Attribute hiding (null)
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Monoid
import Data.Foldable

{-
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

data TestUnnamed1 = TestUnnamed1 Int Double T.Text
                    deriving (Show,Eq, Generic)
                             
instance Tupleable TestUnnamed1          
-}

-- | Convert a 'Traverseable' of 'Tupleable's to an 'Insert' 'DatabaseContextExpr'. This is useful for converting, for example, a list of data values to a set of Insert expressions which can be used to add the values to the database.
toInsertExpr :: (Tupleable a, Traversable t) => t a -> RelVarName -> Either RelationalError DatabaseContextExpr
toInsertExpr vals rvName = do
  let attrs = toAttributes (head (toList vals))
  tuples <- mkTupleSet attrs $ toList (fmap toTuple vals)
  let rel = MakeStaticRelation attrs tuples   
  pure (Insert rvName rel)

class Tupleable a where
  toTuple :: a -> RelationTuple
  
  fromTuple :: RelationTuple -> Either RelationalError a
  
  toAttributes :: a -> Attributes

  default toTuple :: (Generic a, TupleableG (Rep a)) => a -> RelationTuple
  toTuple v = toTupleG (from v)
  
  default fromTuple :: (Generic a, TupleableG (Rep a)) => RelationTuple -> Either RelationalError a
  fromTuple tup = to <$> fromTupleG tup
    
  default toAttributes :: (Generic a, TupleableG (Rep a)) => a -> Attributes
  toAttributes v = toAttributesG (from v)
  
class TupleableG g where  
  toTupleG :: g a -> RelationTuple
  toAttributesG :: g a -> Attributes
  fromTupleG :: RelationTuple -> Either RelationalError (g a)
  
--data type metadata
instance (Datatype c, TupleableG a) => TupleableG (M1 D c a) where
  toTupleG (M1 v) = toTupleG v
  toAttributesG (M1 v) = toAttributesG v
  fromTupleG v = M1 <$> fromTupleG v

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
  toAttributesG (M1 v) = toAttributesG v
  fromTupleG tup = M1 <$> fromTupleG tup
  
-- product types
instance (TupleableG a, TupleableG b) => TupleableG (a :*: b) where
  toTupleG = error "toTupleG"
  toAttributesG ~(x :*: y) = toAttributesG x V.++ toAttributesG y --a bit of extra laziness prevents whnf so that we can use toAttributes (undefined :: Test2T Int Int) without throwing an exception
  fromTupleG tup = (:*:) <$> fromTupleG tup <*> fromTupleG (trimTuple 1 tup)

--selector/record
instance (Selector c, AtomableG a) => TupleableG (M1 S c a) where
  toTupleG = error "toTupleG"
  toAttributesG m@(M1 v) = V.singleton (Attribute name aType)
   where
     name = T.pack (selName m)
     aType = toAtomTypeG v
  fromTupleG tup = if null name then -- non-record type, just pull off the first tuple item
                     M1 <$> atomv (V.head (tupleAtoms tup))
                   else do
                     atom <- atomForAttributeName (T.pack name) tup
                     val <- atomv atom
                     pure (M1 val)
   where
     expectedAtomType = atomType (V.head (toAttributesG (undefined :: M1 S c a x)))
     atomv atom = maybe (Left (AtomTypeMismatchError  
                               expectedAtomType
                               (atomTypeForAtom atom)
                              )) Right (fromAtomG atom [atom])
     name = selName (undefined :: M1 S c a x)

--constructors with no arguments  
--basically useless but orthoganal to relationTrue
instance TupleableG U1 where
  toTupleG _= emptyTuple
  toAttributesG _ = emptyAttributes
  fromTupleG _ = pure U1
  
  