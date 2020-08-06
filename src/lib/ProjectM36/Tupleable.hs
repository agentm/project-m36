{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module ProjectM36.Tupleable
  ( toInsertExpr
  , toDefineExpr
  , tupleAssocsEqualityPredicate
  , partitionByAttributes
  , toUpdateExpr
  , toDeleteExpr
  , validateAttributes
  , Tupleable(..)

    -- * Generics
  , genericToTuple
  , genericFromTuple
  , genericToAttributes
  , TupleableG(..)
    -- ** Options
  , defaultTupleableOptions
  , TupleableOptions()
  , fieldModifier
  ) where

import           Data.Foldable
import           Data.List                      (partition)
import qualified Data.Map                       as Map
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import           Data.Proxy
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           GHC.Generics
import           ProjectM36.Atomable
import           ProjectM36.Attribute           hiding (null)
import           ProjectM36.Base
import           ProjectM36.DataTypes.Primitive
import           ProjectM36.Error
import           ProjectM36.Tuple
import           ProjectM36.TupleSet
import qualified Data.Set as S

{-import Data.Binary
import Control.DeepSeq

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

data Test7A = Test7AC Integer
            deriving (Generic, Show, Eq, Atomable, NFData, Binary)


data Test7T = Test7C Test7A
              deriving (Generic, Show, Eq)

instance Tupleable Test7T
-}

-- | Convert a 'Traverseable' of 'Tupleable's to an 'Insert' 'DatabaseContextExpr'. This is useful for converting, for example, a list of data values to a set of Insert expressions which can be used to add the values to the database.
toInsertExpr :: forall a t. (Tupleable a, Traversable t) => t a -> RelVarName -> Either RelationalError DatabaseContextExpr
toInsertExpr vals rvName = do
  let attrs = toAttributes (Proxy :: Proxy a)
  tuples <- mkTupleSet attrs $ toList (fmap toTuple vals)
  let rel = MakeStaticRelation attrs tuples
  pure (Insert rvName rel)

-- | Convert a 'Tupleable' to a create a 'Define' expression which can be used to create an empty relation variable. Use 'toInsertExpr' to insert the actual tuple data. This function is typically used with 'Data.Proxy'.
toDefineExpr :: forall a proxy. Tupleable a => proxy a -> RelVarName -> DatabaseContextExpr
toDefineExpr _ rvName = Define rvName (map NakedAttributeExpr (V.toList attrs))
  where
    attrs = toAttributes (Proxy :: Proxy a)

tupleAssocsEqualityPredicate :: [(AttributeName, Atom)] -> RestrictionPredicateExpr
tupleAssocsEqualityPredicate [] = TruePredicate
tupleAssocsEqualityPredicate pairs =
  foldr1 AndPredicate $
  map
    (\(name, atom) -> AttributeEqualityPredicate name (NakedAtomExpr atom))
    pairs

partitionByAttributes ::
     Tupleable a
  => [AttributeName]
  -> a
  -> ([(AttributeName, Atom)], [(AttributeName, Atom)])
partitionByAttributes attrs =
  partition ((`elem` attrs) . fst) . tupleAssocs . toTuple

-- | Convert a list of key attributes and a 'Tupleable' value to an 'Update'
--   expression. This expression flushes the non-key attributes of the value to
--   a tuple with the matching key attributes.
toUpdateExpr ::
     forall a. Tupleable a => RelVarName -> [AttributeName] -> a -> Either RelationalError DatabaseContextExpr
toUpdateExpr rvName keyAttrs a = validateAttributes (S.fromList keyAttrs) expectedAttrSet (Update rvName updateMap keyRestriction)
  where
    (keyPairs, updatePairs) = partitionByAttributes keyAttrs a
    updateMap = Map.fromList $ fmap NakedAtomExpr <$> updatePairs
    keyRestriction = tupleAssocsEqualityPredicate keyPairs
    expectedAttrSet = attributeNameSet (toAttributes (Proxy :: Proxy a))

-- | Convert a list of key attributes and a 'Tupleable' value to a 'Delete'
--   expression. This expression deletes tuples matching the key attributes from
--   the value.
toDeleteExpr ::
     forall a. Tupleable a => RelVarName -> [AttributeName] -> a -> Either RelationalError DatabaseContextExpr
toDeleteExpr rvName keyAttrs val = validateAttributes (S.fromList keyAttrs) expectedAttrSet (Delete rvName keyRestriction)
  where
    keyPairs = fst $ partitionByAttributes keyAttrs val
    keyRestriction = tupleAssocsEqualityPredicate keyPairs
    expectedAttrSet = attributeNameSet (toAttributes (Proxy :: Proxy a))

validateAttributes :: S.Set AttributeName -> S.Set AttributeName -> a -> Either RelationalError a
validateAttributes actualAttrs expectedAttrs val
   | S.null actualAttrs = Left EmptyAttributesError
   | not (S.null nonMatchingAttrs) = Left (NoSuchAttributeNamesError nonMatchingAttrs)
   | otherwise = Right val
  where
      nonMatchingAttrs = attributeNamesNotContained actualAttrs expectedAttrs

-- | Types that can be converted to and from 'RelationTuple'.
--
-- deriving without customization:
--
-- > data Example = Example
-- >     { foo :: Integer
-- >     , bar :: Text
-- >     }
-- >     deriving (Generic)
-- >
-- > instance Tupleable Example
--
-- deriving with customization using "ProjectM36.Tupleable.Deriving":
--
-- > data Example = Example
-- >     { exampleFoo :: Integer
-- >     , exampleBar :: Text
-- >     }
-- >     deriving stock (Generic)
-- >     deriving (Tupleable)
-- >         via Codec (Field (DropPrefix "example" >>> CamelCase)) Example
class Tupleable a where
  toTuple :: a -> RelationTuple

  fromTuple :: RelationTuple -> Either RelationalError a

  toAttributes :: Proxy a -> Attributes

  default toTuple :: (Generic a, TupleableG (Rep a)) => a -> RelationTuple
  toTuple = genericToTuple defaultTupleableOptions

  default fromTuple :: (Generic a, TupleableG (Rep a)) => RelationTuple -> Either RelationalError a
  fromTuple = genericFromTuple defaultTupleableOptions

  default toAttributes :: (Generic a, TupleableG (Rep a)) => Proxy a -> Attributes
  toAttributes = genericToAttributes defaultTupleableOptions

-- | Options that influence deriving behavior.
newtype TupleableOptions = TupleableOptions {
  -- | A function that translates record field names into attribute names.
  fieldModifier :: T.Text -> T.Text
  }

-- | The default options for deriving Tupleable instances.
--
-- These options can be customized by using record update syntax. For example,
--
-- > defaultTupleableOptions
-- >     { fieldModifier = \fieldName ->
-- >         case Data.Text.stripPrefix "example" fieldName of
-- >             Nothing -> fieldName
-- >             Just attributeName -> attributeName
-- >     }
--
-- will result in record field names being translated into attribute names by
-- removing the prefix "example" from the field names.
defaultTupleableOptions :: TupleableOptions
defaultTupleableOptions = TupleableOptions {
  fieldModifier = id
  }

genericToTuple :: (Generic a, TupleableG (Rep a)) => TupleableOptions -> a -> RelationTuple
genericToTuple opts v = toTupleG opts (from v)

genericFromTuple :: (Generic a, TupleableG (Rep a)) => TupleableOptions -> RelationTuple -> Either RelationalError a
genericFromTuple opts tup = to <$> fromTupleG opts tup

genericToAttributes :: forall a. (Generic a, TupleableG (Rep a)) => TupleableOptions -> Proxy a -> Attributes
genericToAttributes opts _ = toAttributesG opts (from (undefined :: a))

class TupleableG g where
  toTupleG :: TupleableOptions -> g a -> RelationTuple
  toAttributesG :: TupleableOptions -> g a -> Attributes
  fromTupleG :: TupleableOptions -> RelationTuple -> Either RelationalError (g a)
  isRecordTypeG :: g a -> Bool

--data type metadata
instance (Datatype c, TupleableG a) => TupleableG (M1 D c a) where
  toTupleG opts (M1 v) = toTupleG opts v
  toAttributesG opts (M1 v) = toAttributesG opts v
  fromTupleG opts v = M1 <$> fromTupleG opts v
  isRecordTypeG (M1 v) = isRecordTypeG v

--constructor metadata
instance (Constructor c, TupleableG a, AtomableG a) => TupleableG (M1 C c a) where
  toTupleG opts (M1 v) = RelationTuple attrs atoms
    where
      attrsToCheck = toAttributesG opts v
      counter = V.generate (V.length attrsToCheck) id
      attrs = V.zipWith (\num attr@(Attribute name typ) -> if T.null name then
                                                             Attribute ("attr" <> T.pack (show (num + 1))) typ
                                                           else
                                                             attr) counter attrsToCheck
      atoms = V.fromList (toAtomsG v)
  toAttributesG opts (M1 v) = toAttributesG opts v
  fromTupleG opts tup = M1 <$> fromTupleG opts tup
  isRecordTypeG (M1 v) = isRecordTypeG v

-- product types
instance (TupleableG a, TupleableG b) => TupleableG (a :*: b) where
  toTupleG = error "toTupleG"
  toAttributesG opts ~(x :*: y) = toAttributesG opts x V.++ toAttributesG opts y --a bit of extra laziness prevents whnf so that we can use toAttributes (undefined :: Test2T Int Int) without throwing an exception
  fromTupleG opts tup = (:*:) <$> fromTupleG opts tup <*> fromTupleG opts processedTuple
    where
      processedTuple = if isRecordTypeG (undefined :: a x) then
                         tup
                       else
                         trimTuple 1 tup
  isRecordTypeG ~(x :*: y) = isRecordTypeG x || isRecordTypeG y

--selector/record
instance (Selector c, AtomableG a) => TupleableG (M1 S c a) where
  toTupleG = error "toTupleG"
  toAttributesG opts m@(M1 v) = V.singleton (Attribute modifiedName aType)
   where
     name = T.pack (selName m)
     modifiedName = if T.null name then
                      name
                    else
                      fieldModifier opts name
     aType = toAtomTypeG v
  fromTupleG opts tup = if null name then -- non-record type, just pull off the first tuple item
                     M1 <$> atomv (V.head (tupleAtoms tup))
                   else do
                     atom <- atomForAttributeName (fieldModifier opts (T.pack name)) tup
                     val <- atomv atom
                     pure (M1 val)
   where
     expectedAtomType = atomType (V.head (toAttributesG opts (undefined :: M1 S c a x)))
     atomv atom = maybe (Left (AtomTypeMismatchError
                               expectedAtomType
                               (atomTypeForAtom atom)
                              )) Right (fromAtomG atom [atom])
     name = selName (undefined :: M1 S c a x)
  isRecordTypeG _ = not (null (selName (undefined :: M1 S c a x)))

--constructors with no arguments
--basically useless but orthoganal to relationTrue
instance TupleableG U1 where
  toTupleG _ _ = emptyTuple
  toAttributesG _ _ = emptyAttributes
  fromTupleG _ _ = pure U1
  isRecordTypeG _ = False

