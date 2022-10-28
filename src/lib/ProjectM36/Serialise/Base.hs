{-# LANGUAGE StandaloneDeriving, DerivingVia, TypeApplications, TypeSynonymInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--Serialise instances for ProjectM36.Base data types- orphan instance city
module ProjectM36.Serialise.Base where
import Codec.Winery hiding (Schema)
import Codec.Winery.Internal
import Control.Monad
import ProjectM36.Base
import ProjectM36.MerkleHash
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import Data.UUID
import Data.Proxy
import Data.Word
import ProjectM36.Attribute as A
import qualified Data.Vector as V
import Data.Time.Calendar (Day,toGregorian,fromGregorian)
#if MIN_VERSION_winery(1,4,0)
#else
import qualified Data.List.NonEmpty as NE
#endif

deriving via WineryVariant Atom instance Serialise Atom
deriving via WineryVariant AtomType instance Serialise AtomType
deriving via WineryVariant Attribute instance Serialise Attribute
deriving via WineryVariant RelationTuple instance Serialise RelationTuple
deriving via WineryVariant RelationCardinality instance Serialise RelationCardinality
deriving via WineryVariant (RelationalExprBase a) instance Serialise a => Serialise (RelationalExprBase a)
deriving via WineryVariant (WithNameExprBase a) instance Serialise a => Serialise (WithNameExprBase a)
deriving via WineryVariant Notification instance Serialise Notification
deriving via WineryVariant TypeConstructorDef instance Serialise TypeConstructorDef
deriving via WineryVariant (TypeConstructorBase a) instance Serialise a => Serialise (TypeConstructorBase a)
deriving via WineryVariant DataConstructorDef instance Serialise DataConstructorDef
deriving via WineryVariant DataConstructorDefArg instance Serialise DataConstructorDefArg
deriving via WineryVariant GraphRefTransactionMarker instance Serialise GraphRefTransactionMarker
deriving via WineryVariant SchemaIsomorph instance Serialise SchemaIsomorph
deriving via WineryVariant InclusionDependency instance Serialise InclusionDependency
deriving via WineryVariant (DatabaseContextExprBase a) instance Serialise a => Serialise (DatabaseContextExprBase a)
deriving via WineryVariant (DatabaseContextIOExprBase a) instance Serialise a => Serialise (DatabaseContextIOExprBase a)
deriving via WineryVariant (RestrictionPredicateExprBase a) instance Serialise a => Serialise (RestrictionPredicateExprBase a)
deriving via WineryVariant TransactionInfo instance Serialise TransactionInfo
deriving via WineryVariant (AtomExprBase a) instance Serialise a => Serialise (AtomExprBase a)
deriving via WineryVariant MerkleHash instance Serialise MerkleHash
deriving via WineryVariant (AttributeExprBase a) instance Serialise a => Serialise (AttributeExprBase a)
deriving via WineryVariant (TupleExprsBase a) instance Serialise a => Serialise (TupleExprsBase a)
deriving via WineryVariant (TupleExprBase a) instance Serialise a => Serialise (TupleExprBase a)
deriving via WineryVariant (AttributeNamesBase a) instance Serialise a => Serialise (AttributeNamesBase a)
deriving via WineryVariant (ExtendTupleExprBase a) instance Serialise a => Serialise (ExtendTupleExprBase a)
deriving via WineryVariant Schema instance Serialise Schema
deriving via WineryVariant MergeStrategy instance Serialise MergeStrategy

fromWordsTup :: (Word32, Word32, Word32, Word32) -> TransactionId
fromWordsTup (a,b,c,d) = fromWords a b c d

instance Serialise TransactionId where
  schemaGen _ = getSchema (Proxy @(Word32, Word32, Word32, Word32))
  toBuilder uuid = toBuilder (toWords uuid)
  extractor = fromWordsTup <$> extractor
  decodeCurrent = fromWordsTup <$> decodeCurrent

#if MIN_VERSION_winery(1,4,0)
#else
instance Serialise a => Serialise (NE.NonEmpty a) where
  schemaGen _ = SVector <$> getSchema (Proxy @a)
  toBuilder xs = varInt (length xs) <> foldMap toBuilder xs
  extractor = NE.fromList . V.toList <$> extractListBy extractor --use nonempty instead to replace error with winery error
  decodeCurrent = do
    n <- decodeVarInt
    l <- replicateM n decodeCurrent
    pure (NE.fromList l)
#endif

fromGregorianTup :: (Integer, Int, Int) -> Day
fromGregorianTup (a, b, c) = fromGregorian a b c

instance Serialise Day where
  schemaGen _ = getSchema (Proxy @(Integer, Int, Int))
  toBuilder day = toBuilder (toGregorian day)
  extractor = fromGregorianTup <$> extractor
  decodeCurrent = fromGregorianTup <$> decodeCurrent

instance Serialise Attributes where
  schemaGen _ = SVector <$> getSchema (Proxy @Attribute)
  toBuilder attrs = varInt (V.length (attributesVec attrs)) <> foldMap toBuilder (V.toList (attributesVec attrs))
  extractor =
    attributesFromList . V.toList <$> extractListBy extractor

  decodeCurrent = do
    n <- decodeVarInt
    l <- replicateM n decodeCurrent
    pure (A.attributesFromList l)

-- | A special instance of Serialise which cuts down on duplicate attributes- we should only serialise the attributes at the top-level and not duplicate them per tuple.
instance Serialise Relation where
  schemaGen _ = getSchema (Proxy @(Attributes, [V.Vector Atom]))
  toBuilder rel = toBuilder (attributes rel, map tupleAtoms (tuplesList rel))
  extractor = makeRelation <$> extractor
   where
    makeRelation (attrs, atomList) =  Relation attrs (RelationTupleSet (map (RelationTuple attrs) atomList))
  decodeCurrent = do
    (attrs, atomList) <- decodeCurrent
    pure (Relation attrs (RelationTupleSet (map (RelationTuple attrs) atomList)))   

type SlimTupleSet = Either () (Attributes, [V.Vector Atom])

slimTupleSet :: RelationTupleSet -> SlimTupleSet
slimTupleSet tupSet =
  case asList tupSet of
    [] -> Left ()
    tup:tups -> Right (tupleAttributes tup, map tupleAtoms (tup:tups))

-- | restore slimmed tuple set to include single shared attributes list
fattenTupleSet :: SlimTupleSet -> RelationTupleSet
fattenTupleSet Left{} = emptyTupleSet
fattenTupleSet (Right (attrs, vtups)) = RelationTupleSet $ map (RelationTuple attrs) vtups

-- | A special instance of Serialise which cuts down on duplicate attributes- we should only serialise the attributes at the top-level and not duplicate them per tuple. If we have an empty tupleset, we lack all attributes which is fine in this case.
instance Serialise RelationTupleSet where
  schemaGen _ = getSchema (Proxy @SlimTupleSet)
  toBuilder tupSet = toBuilder (slimTupleSet tupSet)
  extractor = fattenTupleSet <$> extractor
  decodeCurrent = fattenTupleSet <$> decodeCurrent
