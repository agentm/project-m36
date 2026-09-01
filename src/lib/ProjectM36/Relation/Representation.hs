{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.Relation.Representation where
import ProjectM36.Base
import qualified ProjectM36.RelExprSize as RE
import ProjectM36.Relation.Representation.BTree ()

import qualified Data.STree.BTree as BTree

import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import Data.Hashable (Hashable, hashWithSalt)

-- | Relational results can be represented using multiple representations such as
-- * unsorted tupleset
-- * tuples sorted by some ordering
-- * pinned relational expression (which may have been partially evaluated and could refer to other potentially-cached expressions)
-- * b+tree with tuples
-- All representations are immutable and pegged to specific transactions.
-- These representations are used to cache evaluated relational expressions out of the transaction graph
data RelationRepresentation =
  PinnedExpressionRep PinnedRelationalExpr |
  UnsortedTupleSetRep Attributes RelationTupleSet |
  SortedTuplesRep [RelationTuple] (NE.NonEmpty (AttributeName, SortOrder)) |
  -- | A BTree can serve existence queries '(rel where attr = 3){}' and single-attribute scans such as 'rel{a} where a <= 3'
  BTreeRep PinnedRelationalExpr Attributes (BTree.BTree 16 Int) 
--  BPlusTreeRep PinnedRelationalExpr (STree.BPlusTree x y)
  -- | Tombstone PinnedRelationalExpr -- ^ denotes a cache entry that was tried before and was a failure, but this only reduces the probability that it could be reintroduced as the database ages/changes
  deriving (Eq, Generic)

instance Hashable RelationRepresentation where
  hashWithSalt s rep =
    case rep of
      PinnedExpressionRep pexpr -> s `hashWithSalt` pexpr
      UnsortedTupleSetRep attrs tupSet -> s `hashWithSalt` attrs `hashWithSalt` tupSet
      SortedTuplesRep tups sortOrder -> s `hashWithSalt` tups `hashWithSalt` sortOrder
      BTreeRep pexpr attrs _bt -> s `hashWithSalt` attrs `hashWithSalt` pexpr

instance RE.Size RelationRepresentation where
  size (PinnedExpressionRep pRelExpr) = RE.size pRelExpr
  size (UnsortedTupleSetRep _ tupSet) = RE.size tupSet
  size (SortedTuplesRep tups _) = RE.size tups
  size (BTreeRep pexpr _attrs bt) = RE.size pexpr + RE.size bt

data SortOrder = AscSortOrder | DescSortOrder
  deriving (Eq, Generic, Hashable)
  
