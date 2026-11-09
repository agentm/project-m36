{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances #-}
module ProjectM36.AttributeNamesExpr where
import ProjectM36.Base
import qualified Data.Set as S
#if MIN_VERSION_base(4,20,0)
#else
import Data.Foldable (foldl')
#endif
import qualified Data.Map as M

--AttributeNames is a data structure which can represent inverted projection attributes and attribute names derived from relational expressions

empty :: AttributeNamesExprBase a
empty = AttributeNames S.empty

all :: AttributeNamesExprBase a
all = InvertedAttributeNames S.empty

-- | Coalesce a bunch of AttributeNamesExprs into a single AttributeNamesExpr.
some :: Eq a => [AttributeNamesExprBase a] -> AttributeNamesExprBase a
some [] = ProjectM36.AttributeNamesExpr.all
some [an] = an
some (a:as) = foldl' folder a as
  where
    folder :: Eq a => AttributeNamesExprBase a -> AttributeNamesExprBase a -> AttributeNamesExprBase a
    folder acc names =
      case acc of
        AttributeNames an | S.null an -> names
        acc' -> if names == empty then
                  acc
                else
                  UnionAttributeNames acc' names

-- convert () -> GraphRefTransactionMarker
type family NormalizeMarker a where
  NormalizeMarker (AttributeNamesExprBase ()) =
    AttributeNamesExprBase GraphRefTransactionMarker

  NormalizeMarker (RelationalExprBase (AttributeNamesExprBase ()) ()) =
    RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeMarker (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) =
    RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeMarker (TupleExprsBase (AttributeNamesExprBase ()) ()) =
    TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeMarker (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) =
    ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeMarker (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) =
    WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeMarker (TupleExprBase (AttributeNamesExprBase ()) ()) =
    TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeMarker (AtomExprBase (AttributeNamesExprBase ()) ()) =
    AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeMarker (WithNameExprBase ()) =
    WithNameExprBase GraphRefTransactionMarker

  NormalizeMarker (AttributeExprBase ()) =
    AttributeExprBase GraphRefTransactionMarker

class NormalizableMarker a where
  normalizeMarker :: a -> NormalizeMarker a

instance NormalizableMarker (AttributeNamesExprBase ()) where
  normalizeMarker expr =
    case expr of
      AttributeNames names -> AttributeNames names
      InvertedAttributeNames names -> InvertedAttributeNames names
      UnionAttributeNames a b -> UnionAttributeNames (normalizeMarker a) (normalizeMarker b)
      IntersectAttributeNames a b -> IntersectAttributeNames (normalizeMarker a) (normalizeMarker b)
      RelationalExprAttributeNames relExpr ->
        RelationalExprAttributeNames (normalizeMarker relExpr)

instance NormalizableMarker (RelationalExprBase (AttributeNamesExprBase ()) ()) where
  normalizeMarker expr =
    case expr of
      MakeRelationFromExprs attrExprs tupleExprs -> 
        MakeRelationFromExprs ((fmap . fmap) normalizeMarker attrExprs) (normalizeMarker tupleExprs)
      MakeStaticRelation attrs tupSet ->
        MakeStaticRelation attrs tupSet
      ExistingRelation rel ->
        ExistingRelation rel
      RelationVariable nam () ->
        RelationVariable nam UncommittedContextMarker
      RelationValuedAttribute nam ->
        RelationValuedAttribute nam
      Project attrExprs expr' -> 
        Project (normalizeMarker attrExprs) (normalizeMarker expr')
      Union exprA exprB ->
        Union (normalizeMarker exprA) (normalizeMarker exprB)
      Join exprA exprB ->
        Join (normalizeMarker exprA) (normalizeMarker exprB)
      Rename renameAssocs expr' ->
        Rename renameAssocs (normalizeMarker expr')
      Difference exprA exprB ->
        Difference (normalizeMarker exprA) (normalizeMarker exprB)
      Group attrNameExprs gname expr' -> do
        Group (normalizeMarker attrNameExprs) gname (normalizeMarker expr')
      Ungroup ungname expr' ->
        Ungroup ungname (normalizeMarker expr')
      Restrict rpredExpr expr' ->
        Restrict (normalizeMarker rpredExpr) (normalizeMarker expr')
      Equals exprA exprB ->
        Equals (normalizeMarker exprA) (normalizeMarker exprB)
      NotEquals exprA exprB ->
        NotEquals (normalizeMarker exprA) (normalizeMarker exprB)
      Extend extendTuplesExpr expr' ->
        Extend (normalizeMarker extendTuplesExpr) (normalizeMarker expr')
      With withNamesAssocs expr' ->
        With (normalizeMarker withNamesAssocs) (normalizeMarker expr')
      
instance NormalizableMarker (TupleExprsBase (AttributeNamesExprBase ()) ()) where
  normalizeMarker (TupleExprs () tups) = TupleExprs UncommittedContextMarker (map normalizeMarker tups)

instance NormalizableMarker (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) where
  normalizeMarker expr = do
    case expr of
      TruePredicate ->
        TruePredicate
      AndPredicate a b ->
        AndPredicate (normalizeMarker a) (normalizeMarker b)
      OrPredicate a b ->
        OrPredicate (normalizeMarker a) (normalizeMarker b)
      NotPredicate a ->
        NotPredicate (normalizeMarker a)
      RelationalExprPredicate relExpr ->
        RelationalExprPredicate (normalizeMarker relExpr)
      AtomExprPredicate atomExpr ->
        AtomExprPredicate (normalizeMarker atomExpr)
      AttributeEqualityPredicate nam atomExpr ->
        AttributeEqualityPredicate nam (normalizeMarker atomExpr)
  
instance NormalizableMarker (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) where
  normalizeMarker (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname (normalizeMarker atomExpr)
  
instance NormalizableMarker (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) where
  normalizeMarker assocs =
    let mapper (WithNameExpr rv (),
                relExpr') = (WithNameExpr rv UncommittedContextMarker,
                             normalizeMarker relExpr') in
    map mapper assocs

instance NormalizableMarker (TupleExprBase (AttributeNamesExprBase ()) ()) where
  normalizeMarker  (TupleExpr tupExprMap) = do
    let mapper (k,v) = do
          let v' = normalizeMarker v in
            (k,v')
    TupleExpr (M.fromList (map mapper (M.toList tupExprMap)))

instance NormalizableMarker (AtomExprBase (AttributeNamesExprBase ()) ()) where
  normalizeMarker expr = do
    case expr of
      AttributeAtomExpr nam ->
        AttributeAtomExpr nam
      SubrelationAttributeAtomExpr relAttr subAttr ->
        SubrelationAttributeAtomExpr relAttr subAttr
      NakedAtomExpr atom ->
        NakedAtomExpr atom
      FunctionAtomExpr fName atomExprs () ->
        FunctionAtomExpr fName (map normalizeMarker atomExprs) UncommittedContextMarker
      RelationAtomExpr expr' ->
        RelationAtomExpr (normalizeMarker expr')
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr (normalizeMarker ifE) (normalizeMarker thenE) (normalizeMarker elseE)
      ConstructedAtomExpr dConsName atomExprs () ->
        ConstructedAtomExpr dConsName (map normalizeMarker atomExprs) UncommittedContextMarker

instance NormalizableMarker (WithNameExprBase ()) where
  normalizeMarker (WithNameExpr rvName ()) =
    WithNameExpr rvName UncommittedContextMarker

instance NormalizableMarker (AttributeExprBase ()) where
  normalizeMarker expr =
    case expr of
      AttributeAndTypeNameExpr attrName tCons () ->
        AttributeAndTypeNameExpr attrName tCons UncommittedContextMarker
      NakedAttributeExpr attr ->
        NakedAttributeExpr attr
