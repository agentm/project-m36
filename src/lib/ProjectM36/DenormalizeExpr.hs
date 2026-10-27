{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module ProjectM36.DenormalizeExpr where
import ProjectM36.Base

-- | Convert a structure reliant on normalized- used by isomorphic schema conversions
class DenormalizeAttributeNames a where
  type AttributeExprRet a
  
  denormalizeAttributeNames :: a -> AttributeExprRet a

instance DenormalizeAttributeNames (RelationalExprBase AttributeNames a) where
  type AttributeExprRet (RelationalExprBase AttributeNames a) = RelationalExprBase (AttributeNamesExprBase a) a

  denormalizeAttributeNames expr =
    case expr of
      MakeRelationFromExprs mAttrExprs tupExprs ->
        MakeRelationFromExprs mAttrExprs (denormalizeAttributeNames tupExprs)
      MakeStaticRelation attrs tupSet ->
        MakeStaticRelation attrs tupSet
      ExistingRelation rel ->
        ExistingRelation rel
      RelationVariable rv a ->
        RelationVariable rv a
      RelationValuedAttribute aName ->
        RelationValuedAttribute aName
      Project attrNames relExpr ->
        Project (denormalizeAttributeNames' attrNames) (denormalizeAttributeNames relExpr)
      Union a b ->
        Union (denormalizeAttributeNames a) (denormalizeAttributeNames b)
      Join a b ->
        Join (denormalizeAttributeNames a) (denormalizeAttributeNames b)
      Rename assocs relExpr ->
        Rename assocs (denormalizeAttributeNames relExpr)
      Difference a b ->
        Difference (denormalizeAttributeNames a) (denormalizeAttributeNames b)
      Group attrNames aName relExpr ->
        Group (denormalizeAttributeNames' attrNames) aName (denormalizeAttributeNames relExpr)
      Ungroup aName relExpr ->
        Ungroup aName (denormalizeAttributeNames relExpr)
      Restrict predExpr relExpr ->
        Restrict (denormalizeAttributeNames predExpr) (denormalizeAttributeNames relExpr)
      Equals a b ->
        Equals (denormalizeAttributeNames a) (denormalizeAttributeNames b)
      NotEquals a b ->
        NotEquals (denormalizeAttributeNames a) (denormalizeAttributeNames b)
      Extend extendTupExpr relExpr ->
        Extend (denormalizeAttributeNames extendTupExpr) (denormalizeAttributeNames relExpr)
      With withAssocs relExpr ->
        With (denormalizeAttributeNames withAssocs) (denormalizeAttributeNames relExpr)
      
instance DenormalizeAttributeNames (TupleExprsBase AttributeNames a) where
  type AttributeExprRet (TupleExprsBase AttributeNames a) = TupleExprsBase (AttributeNamesExprBase a) a

  denormalizeAttributeNames (TupleExprs a exprs) =
    TupleExprs a (map denormalizeAttributeNames exprs)

instance DenormalizeAttributeNames (TupleExprBase AttributeNames a) where
  type AttributeExprRet (TupleExprBase AttributeNames a) = TupleExprBase (AttributeNamesExprBase a) a

  denormalizeAttributeNames (TupleExpr tupMap) =
    TupleExpr (fmap denormalizeAttributeNames tupMap)

instance DenormalizeAttributeNames (RestrictionPredicateExprBase AttributeNames a) where
  type AttributeExprRet (RestrictionPredicateExprBase AttributeNames a) = RestrictionPredicateExprBase (AttributeNamesExprBase a) a

  denormalizeAttributeNames expr =
    case expr of
      TruePredicate ->
        TruePredicate
      AndPredicate a b ->
        AndPredicate (denormalizeAttributeNames a) (denormalizeAttributeNames b)
      OrPredicate a b ->
        OrPredicate (denormalizeAttributeNames a) (denormalizeAttributeNames b)
      NotPredicate a ->
        NotPredicate (denormalizeAttributeNames a)
      RelationalExprPredicate relExpr ->
        RelationalExprPredicate (denormalizeAttributeNames relExpr)
      AtomExprPredicate atomExpr ->
        AtomExprPredicate (denormalizeAttributeNames atomExpr)
      AttributeEqualityPredicate aName atomExpr ->
        AttributeEqualityPredicate aName (denormalizeAttributeNames atomExpr)

instance DenormalizeAttributeNames (ExtendTupleExprBase AttributeNames a) where
  type AttributeExprRet (ExtendTupleExprBase AttributeNames a) = ExtendTupleExprBase (AttributeNamesExprBase a) a

  denormalizeAttributeNames (AttributeExtendTupleExpr aName atomExpr) =
    AttributeExtendTupleExpr aName (denormalizeAttributeNames atomExpr)

instance DenormalizeAttributeNames (WithNamesAssocsBase AttributeNames a) where
  type AttributeExprRet (WithNamesAssocsBase AttributeNames a) = WithNamesAssocsBase (AttributeNamesExprBase a) a

  denormalizeAttributeNames expr = fmap (\(wexpr, relExpr) -> (wexpr, denormalizeAttributeNames relExpr)) expr

instance DenormalizeAttributeNames (AtomExprBase AttributeNames a) where
  type AttributeExprRet (AtomExprBase AttributeNames a) = AtomExprBase (AttributeNamesExprBase a) a

  denormalizeAttributeNames expr =
    case expr of
      AttributeAtomExpr aName ->
        AttributeAtomExpr aName
      SubrelationAttributeAtomExpr a b ->
        SubrelationAttributeAtomExpr a b
      NakedAtomExpr a ->
        NakedAtomExpr a
      FunctionAtomExpr fname args a ->
        FunctionAtomExpr fname (map denormalizeAttributeNames args) a
      RelationAtomExpr relExpr ->
        RelationAtomExpr (denormalizeAttributeNames relExpr)
      IfThenAtomExpr a b c ->
        IfThenAtomExpr (denormalizeAttributeNames a) (denormalizeAttributeNames b) (denormalizeAttributeNames c)
      ConstructedAtomExpr dConsName args a ->
        ConstructedAtomExpr dConsName (map denormalizeAttributeNames args) a


denormalizeAttributeNames' :: AttributeNames -> AttributeNamesExprBase a
denormalizeAttributeNames' = AttributeNames 

