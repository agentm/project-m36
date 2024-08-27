{-# LANGUAGE FlexibleInstances #-}
module ProjectM36.WithNameExpr where
import ProjectM36.Base
import Data.List (find)

lookup :: RelVarName -> WithNamesAssocsBase a -> Maybe (RelationalExprBase a)
lookup matchrv assocs =
  snd <$> find (\(WithNameExpr rv _, _) -> rv == matchrv) assocs
  
-- substitute all instances of With-based macros to remove macro context
-- ideally, we would use a different relational expr type to "prove" that the with macros can no longer exist
-- | Drop macros into the relational expression wherever they are referenced.
substituteWithNameMacros ::
  GraphRefWithNameAssocs ->
  GraphRefRelationalExpr ->
  GraphRefRelationalExpr
substituteWithNameMacros _ e@MakeRelationFromExprs{} = e
substituteWithNameMacros _ e@MakeStaticRelation{} = e
substituteWithNameMacros _ e@ExistingRelation{} = e
substituteWithNameMacros _ e@RelationValuedAttribute{} = e
substituteWithNameMacros macros e@(RelationVariable rvname tid) =
  let
    macroFilt (WithNameExpr macroName macroTid, _) = rvname == macroName && tid== macroTid in
  case filter macroFilt macros of
    [] -> e
    [(_,replacement)] -> replacement
    _ -> error "more than one macro matched!"
substituteWithNameMacros macros (Project attrs expr) =
  Project (substituteWithNameMacrosAttributeNames macros attrs) (substituteWithNameMacros macros expr)
substituteWithNameMacros macros (Union exprA exprB) =
  Union (substituteWithNameMacros macros exprA) (substituteWithNameMacros macros exprB)
substituteWithNameMacros macros (Join exprA exprB) =
  Join (substituteWithNameMacros macros exprA) (substituteWithNameMacros macros exprB)
substituteWithNameMacros macros (Rename attrs expr) =
  Rename attrs (substituteWithNameMacros macros expr)
substituteWithNameMacros macros (Difference exprA exprB) =
  Difference (substituteWithNameMacros macros exprA) (substituteWithNameMacros macros exprB)
substituteWithNameMacros macros (Group attrs attr expr) =
  Group attrs attr (substituteWithNameMacros macros expr)  
substituteWithNameMacros macros (Ungroup attr expr) =
  Ungroup attr (substituteWithNameMacros macros expr)  
substituteWithNameMacros macros (Restrict pred' expr) =
  Restrict (substituteWithNameMacrosRestrictionPredicate macros pred') (substituteWithNameMacros macros expr)  
substituteWithNameMacros macros (Equals exprA exprB) =
  Equals (substituteWithNameMacros macros exprA) (substituteWithNameMacros macros exprB)
substituteWithNameMacros macros (NotEquals exprA exprB) =
  NotEquals (substituteWithNameMacros macros exprA) (substituteWithNameMacros macros exprB)
substituteWithNameMacros macros (Extend extendTup expr) =
  Extend (substituteWitNameMacrosExtendTupleExpr macros extendTup) (substituteWithNameMacros macros expr)
substituteWithNameMacros macros (With moreMacros expr) =
  --collect and update nested with exprs
  let newMacros = foldr macroFolder macros moreMacros
      macroFolder (wnexpr, mexpr) acc =
        let subExpr = substituteWithNameMacros macros mexpr in
        filter (\(w,_) -> w /= wnexpr) acc ++ [(wnexpr, subExpr)] in
        --scan for a match- if it exists, replace it (representing a with clause at a lower level
  substituteWithNameMacros newMacros expr


substituteWithNameMacrosRestrictionPredicate :: GraphRefWithNameAssocs -> GraphRefRestrictionPredicateExpr -> GraphRefRestrictionPredicateExpr
substituteWithNameMacrosRestrictionPredicate macros pred' =
  let sub = substituteWithNameMacrosRestrictionPredicate macros in
  case pred' of
    TruePredicate -> pred'
    AndPredicate exprA exprB ->
      AndPredicate (sub exprA) (sub exprB)
    OrPredicate exprA exprB ->
      OrPredicate (sub exprA) (sub exprB)
    NotPredicate expr ->
      NotPredicate (sub expr)
    RelationalExprPredicate reexpr ->
      RelationalExprPredicate (substituteWithNameMacros macros reexpr)
    AtomExprPredicate atomExpr ->
      AtomExprPredicate (substituteWithNameMacrosAtomExpr macros atomExpr)
    AttributeEqualityPredicate attrName atomExpr ->
      AttributeEqualityPredicate attrName (substituteWithNameMacrosAtomExpr macros atomExpr)

substituteWitNameMacrosExtendTupleExpr :: GraphRefWithNameAssocs -> GraphRefExtendTupleExpr -> GraphRefExtendTupleExpr
substituteWitNameMacrosExtendTupleExpr macros (AttributeExtendTupleExpr attrName atomExpr) =
  AttributeExtendTupleExpr attrName (substituteWithNameMacrosAtomExpr macros atomExpr)

substituteWithNameMacrosAtomExpr :: GraphRefWithNameAssocs -> GraphRefAtomExpr -> GraphRefAtomExpr
substituteWithNameMacrosAtomExpr macros atomExpr =
  case atomExpr of
    e@AttributeAtomExpr{} -> e
    e@SubrelationAttributeAtomExpr{} -> e
    e@NakedAtomExpr{} -> e
    FunctionAtomExpr fname atomExprs tid ->
      FunctionAtomExpr fname (map (substituteWithNameMacrosAtomExpr macros) atomExprs) tid
    RelationAtomExpr reExpr ->
      RelationAtomExpr (substituteWithNameMacros macros reExpr)
    IfThenAtomExpr ifE thenE elseE ->
      IfThenAtomExpr (substituteWithNameMacrosAtomExpr macros ifE) (substituteWithNameMacrosAtomExpr macros thenE) (substituteWithNameMacrosAtomExpr macros elseE)
    ConstructedAtomExpr dconsName atomExprs tid ->
      ConstructedAtomExpr dconsName (map (substituteWithNameMacrosAtomExpr macros) atomExprs) tid


{-
-- | Return error if with clause name is shadowed. We're not sure if name shadowing is a useful feature or a footgun, so disable it for now.
class ValidateWith a where
  validate :: S.Set RelVarName -> WithNameAssocs -> a -> Either RelationalError a
  

instance ValidateWith GraphRefRelationalExpr where
  validate inUseSet withAssocs expr =
    case expr of
      MakeRelationFromExprs mAttrs tupleExprs ->
        MakeRelationFromExprs mAttrs <$> val tupleExprs
      rel@MakeStaticRelation{} ->
        pure expr
      rel@ExistingRelation{} ->
        pure expr
      RelationVariable rvName marker -> do
        checkName rvName
      Project attrs exprA ->
        Project attrs <$> val exprA
      Union exprA exprB ->
        Union <$> val exprA <*> val exprB
      Join exprA exprB ->
        Join <$> val exprA <*> val exprB
      Rename nam1 nam2 exprA ->
        Rename nam1 nam2 <$> val exprA
      Difference exprA exprB ->
        Difference <$> val exprA <*> val exprB
      Group attrs nam exprA ->
        Group attrs nam <$> val exprA
      Ungroup nam expr' ->
        Ungroup nam <$> val expr'
      Restrict pred exprA ->
        Restrict <$> val pred <*> val exprA
      Equals exprA exprB ->
        Equals <$> val exprA <*> val exprB
      NotEquals exprA exprB ->
        NotEquals <$> val exprA <*> val exprB
      Extend extExpr exprA ->
        Extend <$> val extExpr <*> val exprA
      With withAssocs exprA -> do
        mapM_ checkName (map fst withAssocs)
        With withAssocs <$> val exprA
   where
     val :: forall a. (ValidateWith a) => a -> Either RelationalError a
     val x = validate inUseSet withAssocs x
     checkName :: RelVarName -> Either RelationalError GraphRefRelationalExpr
     checkName nam =
       if nam S.member inUseSet then
         Left (RelVarNameShadowingForbiddenError nam)
         else
         pure expr
         
      
-}

substituteWithNameMacrosAttributeNames :: GraphRefWithNameAssocs -> GraphRefAttributeNames -> GraphRefAttributeNames
substituteWithNameMacrosAttributeNames macros attrNames =
  case attrNames of
    AttributeNames{} -> attrNames
    InvertedAttributeNames{} -> attrNames
    UnionAttributeNames a b ->
      UnionAttributeNames (substituteWithNameMacrosAttributeNames macros a) (substituteWithNameMacrosAttributeNames macros b)
    IntersectAttributeNames a b ->
      IntersectAttributeNames (substituteWithNameMacrosAttributeNames macros a) (substituteWithNameMacrosAttributeNames macros b)
    RelationalExprAttributeNames relExpr ->
      RelationalExprAttributeNames (substituteWithNameMacros macros relExpr)

      

