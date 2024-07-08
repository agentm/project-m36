module ProjectM36.WithNameExpr where
import ProjectM36.Base
import Data.List (find)

-- substitute all instances of With-based macros to remove macro context
-- ideally, we would use a different relational expr type to "prove" that the with macros can no longer exist

-- | 
lookup :: RelVarName -> WithNamesAssocsBase a -> Maybe (RelationalExprBase a)
lookup matchrv assocs =
  snd <$> find (\(WithNameExpr rv _, _) -> rv == matchrv) assocs

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
    e@NakedAtomExpr{} -> e
    FunctionAtomExpr fname atomExprs tid ->
      FunctionAtomExpr fname (map (substituteWithNameMacrosAtomExpr macros) atomExprs) tid
    AggregateFunctionAtomExpr fname aggInfo atomExprs tid ->
      AggregateFunctionAtomExpr fname aggInfo (map (substituteWithNameMacrosAtomExpr macros) atomExprs) tid
    RelationAtomExpr reExpr ->
      RelationAtomExpr (substituteWithNameMacros macros reExpr)
    IfThenAtomExpr ifE thenE elseE ->
      IfThenAtomExpr (substituteWithNameMacrosAtomExpr macros ifE) (substituteWithNameMacrosAtomExpr macros thenE) (substituteWithNameMacrosAtomExpr macros elseE)
    ConstructedAtomExpr dconsName atomExprs tid ->
      ConstructedAtomExpr dconsName (map (substituteWithNameMacrosAtomExpr macros) atomExprs) tid

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

      
