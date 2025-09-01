-- | Return Relation Variables mentioned in the data structure. Useful for access control.
module ProjectM36.RelationVariablesMentioned where
import ProjectM36.Base
import ProjectM36.WithNameExpr
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

mentionsRelVar :: RelationVariablesMentioned a => a -> Bool
mentionsRelVar x =
  not (S.null (relVarsMentioned x))

class RelationVariablesMentioned a where
  relVarsMentioned :: a -> S.Set RelVarName

instance RelationVariablesMentioned (AtomExprBase a) where
  relVarsMentioned expr = 
   case expr of
    AttributeAtomExpr{} -> mempty
    SubrelationAttributeAtomExpr{} -> mempty
    NakedAtomExpr{} -> mempty
    FunctionAtomExpr _ args _ ->
      S.unions (map relVarsMentioned args)
    RelationAtomExpr relExpr ->
      relVarsMentioned relExpr
    IfThenAtomExpr exprA exprB exprC ->
      S.unions (map relVarsMentioned [exprA, exprB, exprC])
    ConstructedAtomExpr _ args _ ->
      S.unions (map relVarsMentioned args)

instance RelationVariablesMentioned (RelationalExprBase a) where
  relVarsMentioned expr =
    case expr of
      MakeRelationFromExprs _ tupleExprs ->
        relVarsMentioned tupleExprs
      MakeStaticRelation _ tupSet ->
        relVarsMentioned tupSet
      ExistingRelation{} -> mempty
      RelationVariable nam _ -> S.singleton nam
      RelationValuedAttribute{} -> mempty
      Project _ exprA -> relVarsMentioned exprA
      Union exprA exprB -> relVarsMentioned exprA <> relVarsMentioned exprB
      Join exprA exprB -> relVarsMentioned exprA <> relVarsMentioned exprB
      Rename _ exprA -> relVarsMentioned exprA
      Difference exprA exprB -> relVarsMentioned exprA <> relVarsMentioned exprB
      Group _ _ exprA -> relVarsMentioned exprA
      Ungroup _ exprA -> relVarsMentioned exprA
      Restrict _ exprA -> relVarsMentioned exprA
      Equals exprA exprB -> relVarsMentioned exprA <> relVarsMentioned exprB
      NotEquals exprA exprB -> relVarsMentioned exprA <> relVarsMentioned exprB
      Extend extendExpr exprA -> relVarsMentioned extendExpr <> relVarsMentioned exprA
      With withNames exprA -> relVarsMentioned exprA `S.difference` macroNames withNames

instance RelationVariablesMentioned (ExtendTupleExprBase a) where
  relVarsMentioned (AttributeExtendTupleExpr _attrName atomExpr) =
    relVarsMentioned atomExpr

instance RelationVariablesMentioned (TupleExprsBase a) where
  relVarsMentioned (TupleExprs _ tExprs) =
    S.unions (map relVarsMentioned tExprs)

instance RelationVariablesMentioned (TupleExprBase a) where
  relVarsMentioned (TupleExpr tupmap) =
    S.unions (M.map relVarsMentioned tupmap)
    
instance RelationVariablesMentioned RelationTupleSet where
  relVarsMentioned (RelationTupleSet tups) =
    S.unions (map relVarsMentioned tups)

instance RelationVariablesMentioned RelationTuple where
  relVarsMentioned (RelationTuple _ atoms) =
    S.unions (V.toList (V.map relVarsMentioned atoms))

instance RelationVariablesMentioned Atom where
  relVarsMentioned atom =
    case atom of
      IntAtom{} -> mempty
      IntegerAtom{} -> mempty
      ScientificAtom{} -> mempty
      DoubleAtom{} -> mempty
      TextAtom{} -> mempty
      DayAtom{} -> mempty
      DateTimeAtom{} -> mempty
      ByteStringAtom{} -> mempty
      BoolAtom{} -> mempty
      UUIDAtom{} -> mempty
      RelationAtom{} -> mempty
      RelationalExprAtom expr -> relVarsMentioned expr
      SubrelationFoldAtom{} -> mempty
      ConstructedAtom _ _ atoms' -> S.unions (map relVarsMentioned atoms')
