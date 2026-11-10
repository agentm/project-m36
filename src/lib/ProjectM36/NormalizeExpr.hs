-- | Functions to convert all types of expresions into their GraphRef- equivalents.
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, RankNTypes, FlexibleContexts, TypeOperators #-}
module ProjectM36.NormalizeExpr where
import ProjectM36.Base (GraphRefTransactionMarker, RelationalExprBase(..), RestrictionPredicateExprBase(..), WithNameExprBase(..), AttributeNamesExprBase(..), AttributeNamesExprBase(..), DatabaseContextExprBase(..), DatabaseContextIOExprBase(..), ExtendTupleExprBase(..), AtomExprBase(..), TupleExprsBase(..), TupleExprBase(..), AttributeExprBase(..), AttributeNames, WithNamesAssocsBase , AttributeNamesExprBase(..), AttributeNameAtomExprMapBase, InclusionDependencyBase(..), GraphRefTransactionMarker(..))
import ProjectM36.Relation (attributes)
import ProjectM36.Error
import ProjectM36.TransGraphRelationalExpression.Types
import ProjectM36.TransactionGraph.Types
import ProjectM36.TransactionGraph
import ProjectM36.Transaction.Types
import ProjectM36.RelationalExpression.Types
import ProjectM36.RelationalExpression
import ProjectM36.DatabaseContext.Types
import ProjectM36.AttributeNamesExpr (normalizeMarker)

import Control.Monad.Except
import qualified Data.Map as M
import Control.Monad.Reader (local)

-- | Normalize attribute expressions to attribute names across Base types.
type family NormalizeAttributeNames a where
  NormalizeAttributeNames (RelationalExprBase at a) = RelationalExprBase AttributeNames a

  NormalizeAttributeNames (TupleExprsBase at a) = TupleExprsBase AttributeNames a

  NormalizeAttributeNames (AttributeNamesExprBase a) = AttributeNames

  NormalizeAttributeNames (RestrictionPredicateExprBase (AttributeNamesExprBase a) b) = RestrictionPredicateExprBase AttributeNames b

  NormalizeAttributeNames (ExtendTupleExprBase (AttributeNamesExprBase a) b) = ExtendTupleExprBase AttributeNames b

  NormalizeAttributeNames (WithNamesAssocsBase (AttributeNamesExprBase a) b) = WithNamesAssocsBase AttributeNames b

  NormalizeAttributeNames (TupleExprBase (AttributeNamesExprBase a) b) = TupleExprBase AttributeNames b
  
  NormalizeAttributeNames (AtomExprBase (AttributeNamesExprBase a) b) = AtomExprBase AttributeNames b 

--  NormalizeAttributeNames (RelationalExprBase GraphRefAttributeNamesExpr GraphRefTransactionMarker) = RelationalExprBase AttributeNames GraphRefTransactionMarker

  NormalizeAttributeNames (DatabaseContextExprBase (AttributeNamesExprBase a) a r) = DatabaseContextExprBase AttributeNames a r

  NormalizeAttributeNames (AttributeNameAtomExprMapBase (AttributeNamesExprBase a) a) = AttributeNameAtomExprMapBase AttributeNames a

  NormalizeAttributeNames (InclusionDependencyBase (AttributeNamesExprBase a) b) = InclusionDependencyBase AttributeNames ()

  NormalizeAttributeNames (DatabaseContextExprBase AttributeNames () r) = DatabaseContextExprBase AttributeNames () r

  -- trans-graph relational expression normalization

--  NormalizeAttributeNames TransGraphRelationalExpr = RelationalExprBase AttributeNames GraphRefTransactionMarker

class NormalizableAttributeNames a where
  -- | convert AttributeNamesExpr -> AttributeNames (removing expressions to calculate attribute names)
  
  normalizeAttributeNames :: a -> GraphRefRelationalExprM (NormalizeAttributeNames a)

instance NormalizableAttributeNames (RelationalExprBase AttributeNames a) where
  normalizeAttributeNames expr = pure expr

instance NormalizableAttributeNames (RelationalExprBase (AttributeNamesExprBase ()) ()) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames
    case expr of
      MakeRelationFromExprs attrExprs tupleExprs -> 
        MakeRelationFromExprs attrExprs <$> norm tupleExprs
      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)
      ExistingRelation rel ->
        pure (ExistingRelation rel)
      RelationVariable nam x ->
        pure (RelationVariable nam x)
      RelationValuedAttribute nam ->
        pure (RelationValuedAttribute nam)
      Project attrExprs expr' -> do
        normExpr' <- norm expr'
        exprType' <- typeForGraphRefRelationalExpr (fmap (const UncommittedContextMarker) normExpr')
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')
        Project <$> local env (normalizeAttributeNames attrExprs) <*> pure normExpr'
      Union exprA exprB ->
        Union <$> norm exprA <*> norm exprB
      Join exprA exprB ->
        Join <$> norm exprA <*> norm exprB
      Rename renameAssocs expr' ->
        Rename renameAssocs <$> norm expr'
      Difference exprA exprB ->
        Difference <$> norm exprA <*> norm exprB
      Group attrNameExprs gname expr' -> do
        Group <$> norm attrNameExprs <*> pure gname <*> norm expr'
      Ungroup ungname expr' ->
        Ungroup ungname <$> norm expr'
      Restrict rpredExpr expr' ->
        Restrict <$> norm rpredExpr <*> norm expr'
      Equals exprA exprB ->
        Equals <$> norm exprA <*> norm exprB
      NotEquals exprA exprB ->
        NotEquals <$> norm exprA <*> norm exprB
      Extend extendTuplesExpr expr' ->
        Extend <$> norm extendTuplesExpr <*> norm expr'
      With withNamesAssocs expr' ->
        With <$> norm withNamesAssocs <*> norm expr'

instance NormalizableAttributeNames (TupleExprsBase (AttributeNamesExprBase ()) ()) where
  normalizeAttributeNames (TupleExprs a tups) = TupleExprs a <$> mapM normalizeAttributeNames tups

instance NormalizableAttributeNames (TupleExprBase (AttributeNamesExprBase ()) ()) where
  normalizeAttributeNames (TupleExpr tupExprMap) = do
    let mapper (k,v) = do
          v' <- normalizeAttributeNames v
          pure (k,v')
    TupleExpr . M.fromList <$> mapM mapper (M.toList tupExprMap)

instance NormalizableAttributeNames (AttributeNamesExprBase ()) where
  normalizeAttributeNames expr = do
    let normExpr = normalizeMarker expr
    evalGraphRefAttributeNamesExpr normalizeAttributeNames normExpr

instance NormalizableAttributeNames (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) where
  normalizeAttributeNames (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalizeAttributeNames atomExpr

instance NormalizableAttributeNames (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) where
  normalizeAttributeNames assocs = do
    let mapper (WithNameExpr rv tm,
                relExpr) = (,) <$> pure (WithNameExpr rv tm) <*> normalizeAttributeNames relExpr
    mapM mapper assocs

---

instance NormalizableAttributeNames (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames
    case expr of
      MakeRelationFromExprs attrExprs tupleExprs -> 
        MakeRelationFromExprs attrExprs <$> norm tupleExprs
      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)
      ExistingRelation rel ->
        pure (ExistingRelation rel)
      RelationVariable nam x ->
        pure (RelationVariable nam x)
      RelationValuedAttribute nam ->
        pure (RelationValuedAttribute nam)
      Project attrExprs expr' -> do
        normExpr <- norm expr'
        exprType' <- typeForGraphRefRelationalExpr normExpr
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')
        Project <$> local env (normalizeAttributeNames attrExprs) <*> pure normExpr
      Union exprA exprB ->
        Union <$> norm exprA <*> norm exprB
      Join exprA exprB ->
        Join <$> norm exprA <*> norm exprB
      Rename renameAssocs expr' ->
        Rename renameAssocs <$> norm expr'
      Difference exprA exprB ->
        Difference <$> norm exprA <*> norm exprB
      Group attrNameExprs gname expr' -> do
        normExpr <- normalizeAttributeNames expr'
        exprType' <- typeForGraphRefRelationalExpr normExpr
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')
        Group <$> local env (normalizeAttributeNames attrNameExprs) <*> pure gname <*> pure normExpr
      Ungroup ungname expr' ->
        Ungroup ungname <$> norm expr'
      Restrict rpredExpr expr' -> do
        normExpr <- norm expr'
        exprType' <- typeForGraphRefRelationalExpr normExpr
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')       
        Restrict <$> local env (normalizeAttributeNames rpredExpr) <*> pure normExpr
      Equals exprA exprB ->
        Equals <$> norm exprA <*> norm exprB
      NotEquals exprA exprB ->
        NotEquals <$> norm exprA <*> norm exprB
      Extend extendTuplesExpr expr' ->
        Extend <$> norm extendTuplesExpr <*> norm expr'
      With withNamesAssocs expr' ->
        With <$> norm withNamesAssocs <*> norm expr'

instance NormalizableAttributeNames (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames (TupleExprs a tups) = TupleExprs a <$> mapM normalizeAttributeNames tups

instance NormalizableAttributeNames (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames (TupleExpr tupExprMap) = do
    let mapper (k,v) = do
          (,) k <$> normalizeAttributeNames v
    TupleExpr . M.fromList <$> mapM mapper (M.toList tupExprMap)

instance NormalizableAttributeNames (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames
    case expr of
      TruePredicate ->
        pure TruePredicate
      AndPredicate a b ->
        AndPredicate <$> norm a <*> norm b
      OrPredicate a b ->
        OrPredicate <$> norm a <*> norm b
      NotPredicate a ->
        NotPredicate <$> norm a
      RelationalExprPredicate relExpr' ->
        RelationalExprPredicate <$> norm relExpr'
      AtomExprPredicate atomExpr ->
        AtomExprPredicate <$> norm atomExpr
      AttributeEqualityPredicate nam atomExpr ->
        AttributeEqualityPredicate nam <$> norm atomExpr

instance NormalizableAttributeNames (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames
    case expr of
      AttributeAtomExpr nam ->
        pure $ AttributeAtomExpr nam
      SubrelationAttributeAtomExpr relAttr subAttr ->
        pure (SubrelationAttributeAtomExpr relAttr subAttr)
      NakedAtomExpr atom ->
        pure $ NakedAtomExpr atom
      FunctionAtomExpr fName atomExprs a ->
        FunctionAtomExpr fName <$> mapM norm atomExprs  <*> pure a
      RelationAtomExpr expr' ->
        RelationAtomExpr <$> norm expr'
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr <$> norm ifE <*> norm thenE <*> norm elseE
      ConstructedAtomExpr dConsName atomExprs a ->
        ConstructedAtomExpr dConsName <$> mapM norm atomExprs <*> pure a

instance NormalizableAttributeNames (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalizeAttributeNames atomExpr

instance NormalizableAttributeNames (WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames assocs = do
    let mapper (WithNameExpr rv tm,
                relExpr') = (,) <$> pure (WithNameExpr rv tm) <*> normalizeAttributeNames relExpr'
    mapM mapper assocs

instance NormalizableAttributeNames (AttributeNamesExprBase GraphRefTransactionMarker) where
  normalizeAttributeNames expr = do
    let normalizeRelationalExpr = normalizeAttributeNames
    evalGraphRefAttributeNamesExpr normalizeRelationalExpr expr 

instance NormalizableAttributeNames (DatabaseContextExprBase (AttributeNamesExprBase ()) () r) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames
    case expr of
      NoOperation -> pure NoOperation
      Define nam attrExprs -> pure (Define nam attrExprs)
      Undefine nam -> pure (Undefine nam)
      Assign nam rexpr -> Assign nam <$> norm rexpr
      Insert nam rexpr -> Insert nam <$> norm rexpr
      Delete nam pred' -> Delete nam <$> norm pred'
      Update nam attrMap pred' -> Update nam <$> norm attrMap <*> norm pred'

      AddInclusionDependency nam dep ->
        pure (AddInclusionDependency nam dep)
      RemoveInclusionDependency nam ->
        pure (RemoveInclusionDependency nam)
      AddNotification nam exprA exprB exprC ->
        pure (AddNotification nam exprA exprB exprC)
      RemoveNotification nam ->
        pure (RemoveNotification nam)
      AddTypeConstructor tyDef consDefs ->
        pure (AddTypeConstructor tyDef consDefs)
      RemoveTypeConstructor tyName ->
        pure (RemoveTypeConstructor tyName)

      RemoveAtomFunction aFuncName ->
        pure (RemoveAtomFunction aFuncName)
      RemoveDatabaseContextFunction funcName' ->
        pure (RemoveDatabaseContextFunction funcName')
      ExecuteDatabaseContextFunction funcName' atomExprs ->
        ExecuteDatabaseContextFunction funcName' <$> mapM norm atomExprs
      AddRegisteredQuery n q -> pure (AddRegisteredQuery n q)
      RemoveRegisteredQuery n -> pure (RemoveRegisteredQuery n)
      AlterACL e -> pure (AlterACL e)
      MultipleExpr exprs -> MultipleExpr <$> mapM norm exprs      

instance NormalizableAttributeNames (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames
    case expr of
      TruePredicate ->
        pure TruePredicate
      AndPredicate a b ->
        AndPredicate <$> norm a <*> norm b
      OrPredicate a b ->
        OrPredicate <$> norm a <*> norm b
      NotPredicate a ->
        NotPredicate <$> norm a
      RelationalExprPredicate relExpr' ->
        RelationalExprPredicate <$> norm relExpr'
      AtomExprPredicate atomExpr ->
        AtomExprPredicate <$> norm atomExpr
      AttributeEqualityPredicate nam atomExpr ->
        AttributeEqualityPredicate nam <$> norm atomExpr

instance NormalizableAttributeNames (AtomExprBase (AttributeNamesExprBase ()) ()) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames 
    case expr of
      AttributeAtomExpr nam ->
        pure $ AttributeAtomExpr nam
      SubrelationAttributeAtomExpr relAttr subAttr ->
        pure (SubrelationAttributeAtomExpr relAttr subAttr)
      NakedAtomExpr atom ->
        pure $ NakedAtomExpr atom
      FunctionAtomExpr fName atomExprs a ->
        FunctionAtomExpr fName <$> mapM norm atomExprs  <*> pure a
      RelationAtomExpr expr' -> RelationAtomExpr <$> norm expr'
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr <$> norm ifE <*> norm thenE <*> norm elseE
      ConstructedAtomExpr dConsName atomExprs a ->
        ConstructedAtomExpr dConsName <$> mapM norm atomExprs <*> pure a

instance NormalizableAttributeNames (AttributeNameAtomExprMapBase (AttributeNamesExprBase ()) ()) where
  normalizeAttributeNames map' =
    M.fromList <$> mapM mapper (M.toList map')
    where
      mapper (k,v) = (,) <$> pure k <*> normalizeAttributeNames v

---

instance NormalizableAttributeNames (DatabaseContextExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker r) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames
    case expr of
      NoOperation -> pure NoOperation
      Define nam attrExprs -> pure (Define nam attrExprs)
      Undefine nam -> pure (Undefine nam)
      Assign nam rexpr -> Assign nam <$> norm rexpr
      Insert nam rexpr -> Insert nam <$> norm rexpr
      Delete nam pred' -> Delete nam <$> norm pred'
      Update nam attrMap pred' -> Update nam <$> norm attrMap <*> norm pred'

      AddInclusionDependency nam dep ->
        pure (AddInclusionDependency nam dep)
      RemoveInclusionDependency nam ->
        pure (RemoveInclusionDependency nam)
      AddNotification nam exprA exprB exprC ->
        pure (AddNotification nam exprA exprB exprC)
      RemoveNotification nam ->
        pure (RemoveNotification nam)
      AddTypeConstructor tyDef consDefs ->
        pure (AddTypeConstructor tyDef consDefs)
      RemoveTypeConstructor tyName ->
        pure (RemoveTypeConstructor tyName)

      RemoveAtomFunction aFuncName ->
        pure (RemoveAtomFunction aFuncName)
      RemoveDatabaseContextFunction funcName' ->
        pure (RemoveDatabaseContextFunction funcName')
      ExecuteDatabaseContextFunction funcName' atomExprs ->
        ExecuteDatabaseContextFunction funcName' <$> mapM norm atomExprs
      AddRegisteredQuery n q -> pure (AddRegisteredQuery n q)
      RemoveRegisteredQuery n -> pure (RemoveRegisteredQuery n)
      AlterACL e -> pure (AlterACL e)
      MultipleExpr exprs -> MultipleExpr <$> mapM norm exprs      

instance NormalizableAttributeNames (AttributeNameAtomExprMapBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames map' =
    M.fromList <$> mapM mapper (M.toList map')
    where
      mapper (k,v) = (,) <$> pure k <*> normalizeAttributeNames v


instance NormalizableAttributeNames (InclusionDependencyBase (AttributeNamesExprBase ()) ()) where
  normalizeAttributeNames (InclusionDependency relExprA relExprB) = do
    InclusionDependency <$> normalizeAttributeNames relExprA <*> normalizeAttributeNames relExprB

instance NormalizableAttributeNames (InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  normalizeAttributeNames (InclusionDependency relExprA relExprB) = do
    InclusionDependency <$> normalizeAttributeNames relExprA <*> normalizeAttributeNames relExprB

instance NormalizableAttributeNames (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  normalizeAttributeNames expr = 
     let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
         norm = normalizeAttributeNames
     in
    case expr of
      MakeRelationFromExprs attrExprs tupleExprs -> 
        MakeRelationFromExprs attrExprs <$> norm tupleExprs
      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)
      ExistingRelation rel ->
        pure (ExistingRelation rel)
      RelationVariable nam x ->
        pure (RelationVariable nam x)
      RelationValuedAttribute nam ->
        pure (RelationValuedAttribute nam)
      Project attrExprs expr' -> do
        normExpr <- norm expr'
        exprType' <- typeForGraphRefRelationalExpr (fmap (const UncommittedContextMarker) normExpr)
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')       
        Project <$> local env (norm attrExprs) <*> pure normExpr
      Union exprA exprB ->
        Union <$> norm exprA <*> norm exprB
      Join exprA exprB ->
        Join <$> norm exprA <*> norm exprB
      Rename renameAssocs expr' ->
        Rename renameAssocs <$> norm expr'
      Difference exprA exprB ->
        Difference <$> norm exprA <*> norm exprB
      Group attrNameExprs gname expr' ->
        Group <$> norm attrNameExprs <*> pure gname <*> norm expr'
      Ungroup ungname expr' ->
        Ungroup ungname <$> norm expr'
      Restrict rpredExpr expr' -> do
        normExpr <- norm expr'
        exprType' <- typeForGraphRefRelationalExpr (fmap (const UncommittedContextMarker) normExpr)
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')       
        Restrict <$> local env (norm rpredExpr) <*> pure normExpr
      Equals exprA exprB ->
        Equals <$> norm exprA <*> norm exprB
      NotEquals exprA exprB ->
        NotEquals <$> norm exprA <*> norm exprB
      Extend extendTuplesExpr expr' ->
        Extend <$> norm extendTuplesExpr <*> norm expr'
      With withNamesAssocs expr' ->
        With <$> norm withNamesAssocs <*> norm expr'

instance NormalizableAttributeNames (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  normalizeAttributeNames (TupleExpr tupExprMap) = do
    let mapper (k,v) = do
          v' <- normalizeAttributeNames v
          pure (k,v')
    TupleExpr . M.fromList <$> mapM mapper (M.toList tupExprMap)

instance NormalizableAttributeNames (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames
    case expr of
      AttributeAtomExpr nam ->
        pure $ AttributeAtomExpr nam
      SubrelationAttributeAtomExpr relAttr subAttr ->
        pure (SubrelationAttributeAtomExpr relAttr subAttr)
      NakedAtomExpr atom ->
        pure $ NakedAtomExpr atom
      FunctionAtomExpr fName atomExprs a ->
        FunctionAtomExpr fName <$> mapM norm atomExprs  <*> pure a
      RelationAtomExpr expr' -> RelationAtomExpr <$> norm expr'
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr <$> norm ifE <*> norm thenE <*> norm elseE
      ConstructedAtomExpr dConsName atomExprs a ->
        ConstructedAtomExpr dConsName <$> mapM norm atomExprs <*> pure a

instance NormalizableAttributeNames (WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  normalizeAttributeNames assocs = do
    let mapper (WithNameExpr rv tm,
                relExpr) = (,) <$> pure (WithNameExpr rv tm) <*> normalizeAttributeNames relExpr
    mapM mapper assocs

instance NormalizableAttributeNames (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  normalizeAttributeNames (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalizeAttributeNames atomExpr

instance NormalizableAttributeNames (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  normalizeAttributeNames (TupleExprs a tups) = TupleExprs a <$> mapM normalizeAttributeNames tups

instance NormalizableAttributeNames (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  normalizeAttributeNames expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames
    case expr of
      TruePredicate ->
        pure TruePredicate
      AndPredicate a b ->
        AndPredicate <$> norm a <*> norm b
      OrPredicate a b ->
        OrPredicate <$> norm a <*> norm b
      NotPredicate a ->
        NotPredicate <$> norm a
      RelationalExprPredicate relExpr' ->
        RelationalExprPredicate <$> norm relExpr'
      AtomExprPredicate atomExpr ->
        AtomExprPredicate <$> norm atomExpr
      AttributeEqualityPredicate nam atomExpr ->
        AttributeEqualityPredicate nam <$> norm atomExpr

  
---------------------------------------------------      
class NormalizableTransactionMarker a where
  type Args a
  type Ret a
  
  normalizeTransactionMarker :: Args a -> a -> Ret a

instance NormalizableTransactionMarker (RelationalExprBase (AttributeNamesExprBase ()) ()) where
  type Args (RelationalExprBase (AttributeNamesExprBase ()) ()) = GraphRefTransactionMarker
  type Ret (RelationalExprBase (AttributeNamesExprBase ()) ()) = RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
  
  normalizeTransactionMarker marker expr =
    let norm :: forall x.
                (NormalizableTransactionMarker x,
                 Args x ~ GraphRefTransactionMarker
                )
             => x -> Ret x
        norm expr' = normalizeTransactionMarker marker expr'
        in    
    case expr of
      MakeRelationFromExprs mAttrExprs tupleExprs -> do
        let mAttrExprs' = case mAttrExprs of
                            Nothing -> Nothing
                            Just exprs -> Just $ map norm exprs
        MakeRelationFromExprs mAttrExprs' (norm tupleExprs)
      MakeStaticRelation attrs tupSet ->
        MakeStaticRelation attrs tupSet
      ExistingRelation rel ->
        ExistingRelation rel
      RelationValuedAttribute attrName ->
        RelationValuedAttribute attrName
      RelationVariable rvname () -> 
        RelationVariable rvname marker
      Project transAttrNames expr' ->
        Project (norm transAttrNames) (norm expr')
      Union exprA exprB ->
        Union (norm exprA) (norm exprB)
      Join exprA exprB ->
        Join (norm exprA) (norm exprB)
      Rename attrs expr' ->
        Rename attrs (norm expr')
      Difference exprA exprB ->
        Difference (norm exprA) (norm exprB)
      Group transAttrNames attrName expr' ->
        Group 
        (norm transAttrNames)
        attrName
        (norm expr')
      Ungroup attrName expr' -> 
        Ungroup attrName (norm expr')
      Restrict predicateExpr expr' ->
        Restrict (norm predicateExpr) (norm expr')
      Equals exprA exprB ->
        Equals (norm exprA) (norm exprB)
      NotEquals exprA exprB ->  
        NotEquals (norm exprA) (norm exprB)
      Extend extendExpr expr' -> 
        Extend (norm extendExpr) (norm expr')
      With views expr' -> do
        let evaldViews = map (\(wnexpr, vexpr) ->
                                 (norm wnexpr, norm vexpr)
                             ) views
        With evaldViews (norm expr')

instance NormalizableTransactionMarker (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) where
  type Args (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) =
    GraphRefTransactionMarker
  type Ret (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) =
    RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
    
  normalizeTransactionMarker marker expr =
    let norm :: forall x. (NormalizableTransactionMarker x,
                           Args x ~ GraphRefTransactionMarker) =>
                x ->
                Ret x
        norm = normalizeTransactionMarker marker
    in
      case expr of
        TruePredicate ->
          TruePredicate
        AndPredicate a b ->
          AndPredicate (norm a) (norm b)
        OrPredicate a b ->
          OrPredicate (norm a) (norm b)
        NotPredicate a ->
          NotPredicate (norm a)
        RelationalExprPredicate relExpr ->
          RelationalExprPredicate (norm relExpr)
        AtomExprPredicate atomExpr ->
          AtomExprPredicate (norm atomExpr)
        AttributeEqualityPredicate aname atomExpr ->
          AttributeEqualityPredicate aname (norm atomExpr)

instance NormalizableTransactionMarker (TupleExprsBase (AttributeNamesExprBase ()) ()) where
  type Args (TupleExprsBase (AttributeNamesExprBase ()) ()) = GraphRefTransactionMarker
  type Ret (TupleExprsBase (AttributeNamesExprBase ()) ()) = TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
  
  normalizeTransactionMarker marker (TupleExprs () exprs) =
    TupleExprs marker (map (normalizeTransactionMarker marker) exprs)
    

instance NormalizableTransactionMarker (AttributeNamesExprBase ()) where
  type Args (AttributeNamesExprBase ()) = GraphRefTransactionMarker
  type Ret (AttributeNamesExprBase ()) = AttributeNamesExprBase GraphRefTransactionMarker
  normalizeTransactionMarker marker expr =
    case expr of
      AttributeNames names ->
        AttributeNames names
      InvertedAttributeNames names ->
        InvertedAttributeNames names
      UnionAttributeNames a b ->
        UnionAttributeNames (normalizeTransactionMarker marker a) (normalizeTransactionMarker marker b)
      IntersectAttributeNames a b ->
        IntersectAttributeNames (normalizeTransactionMarker marker a) (normalizeTransactionMarker marker b)
      RelationalExprAttributeNames relExpr ->
        RelationalExprAttributeNames (normalizeTransactionMarker marker relExpr)

instance NormalizableTransactionMarker (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) where
  type Args (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) =
    GraphRefTransactionMarker
  type Ret (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) =
    ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
  normalizeTransactionMarker marker (AttributeExtendTupleExpr attrs atomExpr) =
    AttributeExtendTupleExpr attrs (normalizeTransactionMarker marker atomExpr)

instance NormalizableTransactionMarker (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) where
  type Args (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) =
    GraphRefTransactionMarker
  type Ret (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) =
    WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
  
  normalizeTransactionMarker marker assocs =
    map mapper assocs
    where
      mapper (withExpr, relExpr) =
        (normalizeTransactionMarker marker withExpr,
         normalizeTransactionMarker marker relExpr)

instance NormalizableTransactionMarker (TupleExprBase (AttributeNamesExprBase ()) ()) where
  type Args (TupleExprBase (AttributeNamesExprBase ()) ()) =
    GraphRefTransactionMarker
  type Ret (TupleExprBase (AttributeNamesExprBase ()) ()) =
    TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
    
  normalizeTransactionMarker marker (TupleExpr tupMap) = TupleExpr (fmap (normalizeTransactionMarker marker) tupMap)
  
instance NormalizableTransactionMarker (AtomExprBase (AttributeNamesExprBase ()) ()) where
  type Args (AtomExprBase (AttributeNamesExprBase ()) ()) =
    GraphRefTransactionMarker
  type Ret (AtomExprBase (AttributeNamesExprBase ()) ()) =
    AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
  normalizeTransactionMarker marker expr =
    let norm :: forall x. (NormalizableTransactionMarker x, Args x ~ GraphRefTransactionMarker) => x -> Ret x
        norm = normalizeTransactionMarker marker
    in
    case expr of
      AttributeAtomExpr aname ->
        AttributeAtomExpr aname
      SubrelationAttributeAtomExpr aname aname2 ->
        SubrelationAttributeAtomExpr aname aname2
      NakedAtomExpr atom ->
        NakedAtomExpr atom
      FunctionAtomExpr fname atomExprs () ->
        FunctionAtomExpr fname (map norm atomExprs) marker
      RelationAtomExpr relExpr ->
        RelationAtomExpr (norm relExpr)
      IfThenAtomExpr a b c ->
        IfThenAtomExpr (norm a) (norm b) (norm c)
      ConstructedAtomExpr dConsName atomExprs () ->
        ConstructedAtomExpr dConsName (map norm atomExprs) marker
        
        
instance NormalizableTransactionMarker (DatabaseContextExprBase (AttributeNamesExprBase ()) () r) where
  type Args (DatabaseContextExprBase (AttributeNamesExprBase ()) () r) =
    GraphRefTransactionMarker
  type Ret (DatabaseContextExprBase (AttributeNamesExprBase ()) () r) =
    DatabaseContextExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker r
  normalizeTransactionMarker marker expr =
    case expr of
      NoOperation ->
        NoOperation
      Define rvname attrExprs ->
        Define rvname (map (normalizeTransactionMarker marker) attrExprs)
      Undefine rvname ->
        Undefine rvname
      Assign rvName relExpr ->
        Assign rvName (normalizeTransactionMarker marker relExpr)
      Insert rvName relExpr ->
        Insert rvName (normalizeTransactionMarker marker relExpr)
      Delete rvName predi ->
        Delete rvName (normalizeTransactionMarker marker predi)
      Update rvName attrAtomMap predi ->
        Update rvName (normalizeTransactionMarker marker attrAtomMap) (normalizeTransactionMarker marker predi)
      AddInclusionDependency depname incDep ->
        AddInclusionDependency depname incDep
      RemoveInclusionDependency depname ->
        RemoveInclusionDependency depname
      AddNotification notName a b c ->
        AddNotification notName a b c
      RemoveNotification notName ->
        RemoveNotification notName
      AddTypeConstructor tConsDef dConsDefs ->
        AddTypeConstructor tConsDef dConsDefs
      RemoveTypeConstructor tConsName ->
        RemoveTypeConstructor tConsName
      RemoveAtomFunction fname ->
        RemoveAtomFunction fname
      RemoveDatabaseContextFunction fname ->
        RemoveDatabaseContextFunction fname
      ExecuteDatabaseContextFunction fname atomExprs ->
        ExecuteDatabaseContextFunction fname (map (normalizeTransactionMarker marker) atomExprs)
      AddRegisteredQuery qname relExpr ->
        AddRegisteredQuery qname relExpr
      RemoveRegisteredQuery qname ->
        RemoveRegisteredQuery qname
      AlterACL aclExpr ->
        AlterACL aclExpr
      MultipleExpr exprs ->
        MultipleExpr (map (normalizeTransactionMarker marker) exprs)

instance NormalizableTransactionMarker (DatabaseContextIOExprBase ()) where
  type Args (DatabaseContextIOExprBase ()) =
    GraphRefTransactionMarker
  type Ret (DatabaseContextIOExprBase ()) =
    DatabaseContextIOExprBase GraphRefTransactionMarker

  normalizeTransactionMarker marker expr = do
    let norm :: forall x. (NormalizableTransactionMarker x, Args x ~ GraphRefTransactionMarker) => x -> Ret x
        norm = normalizeTransactionMarker marker
    case expr of
      AddAtomFunction fname tconss body ->
        AddAtomFunction fname tconss body
      LoadAtomFunctions nam fname path ->
        LoadAtomFunctions nam fname path
      AddDatabaseContextFunction fname tconss body ->
        AddDatabaseContextFunction fname tconss body
      LoadDatabaseContextFunctions mname fname path ->
        LoadDatabaseContextFunctions mname fname path
      LoadModuleWithFunctions body ->
        LoadModuleWithFunctions body
      CreateArbitraryRelation rv attrExprs range ->
        CreateArbitraryRelation rv (map norm attrExprs) range
                      
instance NormalizableTransactionMarker (AttributeNameAtomExprMapBase (AttributeNamesExprBase ()) ()) where
  type Args (AttributeNameAtomExprMapBase (AttributeNamesExprBase ()) ()) =
    GraphRefTransactionMarker
  type Ret (AttributeNameAtomExprMapBase (AttributeNamesExprBase ()) ()) =
    AttributeNameAtomExprMapBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
    
  normalizeTransactionMarker marker attrAtomMap = fmap (normalizeTransactionMarker marker) attrAtomMap

instance NormalizableTransactionMarker (AttributeExprBase ()) where
  type Args (AttributeExprBase ()) = GraphRefTransactionMarker
  type Ret (AttributeExprBase ()) = AttributeExprBase GraphRefTransactionMarker
  
  normalizeTransactionMarker marker expr =
    case expr of
      AttributeAndTypeNameExpr aname tCons () ->
        AttributeAndTypeNameExpr aname tCons marker
      NakedAttributeExpr aname ->
        NakedAttributeExpr aname

instance NormalizableTransactionMarker (WithNameExprBase ()) where
  type Args (WithNameExprBase ()) =
    GraphRefTransactionMarker
  type Ret (WithNameExprBase ()) =
    WithNameExprBase GraphRefTransactionMarker

  normalizeTransactionMarker marker (WithNameExpr rvName ()) =
    WithNameExpr rvName marker


instance NormalizableTransactionMarker (RestrictionPredicateExprBase AttributeNames ()) where
  type Args (RestrictionPredicateExprBase AttributeNames ()) =
    GraphRefTransactionMarker
  type Ret (RestrictionPredicateExprBase AttributeNames ()) =
    RestrictionPredicateExprBase AttributeNames GraphRefTransactionMarker
  normalizeTransactionMarker marker expr =
    fmap (const marker) expr


-- attributes already normalized
instance NormalizableTransactionMarker (RelationalExprBase AttributeNames ()) where
  type Args (RelationalExprBase AttributeNames ()) =
    GraphRefTransactionMarker
  type Ret (RelationalExprBase AttributeNames ()) =
    RelationalExprBase AttributeNames GraphRefTransactionMarker
    
  normalizeTransactionMarker marker expr = fmap (const marker) expr

instance NormalizableTransactionMarker (DatabaseContextExprBase AttributeNames () r) where
  type Args (DatabaseContextExprBase AttributeNames () r) =
    GraphRefTransactionMarker
  type Ret (DatabaseContextExprBase AttributeNames () r) =
    DatabaseContextExprBase AttributeNames GraphRefTransactionMarker r

  normalizeTransactionMarker marker expr =
    let norm :: forall x. (NormalizableTransactionMarker x, Args x ~ GraphRefTransactionMarker) => x -> Ret x
        norm = normalizeTransactionMarker marker
    in
    case expr of
      NoOperation -> NoOperation
      Define nam attrExprs ->
        Define nam (map norm attrExprs)
      Undefine nam ->
        Undefine nam
      Assign nam rexpr ->
        Assign nam (norm rexpr)
      Insert nam rexpr ->
        Insert nam (norm rexpr)
      Delete nam pred' ->
        Delete nam (norm pred')
      Update nam attrMap pred' ->
        Update nam (norm attrMap) (norm pred')
      AddInclusionDependency nam dep ->
        AddInclusionDependency nam dep
      RemoveInclusionDependency nam ->
        RemoveInclusionDependency nam
      AddNotification nam exprA exprB exprC ->
        AddNotification nam exprA exprB exprC
      RemoveNotification nam ->
        RemoveNotification nam
      AddTypeConstructor tyDef consDefs ->
        AddTypeConstructor tyDef consDefs
      RemoveTypeConstructor tyName ->
        RemoveTypeConstructor tyName
      RemoveAtomFunction fname ->
        RemoveAtomFunction fname
      RemoveDatabaseContextFunction fname ->
        RemoveDatabaseContextFunction fname
      ExecuteDatabaseContextFunction fname args ->
        ExecuteDatabaseContextFunction fname (map (normalizeTransactionMarker marker) args)
      AddRegisteredQuery qname expr' ->
        AddRegisteredQuery qname expr'
      RemoveRegisteredQuery qname ->
        RemoveRegisteredQuery qname
      AlterACL aclExpr ->
        AlterACL aclExpr
      MultipleExpr exprs ->
        MultipleExpr (map (normalizeTransactionMarker marker) exprs)

instance NormalizableTransactionMarker (AttributeNameAtomExprMapBase AttributeNames ()) where
  type Args (AttributeNameAtomExprMapBase AttributeNames ()) =
    GraphRefTransactionMarker
  type Ret (AttributeNameAtomExprMapBase AttributeNames ()) =
    AttributeNameAtomExprMapBase AttributeNames GraphRefTransactionMarker

  normalizeTransactionMarker marker map' = M.map (normalizeTransactionMarker marker) map'

instance NormalizableTransactionMarker (AtomExprBase AttributeNames ()) where
  type Args (AtomExprBase AttributeNames ()) =
    GraphRefTransactionMarker
  type Ret (AtomExprBase AttributeNames ()) =
    AtomExprBase AttributeNames GraphRefTransactionMarker

  normalizeTransactionMarker marker expr =
    case expr of
      AttributeAtomExpr aname ->
        AttributeAtomExpr aname
      SubrelationAttributeAtomExpr aName1 aName2 ->
        SubrelationAttributeAtomExpr aName1 aName2
      NakedAtomExpr atom ->
        NakedAtomExpr atom 
      FunctionAtomExpr fname args () ->
        FunctionAtomExpr fname (map (normalizeTransactionMarker marker) args) marker
      RelationAtomExpr relExpr ->
        RelationAtomExpr (normalizeTransactionMarker marker relExpr)
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr (normalizeTransactionMarker marker ifE) (normalizeTransactionMarker marker thenE) (normalizeTransactionMarker marker elseE)
      ConstructedAtomExpr dConsName args () ->
        ConstructedAtomExpr dConsName (map (normalizeTransactionMarker marker) args) marker


instance NormalizableTransactionMarker (InclusionDependencyBase (AttributeNamesExprBase ()) ()) where
  type Args (InclusionDependencyBase (AttributeNamesExprBase ()) ()) =             
    GraphRefTransactionMarker
  type Ret (InclusionDependencyBase (AttributeNamesExprBase ()) ()) =              
    InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) ()   
                                                                                
  normalizeTransactionMarker marker (InclusionDependency a b) =                 
    InclusionDependency (fmap (const ()) (normalizeTransactionMarker marker a))
                        (fmap (const ()) (normalizeTransactionMarker marker b))

{-
instance NormalizableTransactionMarker (InclusionDependencyBase (AttributeNamesExprBase a)) where
  type Args (InclusionDependencyBase (AttributeNamesExprBase a)) =
    a
  type Ret (InclusionDependencyBase (AttributeNamesExprBase a)) =
    InclusionDependencyBase (AttributeNamesExprBase a)
    
  normalizeTransactionMarker marker (InclusionDependency a b) =
    InclusionDependency (normalizeTransactionMarker marker a) (normalizeTransactionMarker marker b)
-}
--instance NormalizableTransactionMarker (InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) where                                       

instance NormalizableTransactionMarker TransGraphRelationalExpr where
  type Args TransGraphRelationalExpr =
    (TransactionGraph, GraphRefTransactionMarker)
  type Ret TransGraphRelationalExpr = Either RelationalError (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker)

  normalizeTransactionMarker (graph, marker) expr = do
    let norm :: forall x. (NormalizableTransactionMarker x, Args x ~ (TransactionGraph, GraphRefTransactionMarker)) => x -> Ret x
        norm = normalizeTransactionMarker (graph, marker)
    case expr of
      MakeRelationFromExprs mAttrExprs tupleExprs ->
        MakeRelationFromExprs <$> mapM (mapM norm) mAttrExprs <*> norm tupleExprs
      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)
      ExistingRelation rel ->
        pure (ExistingRelation rel)
      RelationValuedAttribute attrName ->
        pure (RelationValuedAttribute attrName)
      RelationVariable rvname transLookup -> 
        RelationVariable rvname <$> findTransId graph transLookup
      Project transAttrNames expr' ->
        Project <$> norm transAttrNames <*> norm expr'
      Union exprA exprB ->
        Union <$> norm exprA <*> norm exprB
      Join exprA exprB ->
        Join <$> norm exprA <*> norm exprB
      Rename attrs expr' ->
        Rename attrs <$> norm expr'
      Difference exprA exprB ->
        Difference <$> norm exprA <*> norm exprB
      Group transAttrNames attrName expr' ->
        Group 
        <$> norm transAttrNames 
        <*> pure attrName
        <*> norm expr'
      Ungroup attrName expr' -> 
        Ungroup attrName <$> norm expr'
      Restrict predicateExpr expr' ->
        Restrict <$> norm predicateExpr <*> norm expr'
      Equals exprA exprB ->
        Equals <$> norm exprA <*> norm exprB
      NotEquals exprA exprB ->  
        NotEquals <$> norm exprA <*> norm exprB
      Extend extendExpr expr' -> 
        Extend <$> norm extendExpr <*> norm expr'
      With views expr' -> do
        evaldViews <- mapM (\(wnexpr, vexpr) ->
                         (,) <$> norm wnexpr <*> norm vexpr
                     ) views
        With evaldViews <$> norm expr'

instance NormalizableTransactionMarker TransGraphAttributeExpr where
  type Args TransGraphAttributeExpr =
    (TransactionGraph, GraphRefTransactionMarker)
  type Ret TransGraphAttributeExpr =
    Either RelationalError (AttributeExprBase GraphRefTransactionMarker)
    
  normalizeTransactionMarker (graph, _marker) expr =
    case expr of
      AttributeAndTypeNameExpr aname tcons tLookup ->
        AttributeAndTypeNameExpr aname tcons <$> findTransId graph tLookup
      NakedAttributeExpr attr ->
        pure (NakedAttributeExpr attr)

instance NormalizableTransactionMarker TransGraphTupleExprs where
  type Args TransGraphTupleExprs =
    (TransactionGraph, GraphRefTransactionMarker)
  type Ret TransGraphTupleExprs =
    Either RelationalError (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker)
  normalizeTransactionMarker (graph, marker) (TupleExprs tLookup tupExprs) =
    TupleExprs <$> findTransId graph tLookup <*> mapM (normalizeTransactionMarker (graph, marker) ) tupExprs

instance NormalizableTransactionMarker TransGraphAttributeNamesExpr where
  type Args TransGraphAttributeNamesExpr =
    (TransactionGraph, GraphRefTransactionMarker)
  type Ret TransGraphAttributeNamesExpr =
    Either RelationalError (AttributeNamesExprBase GraphRefTransactionMarker)
    
  normalizeTransactionMarker (graph, marker) expr =
    case expr of
      AttributeNames anames ->
        pure (AttributeNames anames)
      InvertedAttributeNames anames ->
        pure (InvertedAttributeNames anames)
      UnionAttributeNames a b ->
        UnionAttributeNames <$> normalizeTransactionMarker (graph, marker) a <*> normalizeTransactionMarker (graph, marker) b
      IntersectAttributeNames a b ->
        IntersectAttributeNames <$> normalizeTransactionMarker (graph, marker) a <*> normalizeTransactionMarker (graph, marker) b
      RelationalExprAttributeNames relExpr ->
        RelationalExprAttributeNames <$> normalizeTransactionMarker (graph, marker) relExpr

instance NormalizableTransactionMarker TransGraphRestrictionPredicateExpr where
  type Args TransGraphRestrictionPredicateExpr =
    (TransactionGraph, GraphRefTransactionMarker)
  type Ret TransGraphRestrictionPredicateExpr =
    Either RelationalError (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker)

  normalizeTransactionMarker (graph, marker) expr = do
    let norm :: forall x.
          (NormalizableTransactionMarker x, Args x ~ (TransactionGraph, GraphRefTransactionMarker)) =>
          x -> Ret x
        norm = normalizeTransactionMarker (graph, marker)
    case expr of
      TruePredicate ->
        pure TruePredicate
      AndPredicate a b ->
        AndPredicate <$> norm a <*> norm b
      OrPredicate a b ->
        OrPredicate <$> norm a <*> norm b
      NotPredicate a ->
        NotPredicate <$> norm a
      RelationalExprPredicate relExpr ->
        RelationalExprPredicate <$> norm relExpr
      AtomExprPredicate atomExpr ->
        AtomExprPredicate <$> norm atomExpr
      AttributeEqualityPredicate aname atomExpr ->
        AttributeEqualityPredicate aname <$> norm atomExpr

instance NormalizableTransactionMarker TransGraphExtendTupleExpr where
  type Args TransGraphExtendTupleExpr =
    (TransactionGraph, GraphRefTransactionMarker)
  type Ret TransGraphExtendTupleExpr =
    Either RelationalError (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker)
    
  normalizeTransactionMarker (graph, marker) (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalizeTransactionMarker (graph, marker) atomExpr

instance NormalizableTransactionMarker TransGraphWithNameExpr where
  type Args TransGraphWithNameExpr =
    (TransactionGraph, GraphRefTransactionMarker)
  type Ret TransGraphWithNameExpr =
    Either RelationalError (WithNameExprBase GraphRefTransactionMarker)
  
  normalizeTransactionMarker (graph, _marker) (WithNameExpr rv tLookup) =
    WithNameExpr rv <$> findTransId graph tLookup

instance NormalizableTransactionMarker TransGraphTupleExpr where
  type Args TransGraphTupleExpr =
    (TransactionGraph, GraphRefTransactionMarker)
  type Ret TransGraphTupleExpr =
    Either RelationalError (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker)
  normalizeTransactionMarker (graph, marker) (TupleExpr m) =
    TupleExpr <$> mapM (normalizeTransactionMarker (graph, marker)) m

instance NormalizableTransactionMarker TransGraphAtomExpr where
  type Args TransGraphAtomExpr =
    (TransactionGraph, GraphRefTransactionMarker)
  type Ret TransGraphAtomExpr =
    Either RelationalError (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker)

  normalizeTransactionMarker m@(_graph, marker) expr =
    case expr of
      AttributeAtomExpr aname ->
        pure (AttributeAtomExpr aname)
      SubrelationAttributeAtomExpr aname aname2 ->
        pure (SubrelationAttributeAtomExpr aname aname2)
      NakedAtomExpr atom ->
        pure (NakedAtomExpr atom)
      FunctionAtomExpr fname atomExprs _tlookup ->
        FunctionAtomExpr fname <$> mapM (normalizeTransactionMarker m) atomExprs <*> pure marker
      RelationAtomExpr relExpr ->
        RelationAtomExpr <$> normalizeTransactionMarker m relExpr
      IfThenAtomExpr a b c ->
        IfThenAtomExpr <$>
        normalizeTransactionMarker m a <*>
        normalizeTransactionMarker m b <*>
        normalizeTransactionMarker m c
      ConstructedAtomExpr dConsName atomExprs _tLookup ->
        ConstructedAtomExpr dConsName <$> mapM (normalizeTransactionMarker m) atomExprs <*> pure marker


findTransId :: TransactionGraph -> TransactionIdLookup -> Either RelationalError GraphRefTransactionMarker
findTransId graph tlook = TransactionMarker . transactionId <$> lookupTransaction graph tlook


transGraphNormalizeM ::
  (
    NormalizableTransactionMarker x,
    Args x ~ (TransactionGraph, GraphRefTransactionMarker),
    Ret x ~ Either RelationalError y,
    NormalizableAttributeNames y
  ) =>
  (TransactionGraph, GraphRefTransactionMarker) ->
  x ->
  GraphRefRelationalExprM (NormalizeAttributeNames y)
transGraphNormalizeM graphAndMarker expr =
  case normalizeTransactionMarker graphAndMarker expr of
    Left err -> throwError err
    Right norm1 ->
      normalizeAttributeNames norm1

normalizeE :: 
  (
   NormalizableTransactionMarker x,
   Ret x ~ Either RelationalError y,
   NormalizableAttributeNames y
   ) =>
  Args x ->
  x ->
  GraphRefRelationalExprM (NormalizeAttributeNames y)
normalizeE arg expr = do
  case normalizeTransactionMarker arg expr of
    Left err -> throwError err
    Right norm1 ->
      normalizeAttributeNames norm1

normalizeM ::
  ( 
   NormalizableTransactionMarker x,
   NormalizableAttributeNames y,
   Ret x ~ y,
   Args x ~ GraphRefTransactionMarker
   ) =>
 x ->
 GraphRefRelationalExprM (NormalizeAttributeNames (Ret x))
normalizeM expr =
  let norm1 = normalizeTransactionMarker UncommittedContextMarker expr
  in
    normalizeAttributeNames norm1

normalize ::
  (
   NormalizableTransactionMarker x,
   NormalizableAttributeNames (Ret x),
   Args x ~ GraphRefTransactionMarker
   ) =>
  Maybe DatabaseContext ->
  TransactionGraph ->
  x ->
  Either RelationalError (NormalizeAttributeNames (Ret x))
normalize mctx tgraph expr =
  let norm1 = normalizeTransactionMarker UncommittedContextMarker expr
      gfEnv = freshGraphRefRelationalExprEnv mctx tgraph
  in
    runGraphRefRelationalExprM gfEnv (normalizeAttributeNames norm1)

normalize' ::
  (
   NormalizableTransactionMarker x,
   NormalizableAttributeNames (Ret x),
   Args x ~ GraphRefTransactionMarker
   ) =>
  DatabaseContext ->
  TransactionGraph ->
  x ->
  Either RelationalError (NormalizeAttributeNames (Ret x))
normalize' dbctx = normalize (Just dbctx)

normalizeAttributeNames' ::
  (NormalizableAttributeNames x
  )
  => x -> GraphRefRelationalExprM (NormalizeAttributeNames x)
normalizeAttributeNames' expr = normalizeAttributeNames expr

normalizeTransactionMarker' ::
  (NormalizableTransactionMarker x,
   Args x ~ GraphRefTransactionMarker) =>
  x -> Ret x
normalizeTransactionMarker' expr =
  normalizeTransactionMarker UncommittedContextMarker expr
