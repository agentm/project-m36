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
import Control.Monad.Reader (local, asks)
import Control.Monad.State (get)
import Data.Kind (Type)

class NormalizableAttributeNames a where
  -- | convert AttributeNamesExpr -> AttributeNames (removing expressions to calculate attribute names)
  type AttributeNamesResult a :: Type
  type AttributeNamesM a :: Type -> Type

  normalizeAttributeNames :: a -> AttributeNamesM a (AttributeNamesResult a)

instance NormalizableAttributeNames (RelationalExprBase AttributeNames a) where
  type AttributeNamesResult (RelationalExprBase AttributeNames a) =
    RelationalExprBase AttributeNames a
  type AttributeNamesM (RelationalExprBase AttributeNames a) =
    GraphRefRelationalExprM
    
  normalizeAttributeNames expr = pure expr

instance NormalizableAttributeNames (RelationalExprBase (AttributeNamesExprBase ()) ()) where
  type AttributeNamesResult (RelationalExprBase (AttributeNamesExprBase ()) ()) =
    RelationalExprBase AttributeNames ()
  type AttributeNamesM (RelationalExprBase (AttributeNamesExprBase ()) ()) =
    GraphRefRelationalExprM
    
  normalizeAttributeNames expr = do
    case expr of
      MakeRelationFromExprs attrExprs tupleExprs -> 
        MakeRelationFromExprs attrExprs <$> normalizeAttributeNames tupleExprs
      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)
      ExistingRelation rel ->
        pure (ExistingRelation rel)
      RelationVariable nam x ->
        pure (RelationVariable nam x)
      RelationValuedAttribute nam ->
        pure (RelationValuedAttribute nam)
      Project attrExprs expr' -> do
        normExpr' <- normalizeAttributeNames expr'
        exprType' <- typeForGraphRefRelationalExpr (fmap (const UncommittedContextMarker) normExpr')
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')
        Project <$> local env (normalizeAttributeNames attrExprs) <*> pure normExpr'
      Union exprA exprB ->
        Union <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Join exprA exprB ->
        Join <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Rename renameAssocs expr' ->
        Rename renameAssocs <$> normalizeAttributeNames expr'
      Difference exprA exprB ->
        Difference <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Group attrNameExprs gname expr' -> do
        Group <$> normalizeAttributeNames attrNameExprs <*> pure gname <*> normalizeAttributeNames expr'
      Ungroup ungname expr' ->
        Ungroup ungname <$> normalizeAttributeNames expr'
      Restrict rpredExpr expr' ->
        Restrict <$> normalizeAttributeNames rpredExpr <*> normalizeAttributeNames expr'
      Equals exprA exprB ->
        Equals <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      NotEquals exprA exprB ->
        NotEquals <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Extend extendTuplesExpr expr' ->
        Extend <$> normalizeAttributeNames extendTuplesExpr <*> normalizeAttributeNames expr'
      With withNamesAssocs expr' ->
        With <$> normalizeAttributeNames withNamesAssocs <*> normalizeAttributeNames expr'

instance NormalizableAttributeNames (TupleExprsBase (AttributeNamesExprBase ()) ()) where
  type AttributeNamesResult (TupleExprsBase (AttributeNamesExprBase ()) ()) =
    TupleExprsBase AttributeNames ()
  type AttributeNamesM (TupleExprsBase (AttributeNamesExprBase ()) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (TupleExprs a tups) = TupleExprs a <$> mapM normalizeAttributeNames tups

instance NormalizableAttributeNames (TupleExprBase (AttributeNamesExprBase ()) ()) where
  type AttributeNamesResult (TupleExprBase (AttributeNamesExprBase ()) ()) =
    TupleExprBase AttributeNames ()
  type AttributeNamesM (TupleExprBase (AttributeNamesExprBase ()) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (TupleExpr tupExprMap) = do
    let mapper (k,v) = do
          v' <- normalizeAttributeNames v
          pure (k,v')
    TupleExpr . M.fromList <$> mapM mapper (M.toList tupExprMap)

instance NormalizableAttributeNames (AttributeNamesExprBase ()) where
  type AttributeNamesResult (AttributeNamesExprBase ()) =
    AttributeNames
  type AttributeNamesM (AttributeNamesExprBase ()) =
    GraphRefRelationalExprM

  normalizeAttributeNames expr = do
    let normExpr = normalizeMarker expr
    evalGraphRefAttributeNamesExpr normalizeAttributeNames normExpr

instance NormalizableAttributeNames (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) where
  type AttributeNamesResult (ExtendTupleExprBase (AttributeNamesExprBase()) ()) =
    ExtendTupleExprBase AttributeNames ()
  type AttributeNamesM (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalizeAttributeNames atomExpr

instance NormalizableAttributeNames (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) where
  type AttributeNamesResult (WithNamesAssocsBase (AttributeNamesExprBase()) ()) =
    WithNamesAssocsBase AttributeNames ()
  type AttributeNamesM (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames assocs = do
    let mapper (WithNameExpr rv tm,
                relExpr) = (,) <$> pure (WithNameExpr rv tm) <*> normalizeAttributeNames relExpr
    mapM mapper assocs

---

instance NormalizableAttributeNames (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  type AttributeNamesResult (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    RelationalExprBase AttributeNames GraphRefTransactionMarker
  type AttributeNamesM (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    GraphRefRelationalExprM

  normalizeAttributeNames expr = do
    case expr of
      MakeRelationFromExprs attrExprs tupleExprs -> 
        MakeRelationFromExprs attrExprs <$> normalizeAttributeNames tupleExprs
      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)
      ExistingRelation rel ->
        pure (ExistingRelation rel)
      RelationVariable nam x ->
        pure (RelationVariable nam x)
      RelationValuedAttribute nam ->
        pure (RelationValuedAttribute nam)
      Project attrExprs expr' -> do
        normExpr <- normalizeAttributeNames expr'
        exprType' <- typeForGraphRefRelationalExpr normExpr
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')
        Project <$> local env (normalizeAttributeNames attrExprs) <*> pure normExpr
      Union exprA exprB ->
        Union <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Join exprA exprB ->
        Join <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Rename renameAssocs expr' ->
        Rename renameAssocs <$> normalizeAttributeNames expr'
      Difference exprA exprB ->
        Difference <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Group attrNameExprs gname expr' -> do
        normExpr <- normalizeAttributeNames expr'
        exprType' <- typeForGraphRefRelationalExpr normExpr
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')
        Group <$> local env (normalizeAttributeNames attrNameExprs) <*> pure gname <*> pure normExpr
      Ungroup ungname expr' ->
        Ungroup ungname <$> normalizeAttributeNames expr'
      Restrict rpredExpr expr' -> do
        normExpr <- normalizeAttributeNames expr'
        exprType' <- typeForGraphRefRelationalExpr normExpr
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')       
        Restrict <$> local env (normalizeAttributeNames rpredExpr) <*> pure normExpr
      Equals exprA exprB ->
        Equals <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      NotEquals exprA exprB ->
        NotEquals <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Extend extendTuplesExpr expr' ->
        Extend <$> normalizeAttributeNames extendTuplesExpr <*> normalizeAttributeNames expr'
      With withNamesAssocs expr' ->
        With <$> normalizeAttributeNames withNamesAssocs <*> normalizeAttributeNames expr'

instance NormalizableAttributeNames (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  type AttributeNamesResult (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    TupleExprsBase AttributeNames GraphRefTransactionMarker
  type AttributeNamesM (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    GraphRefRelationalExprM

  normalizeAttributeNames (TupleExprs a tups) = TupleExprs a <$> mapM normalizeAttributeNames tups

instance NormalizableAttributeNames (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  type AttributeNamesResult (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    TupleExprBase AttributeNames GraphRefTransactionMarker
  type AttributeNamesM (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (TupleExpr tupExprMap) = do
    let mapper (k,v) = do
          (,) k <$> normalizeAttributeNames v
    TupleExpr . M.fromList <$> mapM mapper (M.toList tupExprMap)

instance NormalizableAttributeNames (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  type AttributeNamesResult (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    RestrictionPredicateExprBase AttributeNames GraphRefTransactionMarker
  type AttributeNamesM (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    GraphRefRelationalExprM

  normalizeAttributeNames expr = do
    case expr of
      TruePredicate ->
        pure TruePredicate
      AndPredicate a b ->
        AndPredicate <$> normalizeAttributeNames a <*> normalizeAttributeNames b
      OrPredicate a b ->
        OrPredicate <$> normalizeAttributeNames a <*> normalizeAttributeNames b
      NotPredicate a ->
        NotPredicate <$> normalizeAttributeNames a
      RelationalExprPredicate relExpr' ->
        RelationalExprPredicate <$> normalizeAttributeNames relExpr'
      AtomExprPredicate atomExpr ->
        AtomExprPredicate <$> normalizeAttributeNames atomExpr
      AttributeEqualityPredicate nam atomExpr ->
        AttributeEqualityPredicate nam <$> normalizeAttributeNames atomExpr

instance NormalizableAttributeNames (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  type AttributeNamesResult (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    AtomExprBase AttributeNames GraphRefTransactionMarker
  type AttributeNamesM (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames expr = do
    case expr of
      AttributeAtomExpr nam ->
        pure $ AttributeAtomExpr nam
      SubrelationAttributeAtomExpr relAttr subAttr ->
        pure (SubrelationAttributeAtomExpr relAttr subAttr)
      NakedAtomExpr atom ->
        pure $ NakedAtomExpr atom
      FunctionAtomExpr fName atomExprs a ->
        FunctionAtomExpr fName <$> mapM normalizeAttributeNames atomExprs  <*> pure a
      RelationAtomExpr expr' ->
        RelationAtomExpr <$> normalizeAttributeNames expr'
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr <$> normalizeAttributeNames ifE <*> normalizeAttributeNames thenE <*> normalizeAttributeNames elseE
      ConstructedAtomExpr dConsName atomExprs a ->
        ConstructedAtomExpr dConsName <$> mapM normalizeAttributeNames atomExprs <*> pure a

instance NormalizableAttributeNames (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  type AttributeNamesResult (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    ExtendTupleExprBase AttributeNames GraphRefTransactionMarker
  type AttributeNamesM (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalizeAttributeNames atomExpr

instance NormalizableAttributeNames (WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  type AttributeNamesResult (WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    WithNamesAssocsBase AttributeNames GraphRefTransactionMarker
  type AttributeNamesM (WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames assocs = do
    let mapper (WithNameExpr rv tm,
                relExpr') = (,) <$> pure (WithNameExpr rv tm) <*> normalizeAttributeNames relExpr'
    mapM mapper assocs

instance NormalizableAttributeNames (AttributeNamesExprBase GraphRefTransactionMarker) where
  type AttributeNamesResult (AttributeNamesExprBase GraphRefTransactionMarker) =
    AttributeNames
  type AttributeNamesM (AttributeNamesExprBase GraphRefTransactionMarker) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames expr = do
    let normalizeRelationalExpr = normalizeAttributeNames
    evalGraphRefAttributeNamesExpr normalizeRelationalExpr expr 

instance NormalizableAttributeNames (DatabaseContextExprBase (AttributeNamesExprBase ()) () r) where
  type AttributeNamesResult (DatabaseContextExprBase (AttributeNamesExprBase ()) () r) =
    DatabaseContextExprBase AttributeNames () r
  type AttributeNamesM (DatabaseContextExprBase (AttributeNamesExprBase ()) () r) =
    DatabaseContextEvalMonad
  
  normalizeAttributeNames expr = do
    graph <- dbcGraph
    context <- getStateContext
    let gfEnv = freshGraphRefRelationalExprEnv (Just context) graph
        normGfExpr expr = 
          case runGraphRefRelationalExprM gfEnv (normalizeAttributeNames expr) of
            Left err -> throwError err
            Right val -> pure val
        normPredExpr expr =
          case runGraphRefRelationalExprM gfEnv (normalizeAttributeNames expr) of
            Left err -> throwError err
            Right val -> pure val
        normAtomExpr expr =
          case runGraphRefRelationalExprM gfEnv (normalizeAttributeNames expr) of
            Left err -> throwError err
            Right val -> pure val
            
    case expr of
      NoOperation -> pure NoOperation
      Define nam attrExprs -> pure (Define nam attrExprs)
      Undefine nam -> pure (Undefine nam)
      Assign nam rexpr -> Assign nam <$> normGfExpr rexpr
      Insert nam rexpr -> Insert nam <$> normGfExpr rexpr
      Delete nam pred' -> Delete nam <$> normPredExpr pred'
      Update nam attrMap pred' -> Update nam <$> normalizeAttributeNames attrMap <*> normPredExpr pred'

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
        ExecuteDatabaseContextFunction funcName' <$> mapM normAtomExpr atomExprs
      AddRegisteredQuery n q -> pure (AddRegisteredQuery n q)
      RemoveRegisteredQuery n -> pure (RemoveRegisteredQuery n)
      AlterACL e -> pure (AlterACL e)
      MultipleExpr exprs -> MultipleExpr <$> mapM normalizeAttributeNames exprs      

instance NormalizableAttributeNames (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) where
  type AttributeNamesResult (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) =
    RestrictionPredicateExprBase AttributeNames ()
  type AttributeNamesM (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames expr = do
    case expr of
      TruePredicate ->
        pure TruePredicate
      AndPredicate a b ->
        AndPredicate <$> normalizeAttributeNames a <*> normalizeAttributeNames b
      OrPredicate a b ->
        OrPredicate <$> normalizeAttributeNames a <*> normalizeAttributeNames b
      NotPredicate a ->
        NotPredicate <$> normalizeAttributeNames a
      RelationalExprPredicate relExpr' ->
        RelationalExprPredicate <$> normalizeAttributeNames relExpr'
      AtomExprPredicate atomExpr ->
        AtomExprPredicate <$> normalizeAttributeNames atomExpr
      AttributeEqualityPredicate nam atomExpr ->
        AttributeEqualityPredicate nam <$> normalizeAttributeNames atomExpr

instance NormalizableAttributeNames (AtomExprBase (AttributeNamesExprBase ()) ()) where
  type AttributeNamesResult (AtomExprBase (AttributeNamesExprBase ()) ()) =
    AtomExprBase AttributeNames ()
  type AttributeNamesM (AtomExprBase (AttributeNamesExprBase ()) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames expr = do
    case expr of
      AttributeAtomExpr nam ->
        pure $ AttributeAtomExpr nam
      SubrelationAttributeAtomExpr relAttr subAttr ->
        pure (SubrelationAttributeAtomExpr relAttr subAttr)
      NakedAtomExpr atom ->
        pure $ NakedAtomExpr atom
      FunctionAtomExpr fName atomExprs a ->
        FunctionAtomExpr fName <$> mapM normalizeAttributeNames atomExprs  <*> pure a
      RelationAtomExpr expr' ->
        RelationAtomExpr <$> normalizeAttributeNames expr'
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr <$> normalizeAttributeNames ifE <*> normalizeAttributeNames thenE <*> normalizeAttributeNames elseE
      ConstructedAtomExpr dConsName atomExprs a ->
        ConstructedAtomExpr dConsName <$> mapM normalizeAttributeNames atomExprs <*> pure a

instance NormalizableAttributeNames (AttributeNameAtomExprMapBase (AttributeNamesExprBase ()) ()) where
  type AttributeNamesResult (AttributeNameAtomExprMapBase (AttributeNamesExprBase ()) ()) =
    AttributeNameAtomExprMapBase AttributeNames ()
  type AttributeNamesM (AttributeNameAtomExprMapBase (AttributeNamesExprBase ()) ()) =
    DatabaseContextEvalMonad
  
  normalizeAttributeNames map' = do
    graph <- dbcGraph
    context <- getStateContext
    let gfEnv = freshGraphRefRelationalExprEnv (Just context) graph    
        normAtomExpr expr = 
          case runGraphRefRelationalExprM gfEnv (normalizeAttributeNames expr) of
            Left err -> throwError err
            Right val -> pure val
        mapper (k,v) = (,) <$> pure k <*> normAtomExpr v
    M.fromList <$> mapM mapper (M.toList map')

---

instance NormalizableAttributeNames (DatabaseContextExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker r) where
  type AttributeNamesResult (DatabaseContextExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker r) =
    DatabaseContextExprBase AttributeNames GraphRefTransactionMarker r
  type AttributeNamesM (DatabaseContextExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker r) =
    DatabaseContextEvalMonad
  
  normalizeAttributeNames expr = do
    graph <- dbcGraph
    context <- getStateContext
    let gfEnv = freshGraphRefRelationalExprEnv (Just context) graph
        normGfExpr expr = 
          case runGraphRefRelationalExprM gfEnv (normalizeAttributeNames expr) of
            Left err -> throwError err
            Right val -> pure val
        normPredExpr expr =
          case runGraphRefRelationalExprM gfEnv (normalizeAttributeNames expr) of
            Left err -> throwError err
            Right val -> pure val
        normAtomExpr expr =
          case runGraphRefRelationalExprM gfEnv (normalizeAttributeNames expr) of
            Left err -> throwError err
            Right val -> pure val
    case expr of
      NoOperation -> pure NoOperation
      Define nam attrExprs -> pure (Define nam attrExprs)
      Undefine nam -> pure (Undefine nam)
      Assign nam rexpr -> Assign nam <$> normGfExpr rexpr
      Insert nam rexpr -> Insert nam <$> normGfExpr rexpr
      Delete nam pred' -> Delete nam <$> normPredExpr pred'
      Update nam attrMap pred' -> Update nam <$> normalizeAttributeNames attrMap <*> normPredExpr pred'

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
        ExecuteDatabaseContextFunction funcName' <$> mapM normAtomExpr atomExprs
      AddRegisteredQuery n q -> pure (AddRegisteredQuery n q)
      RemoveRegisteredQuery n -> pure (RemoveRegisteredQuery n)
      AlterACL e -> pure (AlterACL e)
      MultipleExpr exprs -> MultipleExpr <$> mapM normalizeAttributeNames exprs

instance NormalizableAttributeNames (AttributeNameAtomExprMapBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  type AttributeNamesResult (AttributeNameAtomExprMapBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    AttributeNameAtomExprMapBase AttributeNames GraphRefTransactionMarker
  type AttributeNamesM (AttributeNameAtomExprMapBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) =
    DatabaseContextEvalMonad
  
  normalizeAttributeNames map' = do
    graph <- dbcGraph
    context <- getStateContext
    let gfEnv = freshGraphRefRelationalExprEnv (Just context) graph    
        normAtomExpr expr = 
          case runGraphRefRelationalExprM gfEnv (normalizeAttributeNames expr) of
            Left err -> throwError err
            Right val -> pure val
        mapper (k,v) = (,) <$> pure k <*> normAtomExpr v
    M.fromList <$> mapM mapper (M.toList map')



instance NormalizableAttributeNames (InclusionDependencyBase (AttributeNamesExprBase ()) ()) where
  type AttributeNamesResult (InclusionDependencyBase (AttributeNamesExprBase ()) ()) =
    InclusionDependencyBase AttributeNames ()
  type AttributeNamesM (InclusionDependencyBase (AttributeNamesExprBase ()) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (InclusionDependency relExprA relExprB) = do
    InclusionDependency <$> normalizeAttributeNames relExprA <*> normalizeAttributeNames relExprB

instance NormalizableAttributeNames (InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  type AttributeNamesResult (InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    InclusionDependencyBase AttributeNames ()
  type AttributeNamesM (InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (InclusionDependency relExprA relExprB) = do
    InclusionDependency <$> normalizeAttributeNames relExprA <*> normalizeAttributeNames relExprB

instance NormalizableAttributeNames (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  type AttributeNamesResult (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    RelationalExprBase AttributeNames ()
  type AttributeNamesM (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames expr = 
    case expr of
      MakeRelationFromExprs attrExprs tupleExprs -> 
        MakeRelationFromExprs attrExprs <$> normalizeAttributeNames tupleExprs
      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)
      ExistingRelation rel ->
        pure (ExistingRelation rel)
      RelationVariable nam x ->
        pure (RelationVariable nam x)
      RelationValuedAttribute nam ->
        pure (RelationValuedAttribute nam)
      Project attrExprs expr' -> do
        normExpr <- normalizeAttributeNames expr'
        exprType' <- typeForGraphRefRelationalExpr (fmap (const UncommittedContextMarker) normExpr)
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')       
        Project <$> local env (normalizeAttributeNames attrExprs) <*> pure normExpr
      Union exprA exprB ->
        Union <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Join exprA exprB ->
        Join <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Rename renameAssocs expr' ->
        Rename renameAssocs <$> normalizeAttributeNames expr'
      Difference exprA exprB ->
        Difference <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Group attrNameExprs gname expr' ->
        Group <$> normalizeAttributeNames attrNameExprs <*> pure gname <*> normalizeAttributeNames expr'
      Ungroup ungname expr' ->
        Ungroup ungname <$> normalizeAttributeNames expr'
      Restrict rpredExpr expr' -> do
        normExpr <- normalizeAttributeNames expr'
        exprType' <- typeForGraphRefRelationalExpr (fmap (const UncommittedContextMarker) normExpr)
        let env = mergeAttributesIntoGraphRefRelationalExprEnv (attributes exprType')       
        Restrict <$> local env (normalizeAttributeNames rpredExpr) <*> pure normExpr
      Equals exprA exprB ->
        Equals <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      NotEquals exprA exprB ->
        NotEquals <$> normalizeAttributeNames exprA <*> normalizeAttributeNames exprB
      Extend extendTuplesExpr expr' ->
        Extend <$> normalizeAttributeNames extendTuplesExpr <*> normalizeAttributeNames expr'
      With withNamesAssocs expr' ->
        With <$> normalizeAttributeNames withNamesAssocs <*> normalizeAttributeNames expr'

instance NormalizableAttributeNames (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  type AttributeNamesResult (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    TupleExprBase AttributeNames ()
  type AttributeNamesM (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (TupleExpr tupExprMap) = do
    let mapper (k,v) = do
          v' <- normalizeAttributeNames v
          pure (k,v')
    TupleExpr . M.fromList <$> mapM mapper (M.toList tupExprMap)

instance NormalizableAttributeNames (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  type AttributeNamesResult (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    AtomExprBase AttributeNames ()
  type AttributeNamesM (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames expr = do
    case expr of
      AttributeAtomExpr nam ->
        pure $ AttributeAtomExpr nam
      SubrelationAttributeAtomExpr relAttr subAttr ->
        pure (SubrelationAttributeAtomExpr relAttr subAttr)
      NakedAtomExpr atom ->
        pure $ NakedAtomExpr atom
      FunctionAtomExpr fName atomExprs a ->
        FunctionAtomExpr fName <$> mapM normalizeAttributeNames atomExprs  <*> pure a
      RelationAtomExpr expr' -> RelationAtomExpr <$> normalizeAttributeNames expr'
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr <$> normalizeAttributeNames ifE <*> normalizeAttributeNames thenE <*> normalizeAttributeNames elseE
      ConstructedAtomExpr dConsName atomExprs a ->
        ConstructedAtomExpr dConsName <$> mapM normalizeAttributeNames atomExprs <*> pure a

instance NormalizableAttributeNames (WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  type AttributeNamesResult (WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    WithNamesAssocsBase AttributeNames ()
  type AttributeNamesM (WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames assocs = do
    let mapper (WithNameExpr rv tm,
                relExpr) = (,) <$> pure (WithNameExpr rv tm) <*> normalizeAttributeNames relExpr
    mapM mapper assocs

instance NormalizableAttributeNames (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  type AttributeNamesResult (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    ExtendTupleExprBase AttributeNames ()
  type AttributeNamesM (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalizeAttributeNames atomExpr

instance NormalizableAttributeNames (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  type AttributeNamesResult (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    TupleExprsBase AttributeNames ()
  type AttributeNamesM (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames (TupleExprs a tups) = TupleExprs a <$> mapM normalizeAttributeNames tups

instance NormalizableAttributeNames (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) where
  type AttributeNamesResult (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    RestrictionPredicateExprBase AttributeNames ()
  type AttributeNamesM (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) =
    GraphRefRelationalExprM
  
  normalizeAttributeNames expr = do
    case expr of
      TruePredicate ->
        pure TruePredicate
      AndPredicate a b ->
        AndPredicate <$> normalizeAttributeNames a <*> normalizeAttributeNames b
      OrPredicate a b ->
        OrPredicate <$> normalizeAttributeNames a <*> normalizeAttributeNames b
      NotPredicate a ->
        NotPredicate <$> normalizeAttributeNames a
      RelationalExprPredicate relExpr' ->
        RelationalExprPredicate <$> normalizeAttributeNames relExpr'
      AtomExprPredicate atomExpr ->
        AtomExprPredicate <$> normalizeAttributeNames atomExpr
      AttributeEqualityPredicate nam atomExpr ->
        AttributeEqualityPredicate nam <$> normalizeAttributeNames atomExpr

instance NormalizableAttributeNames (DatabaseContextIOExprBase a) where
  type AttributeNamesResult (DatabaseContextIOExprBase a) =
    DatabaseContextIOExprBase a
  type AttributeNamesM (DatabaseContextIOExprBase a) =
    DatabaseContextIOEvalMonad
  
  normalizeAttributeNames expr = pure expr
        
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
    NormalizableAttributeNames y,
    MonadError RelationalError (AttributeNamesM y)
  ) =>
  (TransactionGraph, GraphRefTransactionMarker) ->
  x ->
  AttributeNamesM y (AttributeNamesResult y)
transGraphNormalizeM graphAndMarker expr =
  case normalizeTransactionMarker graphAndMarker expr of
    Left err -> throwError err
    Right norm1 ->
      normalizeAttributeNames norm1

normalizeE :: 
  (
   NormalizableTransactionMarker x,
   Ret x ~ Either RelationalError y,
   NormalizableAttributeNames y,
   MonadError RelationalError (AttributeNamesM y)   
   ) =>
  Args x ->
  x ->
  AttributeNamesM y (AttributeNamesResult y)
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
 AttributeNamesM y (AttributeNamesResult y)
normalizeM expr =
  let norm1 = normalizeTransactionMarker UncommittedContextMarker expr
  in
    normalizeAttributeNames norm1

normalize ::
  (
   NormalizableTransactionMarker x,
   NormalizableAttributeNames (Ret x),
   Args x ~ GraphRefTransactionMarker,
   AttributeNamesM (Ret x) ~ GraphRefRelationalExprM
   ) =>
  Maybe DatabaseContext ->
  TransactionGraph ->
  x ->
  Either RelationalError (AttributeNamesResult (Ret x))
normalize mctx tgraph expr =
  let norm1 = normalizeTransactionMarker UncommittedContextMarker expr
      gfEnv = freshGraphRefRelationalExprEnv mctx tgraph
  in
    runGraphRefRelationalExprM gfEnv (normalizeAttributeNames norm1)

normalizeDBCExpr ::    
  (
   NormalizableTransactionMarker x,
   NormalizableAttributeNames (Ret x),
   Args x ~ GraphRefTransactionMarker
   ) =>
  x ->
  AttributeNamesM (Ret x) (AttributeNamesResult (Ret x))
normalizeDBCExpr expr =
  let norm1 = normalizeTransactionMarker UncommittedContextMarker expr
  in
  normalizeAttributeNames norm1

normalize' ::
  (
   NormalizableTransactionMarker x,
   NormalizableAttributeNames (Ret x),
   Args x ~ GraphRefTransactionMarker,
   AttributeNamesM (Ret x) ~ GraphRefRelationalExprM
   ) =>
  DatabaseContext ->
  TransactionGraph ->
  x ->
  Either RelationalError (AttributeNamesResult (Ret x))
normalize' dbctx = normalize (Just dbctx)

normalizeAttributeNames' ::
  (NormalizableAttributeNames x
  )
  => x -> AttributeNamesM x (AttributeNamesResult x)
normalizeAttributeNames' expr = normalizeAttributeNames expr

normalizeTransactionMarker' ::
  (NormalizableTransactionMarker x,
   Args x ~ GraphRefTransactionMarker) =>
  x -> Ret x
normalizeTransactionMarker' expr =
  normalizeTransactionMarker UncommittedContextMarker expr
