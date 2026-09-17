-- | Functions to convert all types of expresions into their GraphRef- equivalents.
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, RankNTypes, FlexibleContexts #-}
module ProjectM36.NormalizeExpr where
import ProjectM36.Base (GraphRefTransactionMarker, TransactionId, RelationalExprBase(..), RestrictionPredicateExprBase(..), WithNameExprBase(..), WithNameExpr, AttributeNamesExprBase(..), GraphRefWithNameExpr, AttributeNamesExprBase(..), AttributeNamesExpr, DatabaseContextExprBase(..), DatabaseContextIOExprBase(..), DatabaseContextIOExpr, GraphRefDatabaseContextIOExpr, ExtendTupleExprBase(..), AtomExprBase(..), TupleExprsBase(..), TupleExprBase(..), AttributeExprBase(..), AttributeNames, WithNamesAssocsBase , AttributeNamesExprBase(..), AttributeNameAtomExprMapBase(..), GraphRefAttributeNamesExpr, InclusionDependencyBase(..), GraphRefTransactionMarker(..))
import ProjectM36.AttributeNamesBase (RelationalExpr, GraphRefRelationalExpr, RestrictionPredicateExpr, GraphRefRestrictionPredicateExpr, TupleExprs, GraphRefTupleExprs, TupleExpr, GraphRefTupleExpr, ExtendTupleExpr, GraphRefExtendTupleExpr, AtomExpr, GraphRefAtomExpr, DatabaseContextExpr, GraphRefDatabaseContextExpr)
import ProjectM36.RelationalExpression (GraphRefRelationalExprM)
import qualified ProjectM36.Attribute as A
import ProjectM36.RelationalExpression
import ProjectM36.Relation (relationTrue)
import ProjectM36.Error
import ProjectM36.TransGraphRelationalExpression.Types
import ProjectM36.TransactionGraph.Types
import ProjectM36.Transaction.Types
import ProjectM36.TransactionGraph

import Control.Monad.Trans.Reader as R
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S


-- TODO: should I switch to associated type families to control which context is required- normalizing trans-graph expressions requires the transaction graph, but relationalexpr does not even require the marker argument which leads to problems in isoschema which does not pass the graph around
-- | Normalize attribute expressions to attribute names across Base types.
type family NormalizeAttributeNames a where
  NormalizeAttributeNames (RelationalExprBase at a) = RelationalExprBase AttributeNames a

  NormalizeAttributeNames (TupleExprsBase at a) = TupleExprsBase AttributeNames a

  NormalizeAttributeNames (AttributeNamesExprBase a) = AttributeNames

  NormalizeAttributeNames (RestrictionPredicateExprBase (AttributeNamesExprBase a) a) = RestrictionPredicateExprBase AttributeNames a

  NormalizeAttributeNames (ExtendTupleExprBase (AttributeNamesExprBase a) a) = ExtendTupleExprBase AttributeNames a

  NormalizeAttributeNames (WithNamesAssocsBase (AttributeNamesExprBase a) a) = WithNamesAssocsBase AttributeNames a

  NormalizeAttributeNames (TupleExprBase (AttributeNamesExprBase a) a) = TupleExprBase AttributeNames a
  
  NormalizeAttributeNames (AtomExprBase (AttributeNamesExprBase a) a) = AtomExprBase AttributeNames a  

  NormalizeAttributeNames (RelationalExprBase GraphRefAttributeNamesExpr GraphRefTransactionMarker) = RelationalExprBase AttributeNames GraphRefTransactionMarker

  NormalizeAttributeNames (DatabaseContextExprBase (AttributeNamesExprBase a) a r) = DatabaseContextExprBase AttributeNames a r

  NormalizeAttributeNames (AttributeNameAtomExprMapBase (AttributeNamesExprBase a) a) = AttributeNameAtomExprMapBase AttributeNames a

  NormalizeAttributeNames (InclusionDependencyBase (AttributeNamesExprBase a) a) = InclusionDependencyBase AttributeNames a

  -- trans-graph relational expression normalization

  NormalizeAttributeNames TransGraphRelationalExpr = RelationalExprBase AttributeNames GraphRefTransactionMarker

-- | Normalize transaction marker across Base types.
type family NormalizeTransactionMarker a where
  NormalizeTransactionMarker (RelationalExprBase (AttributeNamesExprBase ()) ()) = RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker (TupleExprsBase (AttributeNamesExprBase ()) ()) = TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker (AttributeNamesExprBase ()) = AttributeNamesExprBase GraphRefTransactionMarker

  NormalizeTransactionMarker (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) = RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
  NormalizeTransactionMarker (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) = ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) = WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker (TupleExprBase (AttributeNamesExprBase ()) ()) = TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker
  
  NormalizeTransactionMarker (AtomExprBase (AttributeNamesExprBase ()) ()) = AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker (WithNameExprBase ()) = WithNameExprBase GraphRefTransactionMarker

  NormalizeTransactionMarker (DatabaseContextExprBase (AttributeNamesExprBase ()) () r) = DatabaseContextExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker r

  NormalizeTransactionMarker (AttributeNameAtomExprMapBase (AttributeNamesExprBase ()) ()) = AttributeNameAtomExprMapBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker (AttributeExprBase ()) = AttributeExprBase GraphRefTransactionMarker

  NormalizeTransactionMarker (InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) ()) = InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker (InclusionDependencyBase (AttributeNamesExprBase ()) ()) = InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

-- AttributeNames family for when sometimes the attribute names have already been normalized
  NormalizeTransactionMarker (RestrictionPredicateExprBase AttributeNames ()) = RestrictionPredicateExprBase AttributeNames GraphRefTransactionMarker

  NormalizeTransactionMarker (RelationalExprBase AttributeNames ()) = RelationalExprBase AttributeNames GraphRefTransactionMarker

  NormalizeTransactionMarker (AtomExprBase AttributeNames ()) = AtomExprBase AttributeNames GraphRefTransactionMarker  

-- trans-graph relational expressions

  NormalizeTransactionMarker TransGraphRelationalExpr = RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker TransGraphTupleExprs = TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker TransGraphTupleExpr = TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker TransGraphAttributeNamesExpr = AttributeNamesExprBase GraphRefTransactionMarker

  NormalizeTransactionMarker TransGraphAttributeExpr = AttributeExprBase GraphRefTransactionMarker

  NormalizeTransactionMarker TransGraphRestrictionPredicateExpr = RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker TransGraphExtendTupleExpr = ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker

  NormalizeTransactionMarker TransGraphWithNameExpr = WithNameExprBase GraphRefTransactionMarker
 
class NormalizableAttributeNames a where
  -- | convert AttributeNamesExpr -> AttributeNames (removing expressions to calculate attribute names)
  normalizeAttributeNames :: GraphRefRelationalExpr -> a -> GraphRefRelationalExprM (NormalizeAttributeNames a)

instance NormalizableAttributeNames (RelationalExprBase AttributeNames a) where
  normalizeAttributeNames marker expr = pure expr
  

instance NormalizableAttributeNames (RelationalExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames relExpr
    case expr of
      MakeRelationFromExprs Nothing tupleExprs -> 
        MakeRelationFromExprs Nothing <$> norm tupleExprs
      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)
      ExistingRelation rel ->
        pure (ExistingRelation rel)
      RelationVariable nam x ->
        pure (RelationVariable nam x)
      RelationValuedAttribute nam ->
        pure (RelationValuedAttribute nam)
      Project attrExprs expr -> do
        Project <$> norm attrExprs <*> norm expr
      Union exprA exprB ->
        Union <$> norm exprA <*> norm exprB
      Join exprA exprB ->
        Join <$> norm exprA <*> norm exprB
      Rename renameAssocs expr ->
        Rename renameAssocs <$> norm expr
      Difference exprA exprB ->
        Difference <$> norm exprA <*> norm exprB
      Group attrNameExprs gname expr -> do
        Group <$> norm attrNameExprs <*> pure gname <*> norm expr
      Ungroup ungname expr ->
        Ungroup ungname <$> norm expr
      Restrict rpredExpr expr ->
        Restrict <$> norm rpredExpr <*> norm expr
      Equals exprA exprB ->
        Equals <$> norm exprA <*> norm exprB
      NotEquals exprA exprB ->
        NotEquals <$> norm exprA <*> norm exprB
      Extend extendTuplesExpr expr ->
        Extend <$> norm extendTuplesExpr <*> norm expr
      With withNamesAssocs expr ->
        With <$> norm withNamesAssocs <*> norm expr

instance NormalizableAttributeNames (TupleExprsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr (TupleExprs a tups) = TupleExprs a <$> mapM (normalizeAttributeNames relExpr) tups

instance NormalizableAttributeNames (TupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr (TupleExpr tupExprMap) = do
    let mapper (k,v) = do
          v' <- normalizeAttributeNames relExpr v
          pure (k,v')
    TupleExpr . M.fromList <$> mapM mapper (M.toList tupExprMap)

instance NormalizableAttributeNames (RestrictionPredicateExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames relExpr
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
      AttributeEqualityPredicate nam atomExpr ->
        AttributeEqualityPredicate nam <$> norm atomExpr

instance NormalizableAttributeNames (AtomExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames relExpr
    case expr of
      AttributeAtomExpr nam ->
        pure $ AttributeAtomExpr nam
      SubrelationAttributeAtomExpr relAttr subAttr ->
        pure (SubrelationAttributeAtomExpr relAttr subAttr)
      NakedAtomExpr atom ->
        pure $ NakedAtomExpr atom
      FunctionAtomExpr fName atomExprs a ->
        FunctionAtomExpr fName <$> mapM norm atomExprs  <*> pure a
      RelationAtomExpr expr -> RelationAtomExpr <$> norm expr
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr <$> norm ifE <*> norm thenE <*> norm elseE
      ConstructedAtomExpr dConsName atomExprs a ->
        ConstructedAtomExpr dConsName <$> mapM norm atomExprs <*> pure a

instance NormalizableAttributeNames (ExtendTupleExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalizeAttributeNames relExpr atomExpr

instance NormalizableAttributeNames (WithNamesAssocsBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr assocs = do
    let mapper (WithNameExpr rv tm,
                relExpr') = (,) <$> pure (WithNameExpr rv tm) <*> normalizeAttributeNames relExpr relExpr'
    mapM mapper assocs

instance NormalizableAttributeNames (AttributeNamesExprBase GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr expr = do
    let normalizeRelationalExpr = normalizeAttributeNames relExpr 
    evalGraphRefAttributeNamesExpr normalizeRelationalExpr expr relExpr

instance NormalizableAttributeNames (DatabaseContextExprBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker r) where
  normalizeAttributeNames relExpr expr = do
    let norm :: forall a. NormalizableAttributeNames a => a -> GraphRefRelationalExprM (NormalizeAttributeNames a)
        norm = normalizeAttributeNames relExpr
    case expr of
      NoOperation -> pure NoOperation
      Define nam attrExprs -> pure (Define nam attrExprs)
      Undefine nam -> pure (Undefine nam)
      Assign nam rexpr -> Assign nam <$> norm rexpr
      Insert nam rexpr -> Insert nam <$> norm rexpr
      Delete nam pred' -> Delete nam <$> norm pred'
      Update nam attrMap pred' -> Update nam <$> norm attrMap <*> norm pred'

      AddInclusionDependency nam dep -> AddInclusionDependency nam <$> norm dep
      RemoveInclusionDependency nam -> pure (RemoveInclusionDependency nam)
      AddNotification nam exprA exprB exprC -> pure (AddNotification nam exprA exprB exprC)
      RemoveNotification nam -> pure (RemoveNotification nam)
      AddTypeConstructor tyDef consDefs -> pure (AddTypeConstructor tyDef consDefs)
      RemoveTypeConstructor tyName -> pure (RemoveTypeConstructor tyName)

      RemoveAtomFunction aFuncName -> pure (RemoveAtomFunction aFuncName)
      RemoveDatabaseContextFunction funcName' -> pure (RemoveDatabaseContextFunction funcName')
      ExecuteDatabaseContextFunction funcName' atomExprs -> ExecuteDatabaseContextFunction funcName' <$> mapM norm atomExprs
      AddRegisteredQuery n q -> pure (AddRegisteredQuery n q)
      RemoveRegisteredQuery n -> pure (RemoveRegisteredQuery n)
      AlterACL e -> pure (AlterACL e)
      MultipleExpr exprs -> MultipleExpr <$> mapM norm exprs

instance NormalizableAttributeNames (AttributeNameAtomExprMapBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr map' =
    M.fromList <$> mapM mapper (M.toList map')
    where
      mapper (k,v) = (,) <$> pure k <*> normalizeAttributeNames relExpr v

instance NormalizableAttributeNames (InclusionDependencyBase (AttributeNamesExprBase GraphRefTransactionMarker) GraphRefTransactionMarker) where
  normalizeAttributeNames relExpr (InclusionDependency relExprA relExprB) =
    InclusionDependency <$> normalizeAttributeNames relExpr relExprA <*> normalizeAttributeNames relExpr relExprB
    
---------------------------------------------------      
class NormalizableTransactionMarker a where
  normalizeTransactionMarker :: TransactionGraph -> GraphRefTransactionMarker -> a -> Either RelationalError (NormalizeTransactionMarker a)

instance NormalizableTransactionMarker (RelationalExprBase (AttributeNamesExprBase ()) ()) where
  normalizeTransactionMarker graph marker expr =
    let norm :: forall a. NormalizableTransactionMarker a => a -> Either RelationalError (NormalizeTransactionMarker a)
        norm = normalizeTransactionMarker graph marker
        in
    case expr of
      MakeRelationFromExprs mAttrExprs tupleExprs -> do
        mAttrExprs' <- case mAttrExprs of
                         Nothing -> pure Nothing
                         Just exprs -> Just <$> mapM norm exprs
        MakeRelationFromExprs mAttrExprs' <$> norm tupleExprs
        

instance NormalizableTransactionMarker (RestrictionPredicateExprBase (AttributeNamesExprBase ()) ()) where
  normalizeTransactionMarker graph marker expr =
    let norm :: forall a. NormalizableTransactionMarker a => a -> Either RelationalError (NormalizeTransactionMarker a)
        norm = normalizeTransactionMarker graph marker
    in
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

instance NormalizableTransactionMarker (TupleExprsBase (AttributeNamesExprBase ()) ()) where
  normalizeTransactionMarker graph marker (TupleExprs () exprs) =
    TupleExprs marker <$> mapM (normalizeTransactionMarker graph marker) exprs
    

instance NormalizableTransactionMarker (AttributeNamesExprBase ()) where
  normalizeTransactionMarker graph marker expr =
    case expr of
      AttributeNames names ->
        pure (AttributeNames names)
      InvertedAttributeNames names ->
        pure (InvertedAttributeNames names)
      UnionAttributeNames a b ->
        UnionAttributeNames <$> normalizeTransactionMarker graph marker a <*> normalizeTransactionMarker graph marker b
      IntersectAttributeNames a b ->
        IntersectAttributeNames <$> normalizeTransactionMarker graph marker a <*> normalizeTransactionMarker graph marker b
      RelationalExprAttributeNames relExpr ->
        RelationalExprAttributeNames <$> normalizeTransactionMarker graph marker relExpr

instance NormalizableTransactionMarker (ExtendTupleExprBase (AttributeNamesExprBase ()) ()) where
  normalizeTransactionMarker graph marker (AttributeExtendTupleExpr attrs atomExpr) =
    AttributeExtendTupleExpr attrs <$> normalizeTransactionMarker graph marker atomExpr

instance NormalizableTransactionMarker (WithNamesAssocsBase (AttributeNamesExprBase ()) ()) where
  normalizeTransactionMarker graph marker assocs =
    mapM mapper assocs
    where
      mapper (withExpr, relExpr) =
        (,)
        <$> normalizeTransactionMarker graph marker withExpr
        <*> normalizeTransactionMarker graph marker relExpr

instance NormalizableTransactionMarker (TupleExprBase (AttributeNamesExprBase ()) ()) where
  normalizeTransactionMarker graph marker (TupleExpr tupMap) = TupleExpr <$> mapM (normalizeTransactionMarker graph marker) tupMap
  
instance NormalizableTransactionMarker (AtomExprBase (AttributeNamesExprBase ()) ()) where
  normalizeTransactionMarker graph marker expr =
    let norm :: forall a. NormalizableTransactionMarker a => a -> Either RelationalError (NormalizeTransactionMarker a)
        norm = normalizeTransactionMarker graph marker
    in
    case expr of
      AttributeAtomExpr aname ->
        pure (AttributeAtomExpr aname)
      SubrelationAttributeAtomExpr aname aname2 ->
        pure (SubrelationAttributeAtomExpr aname aname2)
      NakedAtomExpr atom ->
        pure (NakedAtomExpr atom)
      FunctionAtomExpr fname atomExprs () ->
        FunctionAtomExpr fname <$> mapM norm atomExprs <*> pure marker
      RelationAtomExpr relExpr ->
        RelationAtomExpr <$> norm relExpr
      IfThenAtomExpr a b c ->
        IfThenAtomExpr <$> norm a <*> norm b <*> norm c
      ConstructedAtomExpr dConsName atomExprs () ->
        ConstructedAtomExpr dConsName <$> mapM norm atomExprs <*> pure marker
        
        
instance NormalizableTransactionMarker (DatabaseContextExprBase (AttributeNamesExprBase ()) () r) where
  normalizeTransactionMarker graph marker expr =
    case expr of
      NoOperation ->
        pure NoOperation
      Define rvname attrExprs ->
        Define rvname <$> mapM (normalizeTransactionMarker graph marker) attrExprs
      Undefine rvname ->
        pure (Undefine rvname)
      Assign rvName relExpr ->
        Assign rvName <$> normalizeTransactionMarker graph marker relExpr
      Insert rvName relExpr ->
        Insert rvName <$> normalizeTransactionMarker graph marker relExpr
      Delete rvName predi ->
        Delete rvName <$> normalizeTransactionMarker graph marker predi
      Update rvName attrAtomMap predi ->
        Update rvName <$> normalizeTransactionMarker graph marker attrAtomMap <*> normalizeTransactionMarker graph marker predi
      AddInclusionDependency depname incDep ->
        AddInclusionDependency depname <$> normalizeTransactionMarker graph marker incDep
      RemoveInclusionDependency depname ->
        pure (RemoveInclusionDependency depname)
      AddNotification notName a b c ->
        pure (AddNotification notName a b c)
      RemoveNotification notName ->
        pure (RemoveNotification notName)
      AddTypeConstructor tConsDef dConsDefs ->
        pure (AddTypeConstructor tConsDef dConsDefs)
      RemoveTypeConstructor tConsName ->
        pure (RemoveTypeConstructor tConsName)
      RemoveAtomFunction fname ->
        pure (RemoveAtomFunction fname)
      RemoveDatabaseContextFunction fname ->
        pure (RemoveDatabaseContextFunction fname)
      ExecuteDatabaseContextFunction fname atomExprs ->
        ExecuteDatabaseContextFunction fname <$> mapM (normalizeTransactionMarker graph marker) atomExprs
      AddRegisteredQuery qname relExpr ->
        pure (AddRegisteredQuery qname relExpr)
      RemoveRegisteredQuery qname ->
        pure (RemoveRegisteredQuery qname)
      AlterACL aclExpr ->
        pure (AlterACL aclExpr)
      MultipleExpr exprs ->
        MultipleExpr <$> mapM (normalizeTransactionMarker graph marker) exprs
                      
instance NormalizableTransactionMarker (AttributeNameAtomExprMapBase (AttributeNamesExprBase ()) ()) where
  normalizeTransactionMarker graph marker attrAtomMap = mapM (normalizeTransactionMarker graph marker) attrAtomMap

instance NormalizableTransactionMarker (AttributeExprBase ()) where
  normalizeTransactionMarker graph marker expr =
    case expr of
      AttributeAndTypeNameExpr aname tCons () ->
        pure (AttributeAndTypeNameExpr aname tCons marker)
      NakedAttributeExpr aname ->
        pure (NakedAttributeExpr aname)

instance NormalizableTransactionMarker (WithNameExprBase ()) where
  normalizeTransactionMarker graph marker (WithNameExpr rvName ()) =
    pure (WithNameExpr rvName marker)

instance NormalizableTransactionMarker (RestrictionPredicateExprBase AttributeNames ()) where
  normalizeTransactionMarker graph marker expr =
    mapM (const (pure marker)) expr


-- attributes already normalized
instance NormalizableTransactionMarker (RelationalExprBase AttributeNames ()) where
  normalizeTransactionMarker graph marker expr = mapM (const (pure marker)) expr

instance NormalizableTransactionMarker (InclusionDependencyBase (AttributeNamesExprBase ()) ()) where
  normalizeTransactionMarker graph marker (InclusionDependency a b) =
    InclusionDependency <$> normalizeTransactionMarker graph marker a <*> normalizeTransactionMarker graph marker b

instance NormalizableTransactionMarker TransGraphRelationalExpr where
  normalizeTransactionMarker graph marker expr = do
    let norm :: forall a. NormalizableTransactionMarker a => a -> Either RelationalError (NormalizeTransactionMarker a)
        norm = normalizeTransactionMarker graph marker
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
      Project transAttrNames expr ->
        Project <$> norm transAttrNames <*> norm expr
      Union exprA exprB ->
        Union <$> norm exprA <*> norm exprB
      Join exprA exprB ->
        Join <$> norm exprA <*> norm exprB
      Rename attrs expr ->
        Rename attrs <$> norm expr
      Difference exprA exprB ->
        Difference <$> norm exprA <*> norm exprB
      Group transAttrNames attrName expr ->
        Group 
        <$> norm transAttrNames 
        <*> pure attrName
        <*> norm expr
      Ungroup attrName expr -> 
        Ungroup attrName <$> norm expr
      Restrict predicateExpr expr ->
        Restrict <$> norm predicateExpr <*> norm expr
      Equals exprA exprB ->
        Equals <$> norm exprA <*> norm exprB
      NotEquals exprA exprB ->  
        NotEquals <$> norm exprA <*> norm exprB
      Extend extendExpr expr -> 
        Extend <$> norm extendExpr <*> norm expr
      With views expr -> do
        evaldViews <- mapM (\(wnexpr, vexpr) ->
                         (,) <$> norm wnexpr <*> norm vexpr
                     ) views
        With evaldViews <$> norm expr

instance NormalizableTransactionMarker TransGraphAttributeExpr where
  normalizeTransactionMarker graph marker expr =
    case expr of
      AttributeAndTypeNameExpr aname tcons tLookup ->
        AttributeAndTypeNameExpr aname tcons <$> findTransId graph tLookup
      NakedAttributeExpr attr ->
        pure (NakedAttributeExpr attr)

instance NormalizableTransactionMarker TransGraphTupleExprs where
  normalizeTransactionMarker graph marker (TupleExprs tLookup tupExprs) =
    TupleExprs <$> findTransId graph tLookup <*> mapM (normalizeTransactionMarker graph marker) tupExprs

instance NormalizableTransactionMarker TransGraphAttributeNamesExpr where
  normalizeTransactionMarker graph marker expr =
    case expr of
      AttributeNames anames ->
        pure (AttributeNames anames)
      InvertedAttributeNames anames ->
        pure (InvertedAttributeNames anames)
      UnionAttributeNames a b ->
        UnionAttributeNames <$> normalizeTransactionMarker graph marker a <*> normalizeTransactionMarker graph marker b
      IntersectAttributeNames a b ->
        IntersectAttributeNames <$> normalizeTransactionMarker graph marker a <*> normalizeTransactionMarker graph marker b
      RelationalExprAttributeNames relExpr ->
        RelationalExprAttributeNames <$> normalizeTransactionMarker graph marker relExpr

instance NormalizableTransactionMarker TransGraphRestrictionPredicateExpr where
  normalizeTransactionMarker graph marker expr = do
    let norm :: forall a. NormalizableTransactionMarker a => a -> Either RelationalError (NormalizeTransactionMarker a)
        norm = normalizeTransactionMarker graph marker
    case expr of
      TruePredicate ->
        pure TruePredicate
      AndPredicate a b ->
        AndPredicate <$> norm a <*> norm b
      OrPredicate a b ->
        OrPredicate <$> norm a <*> norm b
      NotPredicate a b ->
        NotPredicate <$> norm a <*> norm b
      RelationalExprPredicate relExpr ->
        RelationalExprPredicate <$> norm relExpr
      AtomExprPredicate atomExpr ->
        AtomExprPredicate <$> norm atomExpr
      AttributeEqualityPredicate aname atomExpr ->
        AttributeEqualityPredicate aname <$> norm atomExpr

instance NormalizableTransactionMarker TransGraphExtendTupleExpr where
  normalizeTransactionMarker graph marker (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalizeTransactionMarker graph marker atomExpr


instance NormalizableTransactionMarker TransGraphWithNameExpr where
  normalizeTransactionMarker graph marker expr = undefined

instance NormalizableTransactionMarker TransGraphTupleExpr where
  normalizeTransactionMarker graph marker expr = undefined
  

findTransId :: TransactionGraph -> TransactionIdLookup -> Either RelationalError GraphRefTransactionMarker
findTransId graph tlook = TransactionMarker . transactionId <$> lookupTransaction graph tlook

normalize ::
  (
   NormalizableTransactionMarker x,
   NormalizableAttributeNames (NormalizeTransactionMarker x)
   ) =>
  TransactionGraph ->
  x ->
  GraphRefRelationalExprM (NormalizeAttributeNames (NormalizeTransactionMarker x))
normalize graph expr = do
  case normalizeTransactionMarker graph UncommittedContextMarker expr of
    Left err -> throwError err
    Right norm1 ->
      normalizeAttributeNames (ExistingRelation relationTrue) norm1
