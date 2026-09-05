-- | Functions to convert all types of expresions into their GraphRef- equivalents.
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module ProjectM36.NormalizeExpr where
import ProjectM36.Base (GraphRefTransactionMarker, TransactionId, RelationalExprBase(..), RestrictionPredicateExprBase(..), WithNameExprBase(..), WithNameExpr, AttributeNamesExprBase(..), GraphRefWithNameExpr, AttributeNamesExprBase(..), AttributeNamesExpr, DatabaseContextExprBase(..), DatabaseContextIOExprBase(..), DatabaseContextIOExpr, GraphRefDatabaseContextIOExpr, ExtendTupleExprBase(..), AtomExprBase(..), TupleExprsBase(..), TupleExprBase(..), AttributeExprBase(..), AttributeExpr, GraphRefAttributeNamesExpr, GraphRefAttributeExpr, AttributeNames, WithNamesAssocsBase , AttributeNamesExprBase(..))
import ProjectM36.AttributeNamesBase (RelationalExpr, GraphRefRelationalExpr, RestrictionPredicateExpr, GraphRefRestrictionPredicateExpr, TupleExprs, GraphRefTupleExprs, TupleExpr, GraphRefTupleExpr, ExtendTupleExpr, GraphRefExtendTupleExpr, AtomExpr, GraphRefAtomExpr, DatabaseContextExpr, GraphRefDatabaseContextExpr)
import ProjectM36.RelationalExpression (GraphRefRelationalExprM)
import qualified ProjectM36.Attribute as A
import ProjectM36.RelationalExpression

import Control.Monad.Trans.Reader as R
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S

type family Normalize a where
  Normalize (RelationalExprBase AttributeNames ()) = RelationalExprBase AttributeNames GraphRefTransactionMarker
  Normalize (RelationalExprBase AttributeNamesExpr ()) = RelationalExprBase AttributeNames GraphRefTransactionMarker
  Normalize (RelationalExprBase GraphRefAttributeNamesExpr GraphRefTransactionMarker) = RelationalExprBase AttributeNames GraphRefTransactionMarker

  Normalize (AttributeNamesExprBase ()) = AttributeNames
  Normalize (AttributeNamesExprBase GraphRefTransactionMarker) = AttributeNames

  Normalize (TupleExprsBase AttributeNamesExpr ()) = TupleExprsBase AttributeNames GraphRefTransactionMarker

  Normalize (TupleExprBase AttributeNamesExpr ()) = TupleExprBase AttributeNames GraphRefTransactionMarker

  Normalize (RestrictionPredicateExprBase AttributeNamesExpr ()) = RestrictionPredicateExprBase AttributeNames GraphRefTransactionMarker

  Normalize (ExtendTupleExprBase AttributeNamesExpr ()) = ExtendTupleExprBase AttributeNames GraphRefTransactionMarker

  Normalize (WithNamesAssocsBase AttributeNamesExpr ()) = WithNamesAssocsBase AttributeNames GraphRefTransactionMarker

  Normalize (AtomExprBase AttributeNamesExpr ()) = AtomExprBase AttributeNames GraphRefTransactionMarker


class Normalizable a where
  normalize :: GraphRefTransactionMarker -> a -> GraphRefRelationalExprM (Normalize a)

instance Normalizable (RelationalExprBase AttributeNames ()) where
  normalize marker expr = pure (fmap (const marker) expr)

instance Normalizable (RelationalExprBase AttributeNamesExpr ()) where
  normalize marker expr =
    case expr of
      MakeRelationFromExprs Nothing tupleExprs -> 
        MakeRelationFromExprs Nothing <$> normalize marker tupleExprs
      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)
      ExistingRelation rel ->
        pure (ExistingRelation rel)
      RelationVariable nam () ->
        pure (RelationVariable nam marker)
      RelationValuedAttribute nam ->
        pure (RelationValuedAttribute nam)
      Project attrExprs expr -> do
        expr' <- normalize marker expr
        Project <$> normalizeAttributeNamesExpr marker attrExprs expr' <*> pure expr'
      Union exprA exprB ->
        Union <$> normalize marker exprA <*> normalize marker exprB
      Join exprA exprB ->
        Join <$> normalize marker exprA <*> normalize marker exprB
      Rename renameAssocs expr ->
        Rename renameAssocs <$> normalize marker expr
      Difference exprA exprB ->
        Difference <$> normalize marker exprA <*> normalize marker exprB
      Group attrNameExprs gname expr -> do
        expr' <- normalize marker expr
        Group <$> normalizeAttributeNamesExpr marker attrNameExprs expr' <*> pure gname <*> pure expr'
      Ungroup ungname expr ->
        Ungroup ungname <$> normalize marker expr
      Restrict rpredExpr expr ->
        Restrict <$> normalize marker rpredExpr <*> normalize marker expr
      Equals exprA exprB ->
        Equals <$> normalize marker exprA <*> normalize marker exprB
      NotEquals exprA exprB ->
        NotEquals <$> normalize marker exprA <*> normalize marker exprB
      Extend extendTuplesExpr expr ->
        Extend <$> normalize marker extendTuplesExpr <*> normalize marker expr
      With withNamesAssocs expr ->
        With <$> normalize marker withNamesAssocs <*> normalize marker expr

instance Normalizable (TupleExprsBase AttributeNamesExpr ()) where
  normalize marker (TupleExprs () tups) = TupleExprs marker <$> mapM (normalize marker) tups

instance Normalizable (TupleExprBase AttributeNamesExpr ()) where
  normalize marker (TupleExpr tupExprMap) = do
    let mapper (k,v) = do
          v' <- normalize marker v
          pure (k,v')
    TupleExpr . M.fromList <$> mapM mapper (M.toList tupExprMap)

instance Normalizable (RestrictionPredicateExprBase AttributeNamesExpr ()) where
  normalize marker expr = 
    case expr of
      TruePredicate ->
        pure TruePredicate
      AndPredicate a b ->
        AndPredicate <$> normalize marker a <*> normalize marker b
      OrPredicate a b ->
        OrPredicate <$> normalize marker a <*> normalize marker b
      NotPredicate a ->
        NotPredicate <$> normalize marker a
      RelationalExprPredicate relExpr ->
        RelationalExprPredicate <$> normalize marker relExpr
      AtomExprPredicate atomExpr ->
        AtomExprPredicate <$> normalize marker atomExpr
      AttributeEqualityPredicate nam atomExpr ->
        AttributeEqualityPredicate nam <$> normalize marker atomExpr

instance Normalizable (AtomExprBase AttributeNamesExpr ()) where
  normalize marker expr =
    case expr of
      AttributeAtomExpr nam ->
        pure $ AttributeAtomExpr nam
      SubrelationAttributeAtomExpr relAttr subAttr ->
        pure (SubrelationAttributeAtomExpr relAttr subAttr)
      NakedAtomExpr atom ->
        pure $ NakedAtomExpr atom
      FunctionAtomExpr fName atomExprs () ->
        FunctionAtomExpr fName <$> mapM (normalize marker) atomExprs  <*> pure marker
      RelationAtomExpr expr -> RelationAtomExpr <$> normalize marker expr
      IfThenAtomExpr ifE thenE elseE ->
        IfThenAtomExpr <$> normalize marker ifE <*> normalize marker thenE <*> normalize marker elseE
      ConstructedAtomExpr dConsName atomExprs () ->
        ConstructedAtomExpr dConsName <$> mapM (normalize marker) atomExprs <*> pure marker

instance Normalizable (ExtendTupleExprBase AttributeNamesExpr ()) where
  normalize marker (AttributeExtendTupleExpr aname atomExpr) =
    AttributeExtendTupleExpr aname <$> normalize marker atomExpr

instance Normalizable (WithNamesAssocsBase AttributeNamesExpr ()) where
  normalize marker assocs = do
    let mapper (WithNameExpr rv (),
                relExpr) = (,) <$> pure (WithNameExpr rv marker) <*> normalize marker relExpr
    mapM mapper assocs
    

normalizeAttributeNamesExpr :: GraphRefTransactionMarker -> AttributeNamesExprBase () -> GraphRefRelationalExpr -> GraphRefRelationalExprM AttributeNames
normalizeAttributeNamesExpr marker attrNamesExpr relExpr = do
  let setMarker expr =
        case expr of
          AttributeNames names -> pure $ AttributeNames names
          InvertedAttributeNames names -> pure $ InvertedAttributeNames names
          UnionAttributeNames a b -> UnionAttributeNames <$> setMarker a <*> setMarker b
          IntersectAttributeNames a b -> IntersectAttributeNames <$> setMarker a <*> setMarker b
          RelationalExprAttributeNames relExpr ->
            RelationalExprAttributeNames <$> normalize marker relExpr
  attrNamesExpr' <- setMarker attrNamesExpr
  normalizeGraphRefAttributeNamesExpr marker attrNamesExpr' relExpr

normalizeGraphRefAttributeNamesExpr :: GraphRefTransactionMarker -> AttributeNamesExprBase GraphRefTransactionMarker -> GraphRefRelationalExpr -> GraphRefRelationalExprM AttributeNames
normalizeGraphRefAttributeNamesExpr marker attrNamesExpr relExpr =
  let relExprNormalizer = normalize marker in
  evalGraphRefAttributeNamesExpr relExprNormalizer attrNamesExpr relExpr


{-
--used to process/normalize exprs to their respective graph ref forms
type ProcessExprM a = Reader GraphRefTransactionMarker a

type CurrentTransactionId = TransactionId

runProcessExprM :: GraphRefTransactionMarker -> ProcessExprM a -> a
runProcessExprM mtid m = runReader m mtid

askMarker :: ProcessExprM GraphRefTransactionMarker
askMarker = R.ask

-- convert a RelationalExpr into a GraphRefRelationalExpr using the current trans Id
processRelationalExpr :: RelationalExpr -> ProcessExprM GraphRefRelationalExpr
processRelationalExpr (MakeRelationFromExprs mAttrs tupleExprs) = do
  mAttrs' <- case mAttrs of
                  Nothing -> pure Nothing
                  Just mAttrs'' -> Just <$> mapM processAttributeExpr mAttrs''
  MakeRelationFromExprs mAttrs' <$> processTupleExprs tupleExprs

processRelationalExpr (ExistingRelation rel) = pure (ExistingRelation rel)
--requires current trans id and graph
processRelationalExpr (RelationValuedAttribute attrName) = pure (RelationValuedAttribute attrName)
processRelationalExpr (RelationVariable rv ()) = RelationVariable rv <$> askMarker
processRelationalExpr (Project attrNames expr) = Project <$> processAttributeNamesExpr attrNames <*> processRelationalExpr expr
processRelationalExpr (Union exprA exprB) = Union <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Join exprA exprB) = Join <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Rename attrs expr) =
  Rename attrs <$> processRelationalExpr expr
processRelationalExpr (Difference exprA exprB) = Difference <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Group attrNames attrName expr) = Group <$> processAttributeNamesExpr attrNames <*> pure attrName <*> processRelationalExpr expr
processRelationalExpr (Ungroup attrName expr) = Ungroup attrName <$> processRelationalExpr expr
processRelationalExpr (Restrict pred' expr) = Restrict <$> processRestrictionPredicateExpr pred' <*> processRelationalExpr expr
processRelationalExpr (Equals exprA exprB) =
  Equals <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (NotEquals exprA exprB) =   
  NotEquals <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Extend extendExpr expr) =
  Extend <$> processExtendTupleExpr extendExpr <*> processRelationalExpr expr
processRelationalExpr (With macros expr) =
  With <$> mapM (\(wnexpr, macroExpr) -> (,) <$> processWithNameExpr wnexpr <*> processRelationalExpr macroExpr) macros <*> processRelationalExpr expr

processWithNameExpr :: WithNameExpr -> ProcessExprM GraphRefWithNameExpr
processWithNameExpr (WithNameExpr rvname ()) =
  WithNameExpr rvname <$> askMarker

processAttributeNamesExpr :: AttributeNamesExpr -> ProcessExprM GraphRefAttributeNamesExpr
processAttributeNamesExpr (AttributeNames nameSet) = pure $ AttributeNames nameSet
processAttributeNamesExpr (InvertedAttributeNames attrNameSet) =
  pure $ InvertedAttributeNames attrNameSet
processAttributeNamesExpr (UnionAttributeNames attrNamesA attrNamesB) = UnionAttributeNames <$> processAttributeNamesExpr attrNamesA <*> processAttributeNamesExpr attrNamesB
processAttributeNamesExpr (IntersectAttributeNames attrNamesA attrNamesB) = IntersectAttributeNames <$> processAttributeNamesExpr attrNamesA <*> processAttributeNamesExpr attrNamesB
processAttributeNames (RelationalExprAttributeNames expr) = RelationalExprAttributeNames <$> processRelationalExpr expr

processDatabaseContextExpr :: DatabaseContextExpr -> ProcessExprM GraphRefDatabaseContextExpr
processDatabaseContextExpr expr =
  case expr of
    NoOperation -> pure NoOperation
    Define nam attrExprs -> Define nam <$> mapM processAttributeExpr attrExprs
    Undefine nam -> pure (Undefine nam)
    Assign nam rexpr -> Assign nam <$> processRelationalExpr rexpr
    Insert nam rexpr -> Insert nam <$> processRelationalExpr rexpr
    Delete nam pred' -> Delete nam <$> processRestrictionPredicateExpr pred'
    Update nam attrMap pred' -> Update nam attrMap <$> processRestrictionPredicateExpr pred'

    AddInclusionDependency nam dep -> pure (AddInclusionDependency nam dep)
    RemoveInclusionDependency nam -> pure (RemoveInclusionDependency nam)
    AddNotification nam exprA exprB exprC -> pure (AddNotification nam exprA exprB exprC)
    RemoveNotification nam -> pure (RemoveNotification nam)
    AddTypeConstructor tyDef consDefs -> pure (AddTypeConstructor tyDef consDefs)
    RemoveTypeConstructor tyName -> pure (RemoveTypeConstructor tyName)

    RemoveAtomFunction aFuncName -> pure (RemoveAtomFunction aFuncName)
    RemoveDatabaseContextFunction funcName' -> pure (RemoveDatabaseContextFunction funcName')
    ExecuteDatabaseContextFunction funcName' atomExprs -> ExecuteDatabaseContextFunction funcName' <$> mapM processAtomExpr atomExprs
    AddRegisteredQuery n q -> pure (AddRegisteredQuery n q)
    RemoveRegisteredQuery n -> pure (RemoveRegisteredQuery n)
    AlterACL e -> pure (AlterACL e)
    MultipleExpr exprs -> MultipleExpr <$> mapM processDatabaseContextExpr exprs

processDatabaseContextIOExpr :: DatabaseContextIOExpr -> ProcessExprM GraphRefDatabaseContextIOExpr
processDatabaseContextIOExpr (AddAtomFunction f tcs sc) =
  pure (AddAtomFunction f tcs sc)
processDatabaseContextIOExpr (LoadAtomFunctions mod' fun file) =
  pure (LoadAtomFunctions mod' fun file)
processDatabaseContextIOExpr (AddDatabaseContextFunction mod' fun path) =
  pure (AddDatabaseContextFunction mod' fun path)
processDatabaseContextIOExpr (LoadDatabaseContextFunctions mod' fun path) =
  pure (LoadDatabaseContextFunctions mod' fun path)
processDatabaseContextIOExpr (LoadModuleWithFunctions modPath) = pure (LoadModuleWithFunctions modPath)
processDatabaseContextIOExpr (CreateArbitraryRelation rvName attrExprs range) =
  CreateArbitraryRelation rvName <$> mapM processAttributeExpr attrExprs <*> pure range
  
processRestrictionPredicateExpr :: RestrictionPredicateExpr -> ProcessExprM GraphRefRestrictionPredicateExpr
processRestrictionPredicateExpr TruePredicate = pure TruePredicate
processRestrictionPredicateExpr (AndPredicate a b) = AndPredicate <$> processRestrictionPredicateExpr a <*> processRestrictionPredicateExpr b
processRestrictionPredicateExpr (OrPredicate a b) = OrPredicate <$> processRestrictionPredicateExpr a <*> processRestrictionPredicateExpr b
processRestrictionPredicateExpr (NotPredicate a) = NotPredicate <$> processRestrictionPredicateExpr a
processRestrictionPredicateExpr (RelationalExprPredicate expr) =
  RelationalExprPredicate <$> processRelationalExpr expr
processRestrictionPredicateExpr (AtomExprPredicate expr) =
  AtomExprPredicate <$> processAtomExpr expr
processRestrictionPredicateExpr (AttributeEqualityPredicate nam expr) =
  AttributeEqualityPredicate nam <$> processAtomExpr expr

processExtendTupleExpr :: ExtendTupleExpr -> ProcessExprM GraphRefExtendTupleExpr
processExtendTupleExpr (AttributeExtendTupleExpr nam atomExpr) =
  AttributeExtendTupleExpr nam <$> processAtomExpr atomExpr

processAtomExpr :: AtomExpr -> ProcessExprM GraphRefAtomExpr
processAtomExpr (AttributeAtomExpr nam) = pure $ AttributeAtomExpr nam
processAtomExpr (SubrelationAttributeAtomExpr relAttr subAttr) = pure (SubrelationAttributeAtomExpr relAttr subAttr)
processAtomExpr (NakedAtomExpr atom) = pure $ NakedAtomExpr atom
processAtomExpr (FunctionAtomExpr fName atomExprs ()) =
  FunctionAtomExpr fName <$> mapM processAtomExpr atomExprs  <*> askMarker
processAtomExpr (RelationAtomExpr expr) = RelationAtomExpr <$> processRelationalExpr expr
processAtomExpr (IfThenAtomExpr ifE thenE elseE) =
  IfThenAtomExpr <$> processAtomExpr ifE <*> processAtomExpr thenE <*> processAtomExpr elseE
processAtomExpr (ConstructedAtomExpr dConsName atomExprs ()) = ConstructedAtomExpr dConsName <$> mapM processAtomExpr atomExprs <*> askMarker

processTupleExprs :: TupleExprs -> ProcessExprM GraphRefTupleExprs
processTupleExprs (TupleExprs () tupleExprs) = do
  marker <- askMarker
  TupleExprs marker <$> mapM processTupleExpr tupleExprs
  
processTupleExpr :: TupleExpr -> ProcessExprM GraphRefTupleExpr
processTupleExpr (TupleExpr tMap) =
  TupleExpr . M.fromList <$> mapM (\(k,v) -> (,) k <$> processAtomExpr v) (M.toList tMap)

--convert AttributeExpr to GraphRefAttributeExpr
processAttributeExpr :: AttributeExpr -> ProcessExprM GraphRefAttributeExpr
processAttributeExpr (AttributeAndTypeNameExpr nam tCons ()) =
  AttributeAndTypeNameExpr nam tCons <$> askMarker
processAttributeExpr (NakedAttributeExpr attr) = pure $ NakedAttributeExpr attr

-}
