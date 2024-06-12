-- | Functions to convert all types of expresions into their GraphRef- equivalents.
module ProjectM36.NormalizeExpr where
import ProjectM36.Base
import Control.Monad.Trans.Reader as R
import qualified Data.Map as M

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
processRelationalExpr (MakeStaticRelation attrs tupSet) = pure (MakeStaticRelation attrs tupSet)
processRelationalExpr (ExistingRelation rel) = pure (ExistingRelation rel)
--requires current trans id and graph
processRelationalExpr (RelationValuedAttribute attrName) = pure (RelationValuedAttribute attrName)
processRelationalExpr (RelationVariable rv ()) = RelationVariable rv <$> askMarker
processRelationalExpr (Project attrNames expr) = Project <$> processAttributeNames attrNames <*> processRelationalExpr expr
processRelationalExpr (Union exprA exprB) = Union <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Join exprA exprB) = Join <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Rename attrs expr) =
  Rename attrs <$> processRelationalExpr expr
processRelationalExpr (Difference exprA exprB) = Difference <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Group attrNames attrName expr) = Group <$> processAttributeNames attrNames <*> pure attrName <*> processRelationalExpr expr
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

processAttributeNames :: AttributeNames -> ProcessExprM GraphRefAttributeNames
processAttributeNames (AttributeNames nameSet) = pure $ AttributeNames nameSet
processAttributeNames (InvertedAttributeNames attrNameSet) =
  pure $ InvertedAttributeNames attrNameSet
processAttributeNames (UnionAttributeNames attrNamesA attrNamesB) = UnionAttributeNames <$> processAttributeNames attrNamesA <*> processAttributeNames attrNamesB
processAttributeNames (IntersectAttributeNames attrNamesA attrNamesB) = IntersectAttributeNames <$> processAttributeNames attrNamesA <*> processAttributeNames attrNamesB
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
processAtomExpr (NakedAtomExpr atom) = pure $ NakedAtomExpr atom
processAtomExpr (FunctionAtomExpr fName atomExprs aggInfo ()) =
  FunctionAtomExpr fName <$> mapM processAtomExpr atomExprs  <*> pure aggInfo <*> askMarker 
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

