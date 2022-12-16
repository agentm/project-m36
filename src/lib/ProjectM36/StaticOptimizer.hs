{-# LANGUAGE FlexibleInstances, TypeFamilies, MultiParamTypeClasses, RankNTypes #-}

module ProjectM36.StaticOptimizer where
import ProjectM36.Base
import ProjectM36.GraphRefRelationalExpr
import ProjectM36.Relation
import qualified ProjectM36.TupleSet as TS
import ProjectM36.RelationalExpression
import ProjectM36.TransGraphRelationalExpression as TGRE hiding (askGraph)
import ProjectM36.Error
import ProjectM36.Transaction
import ProjectM36.NormalizeExpr
import qualified ProjectM36.Attribute as A
import qualified ProjectM36.AttributeNames as AS
import ProjectM36.Streaming.RelationalExpression
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.Set as S

-- the static optimizer performs optimizations which need not take any specific-relation statistics into account

data GraphRefSOptRelationalExprEnv =
  GraphRefSOptRelationalExprEnv
  {
    ore_graph :: TransactionGraph,
    ore_mcontext :: Maybe DatabaseContext
  }
  
type GraphRefSOptRelationalExprM a = ReaderT GraphRefSOptRelationalExprEnv (ExceptT RelationalError Identity) a

data GraphRefSOptDatabaseContextExprEnv =
  GraphRefSOptDatabaseContextExprEnv
  {
    odce_graph :: TransactionGraph,
    odce_context :: DatabaseContext, --not optional for DatabaseContextExpr evaluation
    odce_transId :: TransactionId -- parent if context is committed- needed because MultipleExpr optimization requires running the DatabaseContextExprs (with empty relvars)
    }

type GraphRefSOptDatabaseContextExprM a = ReaderT GraphRefSOptDatabaseContextExprEnv (ExceptT RelationalError Identity) a

class Optimize expr optExpr where
  type OptimizeEnv expr optExpr
  
  optimize :: OptimizeEnv expr optExpr -> expr -> Either RelationalError optExpr


instance Optimize RelationalExpr GraphRefRelationalExpr where
  type OptimizeEnv RelationalExpr GraphRefRelationalExpr = RelationalExprEnv
  
  optimize env expr = do
    let gfExpr = runProcessExprM UncommittedContextMarker (processRelationalExpr expr) -- references parent tid instead of context! options- I could add the context to the graph with a new transid or implement an evalRelationalExpr in RE.hs to use the context (which is what I had previously)
        ctx = re_context env
    runGraphRefSOptRelationalExprM (Just ctx) (re_graph env) (fullOptimizeGraphRefRelationalExpr gfExpr)

instance Optimize DatabaseContextExpr GraphRefDatabaseContextExpr where
  type OptimizeEnv DatabaseContextExpr GraphRefDatabaseContextExpr = GraphRefSOptDatabaseContextExprEnv
  optimize env expr = do
    let gfExpr = runProcessExprM UncommittedContextMarker (processDatabaseContextExpr expr)
        graph = odce_graph env
        transId = odce_transId env
        ctx = odce_context env
    runGraphRefSOptDatabaseContextExprM transId ctx graph (optimizeGraphRefDatabaseContextExpr gfExpr)

-- | Apply pure optimizations.
optimizeAndEvalRelationalExpr :: RelationalExprEnv -> RelationalExpr -> Either RelationalError Relation
optimizeAndEvalRelationalExpr env expr = do
  let gfExpr = runProcessExprM UncommittedContextMarker (processRelationalExpr expr) -- references parent tid instead of context! options- I could add the context to the graph with a new transid or implement an evalRelationalExpr in RE.hs to use the context (which is what I had previously)
      graph = re_graph env
      ctx = re_context env
      gfEnv = freshGraphRefRelationalExprEnv (Just ctx) graph
  optExpr <- runGraphRefSOptRelationalExprM (Just ctx) (re_graph env) (fullOptimizeGraphRefRelationalExpr gfExpr)
  runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr optExpr)

-- | Uses streamly interface for parallel execution.
optimizeAndEvalRelationalExpr' :: RelationalExprEnv -> RelationalExpr -> IO (Either RelationalError Relation)
optimizeAndEvalRelationalExpr' env expr = do
  let gfExpr = runProcessExprM UncommittedContextMarker (processRelationalExpr expr) -- references parent tid instead of context! options- I could add the context to the graph with a new transid or implement an evalRelationalExpr in RE.hs to use the context (which is what I had previously)
      graph = re_graph env
      ctx = re_context env
      gfEnv = freshGraphRefRelationalExprEnv (Just ctx) graph
  --first, type check
  case runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr) of
    Left err -> pure (Left err)
    Right _ -> do
      --then, optimize
      case runGraphRefSOptRelationalExprM (Just ctx) (re_graph env) (fullOptimizeGraphRefRelationalExpr gfExpr) of
        Left err -> pure (Left err)
        Right optGfExpr -> 
          case planGraphRefRelationalExpr (traceShow ("optEval", optGfExpr) optGfExpr) gfEnv of
            Left err -> pure (Left err)
            Right plan ->
              case executePlan plan mempty of -- try/catch to handle exceptions
                Left err -> pure (Left err)
                Right resultStream ->
                  --convert tuple stream into relation
                  pure <$> streamRelationAsRelation resultStream

class Monad m => AskGraphContext m where
  askGraph :: m TransactionGraph
  askContext :: m DatabaseContext

instance AskGraphContext (ReaderT GraphRefSOptDatabaseContextExprEnv (ExceptT RelationalError Identity)) where
  askGraph = asks odce_graph
  askContext = asks odce_context 

instance AskGraphContext (ReaderT GraphRefSOptRelationalExprEnv (ExceptT RelationalError Identity)) where
  askGraph = asks ore_graph
  askContext = do
    mctx <- asks ore_mcontext
    case mctx of
      Nothing -> throwError NoUncommittedContextInEvalError
      Just ctx -> pure ctx

askTransId :: GraphRefSOptDatabaseContextExprM TransactionId
askTransId = asks odce_transId

askMaybeContext :: GraphRefSOptRelationalExprM (Maybe DatabaseContext)
askMaybeContext = asks ore_mcontext

optimizeDatabaseContextExpr :: DatabaseContextExpr -> GraphRefSOptDatabaseContextExprM GraphRefDatabaseContextExpr
optimizeDatabaseContextExpr expr = do
  let gfExpr = runProcessExprM UncommittedContextMarker (processDatabaseContextExpr expr)
  optimizeGraphRefDatabaseContextExpr gfExpr
  
optimizeAndEvalDatabaseContextExpr :: Bool -> DatabaseContextExpr -> DatabaseContextEvalMonad ()
optimizeAndEvalDatabaseContextExpr optimize expr = do
  graph <- asks dce_graph
  transId <- asks dce_transId
  context <- getStateContext
  let gfExpr = runProcessExprM UncommittedContextMarker (processDatabaseContextExpr expr)
      eOptExpr = if optimize then
                   runGraphRefSOptDatabaseContextExprM transId context graph (optimizeGraphRefDatabaseContextExpr gfExpr)
                   else
                   pure gfExpr
  case eOptExpr of
    Left err -> throwError err
    Right optExpr -> evalGraphRefDatabaseContextExpr optExpr


optimizeAndEvalTransGraphRelationalExpr :: TransactionGraph -> TransGraphRelationalExpr -> Either RelationalError Relation
optimizeAndEvalTransGraphRelationalExpr graph tgExpr = do
  gfExpr <- TGRE.process (TransGraphEvalEnv graph) tgExpr
  optExpr <- runGraphRefSOptRelationalExprM Nothing graph (fullOptimizeGraphRefRelationalExpr gfExpr)
  let gfEnv = freshGraphRefRelationalExprEnv Nothing graph
  runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr optExpr)

optimizeAndEvalDatabaseContextIOExpr :: DatabaseContextIOExpr -> DatabaseContextIOEvalMonad (Either RelationalError ())
optimizeAndEvalDatabaseContextIOExpr expr = do
  transId <- asks dbcio_transId
  ctx <- getDBCIOContext
  graph <- asks dbcio_graph
  let gfExpr = runProcessExprM UncommittedContextMarker (processDatabaseContextIOExpr expr)
      eOptExpr = runGraphRefSOptDatabaseContextExprM transId ctx graph (optimizeDatabaseContextIOExpr gfExpr)
  case eOptExpr of
    Left err -> pure (Left err)
    Right optExpr ->
      evalGraphRefDatabaseContextIOExpr optExpr

{-  
runStaticOptimizerMonad :: RelationalExprEnv -> StaticOptimizerMonad a -> Either RelationalError a
runStaticOptimizerMonad env m = runIdentity (runExceptT (runReaderT m env))
-}

runGraphRefSOptRelationalExprM ::
  Maybe DatabaseContext ->  
  TransactionGraph ->
  GraphRefSOptRelationalExprM a ->
  Either RelationalError a
runGraphRefSOptRelationalExprM mctx graph m = runIdentity (runExceptT (runReaderT m env))
  where
    env = GraphRefSOptRelationalExprEnv {
      ore_graph = graph,
      ore_mcontext = mctx
      }
  

runGraphRefSOptDatabaseContextExprM ::
  TransactionId ->
  DatabaseContext ->
  TransactionGraph ->
  GraphRefSOptDatabaseContextExprM a ->
  Either RelationalError a
runGraphRefSOptDatabaseContextExprM tid ctx graph m =
  runIdentity (runExceptT (runReaderT m env))
  where
    env = GraphRefSOptDatabaseContextExprEnv {
      odce_graph = graph,
      odce_context = ctx,
      odce_transId = tid
      }

optimizeGraphRefRelationalExpr' ::
  Maybe DatabaseContext ->
  TransactionGraph ->
  GraphRefRelationalExpr ->
  Either RelationalError GraphRefRelationalExpr
optimizeGraphRefRelationalExpr' mctx graph expr =
  runIdentity (runExceptT (runReaderT (optimizeGraphRefRelationalExpr expr) env))
  where
    env = GraphRefSOptRelationalExprEnv {
      ore_graph = graph,
      ore_mcontext = mctx
      }

-- | optimize relational expression within database context expr monad
liftGraphRefRelExpr :: GraphRefSOptRelationalExprM a -> GraphRefSOptDatabaseContextExprM a
liftGraphRefRelExpr m = do
  context <- asks odce_context
  graph <- asks odce_graph
  lift $ except $ runGraphRefSOptRelationalExprM (Just context) graph m
  
fullOptimizeGraphRefRelationalExpr :: GraphRefRelationalExpr -> GraphRefSOptRelationalExprM GraphRefRelationalExpr
fullOptimizeGraphRefRelationalExpr expr = do
  optExpr <- optimizeGraphRefRelationalExpr expr
  let optExpr' = applyStaticRestrictionPushdown (applyStaticRestrictionCollapse optExpr)
  applyStaticJoinElimination optExpr'

-- apply optimizations which merely remove steps to become no-ops: example: projection of a relation across all of its attributes => original relation

--should optimizations offer the possibility to pure errors? If they perform the up-front type-checking, maybe so
optimizeGraphRefRelationalExpr :: GraphRefRelationalExpr -> GraphRefSOptRelationalExprM GraphRefRelationalExpr
optimizeGraphRefRelationalExpr e@(MakeStaticRelation _ _) = pure e

optimizeGraphRefRelationalExpr e@MakeRelationFromExprs{} = pure e

optimizeGraphRefRelationalExpr e@(ExistingRelation _) = pure e

optimizeGraphRefRelationalExpr e@(RelationVariable _ _) = pure e
  
--remove project of attributes which removes no attributes
optimizeGraphRefRelationalExpr (Project attrNameSet expr) = do
  graph <- askGraph
  mctx <- askMaybeContext
  let relType = runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr expr)
      gfEnv = freshGraphRefRelationalExprEnv mctx graph
  case relType of
    Left err -> throwError err
    Right relType2 
      | AS.all == attrNameSet ->        
        optimizeGraphRefRelationalExpr expr
      | AttributeNames (attributeNames relType2) == attrNameSet ->
        optimizeGraphRefRelationalExpr expr
      | otherwise -> do
        optSubExpr <- optimizeGraphRefRelationalExpr expr 
        pure (Project attrNameSet optSubExpr)
                           
optimizeGraphRefRelationalExpr (Union exprA exprB) = do
  optExprA <- optimizeGraphRefRelationalExpr exprA
  optExprB <- optimizeGraphRefRelationalExpr exprB
  -- (x where pred1) union (x where pred2) -> (x where pred1 or pred2)
  traceShowM ("union opt", optExprA, isEmptyRelationExpr optExprA, optExprB, isEmptyRelationExpr optExprB)
  case (optExprA, optExprB) of 
          (Restrict predA (RelationVariable nameA sA),
           Restrict predB (RelationVariable nameB sB)) | nameA == nameB && sA == sB -> pure (Restrict (AndPredicate predA predB) (RelationVariable nameA sA))
          (exprA', exprB') | isEmptyRelationExpr exprA' -> pure exprB'
                           | isEmptyRelationExpr exprB' -> pure exprA'
          _ -> if optExprA == optExprB then           
            pure optExprA
            else
            pure $ Union optExprA optExprB
                            
optimizeGraphRefRelationalExpr (Join exprA exprB) = do
  optExprA <- optimizeGraphRefRelationalExpr exprA
  optExprB <- optimizeGraphRefRelationalExpr exprB
  -- if the relvars to join are the same but with predicates, then just AndPredicate the predicates
  case (optExprA, optExprB) of
          (Restrict predA (RelationVariable nameA sA),
           Restrict predB (RelationVariable nameB sB)) | nameA == nameB && sA == sB -> pure (Restrict  (AndPredicate predA predB) (RelationVariable nameA sA))
          _ -> if optExprA == optExprB then --A join A == A
                           pure optExprA
                         else
                           pure (Join optExprA optExprB)
                           
optimizeGraphRefRelationalExpr (Difference exprA exprB) = do
  graph <- askGraph
  context <- askMaybeContext
  optExprA <- optimizeGraphRefRelationalExpr exprA
  optExprB <- optimizeGraphRefRelationalExpr exprB
  if optExprA == optExprB then do --A difference A == A where false
    let eEmptyRel = runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr optExprA)
        gfEnv = freshGraphRefRelationalExprEnv context graph
    case eEmptyRel of
      Left err -> throwError err
      Right emptyRel -> pure (ExistingRelation emptyRel)
    else
    pure (Difference optExprA optExprB)
                           
optimizeGraphRefRelationalExpr e@Rename{} = pure e

optimizeGraphRefRelationalExpr (Group oldAttrNames newAttrName expr) =
  pure $ Group oldAttrNames newAttrName expr
  
optimizeGraphRefRelationalExpr (Ungroup attrName expr) =
  pure $ Ungroup attrName expr
  
--remove restriction of nothing
optimizeGraphRefRelationalExpr (Restrict predicate expr) = do
  graph <- askGraph
  mctx <- askMaybeContext
  optimizedPredicate <- applyStaticPredicateOptimization predicate
  case optimizedPredicate of
    optimizedPredicate' | isTrueExpr optimizedPredicate' -> optimizeGraphRefRelationalExpr expr -- remove predicate entirely
    optimizedPredicate' | isFalseExpr optimizedPredicate' -> do -- replace where false predicate with empty relation with attributes from relexpr
        let attributesRel = runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr expr)
            gfEnv = freshGraphRefRelationalExprEnv mctx graph
        case attributesRel of 
          Left err -> throwError err
          Right attributesRelA -> pure $ MakeStaticRelation (attributes attributesRelA) TS.empty
      | otherwise -> do
        optSubExpr <- optimizeGraphRefRelationalExpr expr
        pure $ Restrict optimizedPredicate' optSubExpr
  
optimizeGraphRefRelationalExpr e@(Equals _ _) = pure e 

optimizeGraphRefRelationalExpr e@(NotEquals _ _) = pure e 
  
optimizeGraphRefRelationalExpr e@(Extend _ _) = pure e  

optimizeGraphRefRelationalExpr e@(With _ _) = pure e  

-- database context expr
optimizeGraphRefDatabaseContextExpr :: GraphRefDatabaseContextExpr -> GraphRefSOptDatabaseContextExprM GraphRefDatabaseContextExpr
optimizeGraphRefDatabaseContextExpr x@NoOperation = pure x
optimizeGraphRefDatabaseContextExpr x@(Define _ _) = pure x

optimizeGraphRefDatabaseContextExpr x@(Undefine _) = pure x

optimizeGraphRefDatabaseContextExpr (Assign name expr) = do
  optExpr <- liftGraphRefRelExpr (fullOptimizeGraphRefRelationalExpr expr)
  pure $ Assign name optExpr
    
optimizeGraphRefDatabaseContextExpr (Insert targetName expr) = do
  optimizedExpr <- liftGraphRefRelExpr (fullOptimizeGraphRefRelationalExpr expr)
  if isEmptyRelationExpr optimizedExpr then -- if we are trying to insert an empty relation, do nothing
    pure NoOperation
    else 
    case optimizedExpr of 
      -- if the target relvar and the insert relvar are the same, there is nothing to do
      -- insert s s -> NoOperation
      RelationVariable insName _ | insName == targetName -> pure NoOperation
      _ -> pure (Insert targetName optimizedExpr)
  
optimizeGraphRefDatabaseContextExpr (Delete name predicate) =
  Delete name <$> liftGraphRefRelExpr (applyStaticPredicateOptimization predicate)

optimizeGraphRefDatabaseContextExpr (Update name upmap predicate) =
  Update name upmap <$> liftGraphRefRelExpr (applyStaticPredicateOptimization predicate)
      
optimizeGraphRefDatabaseContextExpr dep@(AddInclusionDependency _ _) = pure dep

optimizeGraphRefDatabaseContextExpr (RemoveInclusionDependency name) = pure (RemoveInclusionDependency name)

optimizeGraphRefDatabaseContextExpr (AddNotification name triggerExpr resultOldExpr resultNewExpr) = 
  --we can't optimize these expressions until they run
  pure (AddNotification name triggerExpr resultOldExpr resultNewExpr)

optimizeGraphRefDatabaseContextExpr notif@(RemoveNotification _) = pure notif

optimizeGraphRefDatabaseContextExpr c@(AddTypeConstructor _ _) = pure c
optimizeGraphRefDatabaseContextExpr c@(RemoveTypeConstructor _) = pure c
optimizeGraphRefDatabaseContextExpr c@(RemoveAtomFunction _) = pure c
optimizeGraphRefDatabaseContextExpr c@(RemoveDatabaseContextFunction _) = pure c
optimizeGraphRefDatabaseContextExpr c@(ExecuteDatabaseContextFunction _ _) = pure c
optimizeGraphRefDatabaseContextExpr c@AddRegisteredQuery{} = pure c
optimizeGraphRefDatabaseContextExpr c@RemoveRegisteredQuery{} = pure c
--optimization: from pgsql lists- check for join condition referencing foreign key- if join projection project away the referenced table, then it does not need to be scanned

--applyStaticDatabaseOptimization (MultipleExpr exprs) = pure $ Right $ MultipleExpr exprs
--for multiple expressions, we must evaluate
optimizeGraphRefDatabaseContextExpr (MultipleExpr exprs) = do
  --a previous expression in the exprs list could create a relvar; we don't want to miss it, so we clear the tuples and execute the expression to get an empty relation in the relvar
  context <- askContext
  graph <- askGraph
  parentId <- askTransId
  
  let emptyRvs ctx = ctx { relationVariables = mkEmptyRelVars (relationVariables ctx) }
      dbcEnv = mkDatabaseContextEvalEnv parentId graph
      folder (ctx, expracc) expr = do
        --optimize the expr and run it against empty relvars to add it to the context
        case runGraphRefSOptDatabaseContextExprM parentId ctx graph (optimizeGraphRefDatabaseContextExpr expr) of
          Left err -> throwError err
          Right optExpr ->
            case runDatabaseContextEvalMonad ctx dbcEnv (evalGraphRefDatabaseContextExpr optExpr) of
              Left err -> throwError err
              Right dbcState ->
                pure (emptyRvs $ dbc_context dbcState, expracc ++ [optExpr])

  (_, exprs') <- foldM folder (context,[]) exprs
  pure (MultipleExpr exprs')

applyStaticPredicateOptimization :: GraphRefRestrictionPredicateExpr -> GraphRefSOptRelationalExprM GraphRefRestrictionPredicateExpr
applyStaticPredicateOptimization predi = do
  optPred <- case predi of 
-- where x and x => where x
    AndPredicate pred1 pred2 -> do
      optPredA <- applyStaticPredicateOptimization pred1
      optPredB <- applyStaticPredicateOptimization pred2
      if optPredA == optPredB then
        pure optPredA
        else
        pure (AndPredicate optPredA optPredB)
-- where x or x => where x    
    OrPredicate pred1 pred2 -> do
      optPredA <- applyStaticPredicateOptimization pred1
      optPredB <- applyStaticPredicateOptimization pred2
      if (optPredA == optPredB) || isTrueExpr optPredA then
        pure optPredA
        else if isTrueExpr optPredB then
               pure optPredB
             else
               pure (OrPredicate optPredA optPredB)
    AttributeEqualityPredicate attrNameA (AttributeAtomExpr attrNameB) ->
      if attrNameA == attrNameB then
        pure TruePredicate
      else
        pure predi
    AttributeEqualityPredicate{} -> pure predi
    TruePredicate -> pure predi
    NotPredicate{} -> pure predi
    RelationalExprPredicate{} -> pure predi
    AtomExprPredicate{} -> pure predi
  let attrMap = findStaticRestrictionPredicates optPred
  pure (replaceStaticAtomExprs optPred attrMap)

--determines if an atom expression is tautologically true
isTrueExpr :: RestrictionPredicateExprBase a -> Bool
isTrueExpr TruePredicate = True
isTrueExpr (AtomExprPredicate (NakedAtomExpr (BoolAtom True))) = True
isTrueExpr _ = False

--determines if an atom expression is tautologically false
isFalseExpr :: RestrictionPredicateExprBase a -> Bool
isFalseExpr (NotPredicate expr) = isTrueExpr expr
isFalseExpr (AtomExprPredicate (NakedAtomExpr (BoolAtom False))) = True
isFalseExpr _ = False
    
--transitive static variable optimization                        
replaceStaticAtomExprs :: GraphRefRestrictionPredicateExpr -> M.Map AttributeName GraphRefAtomExpr -> GraphRefRestrictionPredicateExpr
replaceStaticAtomExprs predIn replaceMap = case predIn of
  AttributeEqualityPredicate newAttrName (AttributeAtomExpr matchName) -> case M.lookup matchName replaceMap of
    Nothing -> predIn
    Just newVal -> AttributeEqualityPredicate newAttrName newVal
  AttributeEqualityPredicate{} -> predIn
  AndPredicate pred1 pred2 -> AndPredicate (replaceStaticAtomExprs pred1 replaceMap) (replaceStaticAtomExprs pred2 replaceMap)
  OrPredicate pred1 pred2 -> OrPredicate (replaceStaticAtomExprs pred1 replaceMap) (replaceStaticAtomExprs pred2 replaceMap)
  NotPredicate pred1 -> NotPredicate (replaceStaticAtomExprs pred1 replaceMap)
  TruePredicate -> predIn
  RelationalExprPredicate{} -> predIn
  AtomExprPredicate{} -> predIn
-- used for transitive attribute optimization- only works on statically-determined atoms for now- in the future, this could work for all AtomExprs which don't reference attributes
findStaticRestrictionPredicates :: GraphRefRestrictionPredicateExpr -> M.Map AttributeName GraphRefAtomExpr
findStaticRestrictionPredicates (AttributeEqualityPredicate attrName atomExpr) = 
  case atomExpr of
    val@NakedAtomExpr{} -> M.singleton attrName val
    val@ConstructedAtomExpr{} -> M.singleton attrName val
    _ -> M.empty

findStaticRestrictionPredicates (AndPredicate pred1 pred2) = 
  M.union (findStaticRestrictionPredicates pred1) (findStaticRestrictionPredicates pred2) 
findStaticRestrictionPredicates (OrPredicate pred1 pred2) =
  M.union (findStaticRestrictionPredicates pred1) (findStaticRestrictionPredicates pred2)
findStaticRestrictionPredicates (NotPredicate predi) = findStaticRestrictionPredicates predi
findStaticRestrictionPredicates TruePredicate = M.empty
findStaticRestrictionPredicates RelationalExprPredicate{} = M.empty
findStaticRestrictionPredicates AtomExprPredicate{} = M.empty

isStaticAtomExpr :: AtomExpr -> Bool
isStaticAtomExpr NakedAtomExpr{} = True
isStaticAtomExpr ConstructedAtomExpr{} = True
isStaticAtomExpr AttributeAtomExpr{} = False
isStaticAtomExpr FunctionAtomExpr{} = False
isStaticAtomExpr RelationAtomExpr{} = False

--if the projection of a join only uses the attributes from one of the expressions and there is a foreign key relationship between the expressions, we know that the join is inconsequential and can be removed
applyStaticJoinElimination :: GraphRefRelationalExpr -> GraphRefSOptRelationalExprM GraphRefRelationalExpr
applyStaticJoinElimination expr@(Project attrNameSet (Join exprA exprB)) = do
  graph <- askGraph
  case inSameTransaction exprA exprB of
    -- the sub exprs are in different transactions or none at all, so we cannot extract inclusion dependencies across transaction boundaries
    Nothing -> pure expr
    Just marker -> do
      commonContext <- case marker of
                         UncommittedContextMarker -> askContext
                         TransactionMarker tid -> concreteDatabaseContext <$> lift (except (transactionForId tid graph))
      let typeForExpr e = lift $ except $ runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr e)
          gfEnv = freshGraphRefRelationalExprEnv (Just commonContext) graph
        
      projType <- typeForExpr expr
      typeA <- typeForExpr exprA
      typeB <- typeForExpr exprB
      let matchesProjectionAttributes 
            | attrNames projType `S.isSubsetOf` attrNames typeA =
                Just ((exprA, typeA), (exprB, typeB))
            | attrNames projType `S.isSubsetOf` attrNames typeB =
                Just ((exprB, typeB), (exprA, typeA))
            | otherwise =
                Nothing
          attrNames = A.attributeNameSet . attributes
      case matchesProjectionAttributes of
        Nothing ->  -- this optimization does not apply
          pure expr
        Just ((joinedExpr, joinedType), (unjoinedExpr, _)) -> do
        --lookup transaction
        --scan inclusion dependencies for a foreign key relationship
         let incDeps = inclusionDependencies commonContext
             fkConstraint = foldM isFkConstraint False incDeps
            --search for matching fk constraint
             isFkConstraint acc (InclusionDependency (Project subAttrNames subrv) (Project _ superrv)) = do
               let gfSubAttrNames = processM (processAttributeNames subAttrNames)
                   gfSubRv = processM (processRelationalExpr subrv)
                   gfSuperRv = processM (processRelationalExpr superrv)
                   processM :: forall a. ProcessExprM a -> a
                   processM = runProcessExprM marker
               case runGraphRefRelationalExprM gfEnv (evalGraphRefAttributeNames gfSubAttrNames expr) of
                 Left _ -> pure acc
                 Right subAttrNameSet -> 
                   pure (acc || (joinedExpr == gfSubRv &&
                                 unjoinedExpr == gfSuperRv && 
                                 -- the fk attribute is one of the projection attributes
                                 A.attributeNamesContained subAttrNameSet (A.attributeNameSet (attributes joinedType))
                                ))
             isFkConstraint acc _ = pure acc
         case fkConstraint of
           Right True -> --join elimination optimization applies
             optimizeGraphRefRelationalExpr (Project attrNameSet joinedExpr)
           Right False -> --join elimination optimization does not apply
             pure expr
           Left err -> throwError err
          
applyStaticJoinElimination expr = pure expr
                                                                              
--restriction collapse converts chained restrictions into (Restrict (And pred1 pred2 pred3...))
  --this optimization should be fairly uncontroversial- performing a tuple scan once is cheaper than twice- parallelization can still take place
applyStaticRestrictionCollapse :: GraphRefRelationalExpr -> GraphRefRelationalExpr
applyStaticRestrictionCollapse expr = 
  case expr of
    MakeRelationFromExprs _ _ -> expr
    MakeStaticRelation _ _ -> expr
    ExistingRelation _ -> expr
    RelationVariable _ _ -> expr
    With _ _ -> expr
    Project attrs subexpr -> 
      Project attrs (applyStaticRestrictionCollapse subexpr)
    Union sub1 sub2 ->
      Union (applyStaticRestrictionCollapse sub1) (applyStaticRestrictionCollapse sub2)    
    Join sub1 sub2 ->
      Join (applyStaticRestrictionCollapse sub1) (applyStaticRestrictionCollapse sub2)
    Rename n1 n2 sub -> 
      Rename n1 n2 (applyStaticRestrictionCollapse sub)
    Difference sub1 sub2 -> 
      Difference (applyStaticRestrictionCollapse sub1) (applyStaticRestrictionCollapse sub2)
    Group n1 n2 sub ->
      Group n1 n2 (applyStaticRestrictionCollapse sub)
    Ungroup n1 sub ->
      Ungroup n1 (applyStaticRestrictionCollapse sub)
    Equals sub1 sub2 -> 
      Equals (applyStaticRestrictionCollapse sub1) (applyStaticRestrictionCollapse sub2)
    NotEquals sub1 sub2 ->
      NotEquals (applyStaticRestrictionCollapse sub1) (applyStaticRestrictionCollapse sub2)
    Extend n sub ->
      Extend n (applyStaticRestrictionCollapse sub)
    Restrict firstPred _ ->
      let restrictions = sequentialRestrictions expr
          finalExpr = last restrictions
          optFinalExpr = case finalExpr of
                              Restrict _ subexpr -> applyStaticRestrictionCollapse subexpr
                              otherExpr -> otherExpr
          andPreds = foldr folder firstPred (tail restrictions)
          folder (Restrict subpred _) acc = AndPredicate acc subpred
          folder _ _ = error "unexpected restriction expression in optimization phase"
      in
      Restrict andPreds optFinalExpr
      
sequentialRestrictions :: RelationalExprBase a -> [RelationalExprBase a]
sequentialRestrictions expr@(Restrict _ subexpr) = expr:sequentialRestrictions subexpr
sequentialRestrictions _ = []

--restriction pushdown only really makes sense for tuple-oriented storage schemes where performing a restriction before projection can cut down on the intermediate storage needed to store the data before the projection
-- x{proj} where c1 -> (x where c1){proj} #project on fewer tuples
-- (x union y) where c -> (x where c) union (y where c) #with a selective restriction, fewer tuples will need to be joined
applyStaticRestrictionPushdown :: GraphRefRelationalExpr -> GraphRefRelationalExpr
applyStaticRestrictionPushdown expr = case expr of
  MakeRelationFromExprs _ _ -> expr
  MakeStaticRelation _ _ -> expr
  ExistingRelation _ -> expr
  RelationVariable _ _ -> expr
  With _ _ -> expr
  Project _ _ -> expr
  --this transformation cannot be inverted because the projection attributes might not exist in the inverted version
  Restrict restrictAttrs (Project projAttrs subexpr) -> 
    Project projAttrs (Restrict restrictAttrs (applyStaticRestrictionPushdown subexpr))
  Restrict restrictAttrs (Union subexpr1 subexpr2) ->
    let optSub1 = applyStaticRestrictionPushdown subexpr1
        optSub2 = applyStaticRestrictionPushdown subexpr2 in
    Union (Restrict restrictAttrs optSub1) (Restrict restrictAttrs optSub2)
  Restrict attrs subexpr -> 
    Restrict attrs (applyStaticRestrictionPushdown subexpr)
    
  Union sub1 sub2 -> 
    Union (applyStaticRestrictionPushdown sub1) (applyStaticRestrictionPushdown sub2)
  Join sub1 sub2 ->
    Join (applyStaticRestrictionPushdown sub1) (applyStaticRestrictionPushdown sub2)
  Rename n1 n2 sub ->
    Rename n1 n2 (applyStaticRestrictionPushdown sub)
  Difference sub1 sub2 -> 
    Difference (applyStaticRestrictionPushdown sub1) (applyStaticRestrictionPushdown sub2)
  Group n1 n2 sub ->
    Group n1 n2 (applyStaticRestrictionPushdown sub)
  Ungroup n1 sub ->
    Ungroup n1 (applyStaticRestrictionPushdown sub)
  Equals sub1 sub2 -> 
    Equals (applyStaticRestrictionPushdown sub1) (applyStaticRestrictionPushdown sub2)
  NotEquals sub1 sub2 ->
    NotEquals (applyStaticRestrictionPushdown sub1) (applyStaticRestrictionPushdown sub2)
  Extend n sub ->
    Extend n (applyStaticRestrictionPushdown sub)
    
-- no optimizations available
optimizeDatabaseContextIOExpr :: GraphRefDatabaseContextIOExpr -> GraphRefSOptDatabaseContextExprM GraphRefDatabaseContextIOExpr
optimizeDatabaseContextIOExpr = pure


