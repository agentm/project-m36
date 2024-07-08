{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings  #-}
module ProjectM36.RelationalExpression where
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Base
import qualified Data.UUID as U
import ProjectM36.Error
import ProjectM36.AtomType
import ProjectM36.Attribute (emptyAttributes, attributesFromList)
import ProjectM36.ScriptSession
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunction
import ProjectM36.DatabaseContextFunction
import ProjectM36.Arbitrary
import ProjectM36.GraphRefRelationalExpr
import ProjectM36.Transaction
import ProjectM36.AggregateFunctions as Agg
import qualified ProjectM36.Attribute as A
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.Set as S
import Control.Monad.State hiding (join)
import Data.Bifunctor (second)
import Data.Maybe
import Data.Tuple (swap)
import Data.Either
import Data.Char (isUpper)
import Data.Time
import qualified Data.List.NonEmpty as NE
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified ProjectM36.TypeConstructorDef as TCD
import qualified Control.Monad.RWS.Strict as RWS
import Control.Monad.RWS.Strict (RWST, execRWST, runRWST)
import Control.Monad.Except hiding (join)
import Control.Monad.Trans.Except (except)
import Control.Monad.Reader as R hiding (join)
import ProjectM36.NormalizeExpr
import ProjectM36.WithNameExpr
import ProjectM36.Function
import Test.QuickCheck
import qualified Data.Functor.Foldable as Fold
import Control.Applicative
#ifdef PM36_HASKELL_SCRIPTING
import GHC hiding (getContext)
import Control.Exception
import GHC.Paths
#endif

import Debug.Trace

data DatabaseContextExprDetails = CountUpdatedTuples

databaseContextExprDetailsFunc :: DatabaseContextExprDetails -> ResultAccumFunc
databaseContextExprDetailsFunc CountUpdatedTuples _ relIn = Relation attrs newTups
  where
    attrs = A.attributesFromList [Attribute "count" IntAtomType]
    existingTuple = fromMaybe (error "impossible counting error in singletonTuple") (singletonTuple relIn)
    existingCount = case V.head (tupleAtoms existingTuple) of
      IntAtom v -> v
      _ -> error "impossible counting error in tupleAtoms"
    newTups = case mkTupleSetFromList attrs [[IntAtom (existingCount + 1)]] of
      Left err -> error ("impossible counting error in " ++ show err)
      Right ts -> ts
      
-- | Used to start a fresh database state for a new database context expression.
mkDatabaseContextEvalState :: DatabaseContext -> DatabaseContextEvalState
mkDatabaseContextEvalState context = DatabaseContextEvalState {
  dbc_context = context,
  dbc_accum = M.empty,
  dbc_dirty = False
  } --future work: propagate return accumulator

-- we need to pass around a higher level RelationTuple and Attributes in order to solve #52
data RelationalExprEnv = RelationalExprEnv {
  re_context :: DatabaseContext, 
  re_graph :: TransactionGraph,
  re_extra :: Maybe (Either RelationTuple Attributes)
  }

envTuple :: GraphRefRelationalExprEnv -> RelationTuple
envTuple e = fromLeft emptyTuple (fromMaybe (Left emptyTuple) (gre_extra e))

envAttributes :: GraphRefRelationalExprEnv -> Attributes
envAttributes e = fromRight emptyAttributes (fromMaybe (Right emptyAttributes) (gre_extra e))
  
instance Show RelationalExprEnv where
  show e@RelationalExprEnv{} = "RelationalExprEnv " ++ show (re_extra e)

--used to eval relationalexpr
type RelationalExprM a = ReaderT RelationalExprEnv (ExceptT RelationalError Identity) a

runRelationalExprM :: RelationalExprEnv -> RelationalExprM a -> Either RelationalError a
runRelationalExprM env m = runIdentity (runExceptT (runReaderT m env))

reGraph :: RelationalExprM TransactionGraph
reGraph = asks re_graph

reContext :: RelationalExprM DatabaseContext
reContext = asks re_context

mkRelationalExprEnv :: DatabaseContext -> TransactionGraph -> RelationalExprEnv
mkRelationalExprEnv ctx graph =
  RelationalExprEnv
  { re_context = ctx,
    re_graph = graph,
    re_extra = Nothing }

askEnv :: GraphRefRelationalExprM GraphRefRelationalExprEnv
askEnv = R.ask

mergeTuplesIntoGraphRefRelationalExprEnv :: RelationTuple -> GraphRefRelationalExprEnv -> GraphRefRelationalExprEnv
mergeTuplesIntoGraphRefRelationalExprEnv tupIn env =
  env { gre_extra = new_elems }
  where
    new_elems = Just (Left newTuple)
    mergedTupMap = M.union (tupleToMap tupIn) (tupleToMap (envTuple env))
    newTuple = mkRelationTupleFromMap mergedTupMap

mergeAttributesIntoGraphRefRelationalExprEnv :: Attributes -> GraphRefRelationalExprEnv -> GraphRefRelationalExprEnv
mergeAttributesIntoGraphRefRelationalExprEnv attrsIn e = e { gre_extra = newattrs }
  where
    newattrs = Just (Right (A.union attrsIn (envAttributes e)))

type ResultAccumName = StringType

type ResultAccumFunc = (RelationTuple -> Relation -> Relation) -> Relation -> Relation

data ResultAccum = ResultAccum { resultAccumFunc :: ResultAccumFunc,
                                 resultAccumResult :: Relation
                                 }

data DatabaseContextEvalState = DatabaseContextEvalState {
  dbc_context :: DatabaseContext, --new, alterable context for a new transaction
  dbc_accum :: M.Map ResultAccumName ResultAccum,
  dbc_dirty :: DirtyFlag
  }

data DatabaseContextEvalEnv = DatabaseContextEvalEnv
  { dce_transId :: TransactionId,
    dce_graph :: TransactionGraph
  }

mkDatabaseContextEvalEnv :: TransactionId -> TransactionGraph -> DatabaseContextEvalEnv
mkDatabaseContextEvalEnv = DatabaseContextEvalEnv

type DatabaseContextEvalMonad a = RWST DatabaseContextEvalEnv () DatabaseContextEvalState (ExceptT RelationalError Identity) a

runDatabaseContextEvalMonad :: DatabaseContext -> DatabaseContextEvalEnv -> DatabaseContextEvalMonad () -> Either RelationalError DatabaseContextEvalState
runDatabaseContextEvalMonad ctx env m = runIdentity (runExceptT (fst <$> execRWST m env freshEnv))
  where
    freshEnv = mkDatabaseContextEvalState ctx
    

dbcTransId :: DatabaseContextEvalMonad TransactionId
dbcTransId = dce_transId <$> RWS.ask

dbcGraph :: DatabaseContextEvalMonad TransactionGraph
dbcGraph = dce_graph <$> RWS.ask

dbcRelationalExprEnv :: DatabaseContextEvalMonad RelationalExprEnv
dbcRelationalExprEnv = 
  mkRelationalExprEnv <$> getStateContext <*> dbcGraph

getStateContext :: DatabaseContextEvalMonad DatabaseContext
getStateContext = gets dbc_context

putStateContext :: DatabaseContext -> DatabaseContextEvalMonad () 
putStateContext ctx' = do
  s <- get
  put (s {dbc_context = ctx', dbc_dirty = True}) 

-- | The context is optionally passed down along in cases where the current context is uncommitted.
data GraphRefRelationalExprEnv =
  GraphRefRelationalExprEnv {
  gre_context :: Maybe DatabaseContext,
  gre_graph :: TransactionGraph,
  gre_extra :: Maybe (Either RelationTuple Attributes)
  }
  
type GraphRefRelationalExprM a = ReaderT GraphRefRelationalExprEnv (ExceptT RelationalError Identity) a

gfTransForId :: TransactionId -> GraphRefRelationalExprM Transaction
gfTransForId tid = do
  graph <- gfGraph
  lift $ except $ transactionForId tid graph

gfDatabaseContextForMarker :: GraphRefTransactionMarker -> GraphRefRelationalExprM DatabaseContext
gfDatabaseContextForMarker (TransactionMarker transId) = concreteDatabaseContext <$> gfTransForId transId
gfDatabaseContextForMarker UncommittedContextMarker = do
  mctx <- gre_context <$> askEnv
  case mctx of
    Nothing -> throwError NoUncommittedContextInEvalError
    Just ctx -> pure ctx

runGraphRefRelationalExprM :: GraphRefRelationalExprEnv -> GraphRefRelationalExprM a -> Either RelationalError a
runGraphRefRelationalExprM env m = runIdentity (runExceptT (runReaderT m env))

freshGraphRefRelationalExprEnv :: Maybe DatabaseContext -> TransactionGraph -> GraphRefRelationalExprEnv
freshGraphRefRelationalExprEnv mctx graph = GraphRefRelationalExprEnv {
  gre_context = mctx,
  gre_graph = graph,
  gre_extra = Nothing
  }

gfGraph :: GraphRefRelationalExprM TransactionGraph
gfGraph = asks gre_graph

envContext :: RelationalExprEnv -> DatabaseContext
envContext = re_context

setEnvContext :: RelationalExprEnv -> DatabaseContext -> RelationalExprEnv
setEnvContext e ctx = e { re_context = ctx }

--helper function to process relation variable creation/assignment
setRelVar :: RelVarName -> GraphRefRelationalExpr -> DatabaseContextEvalMonad ()
setRelVar relVarName relExpr = do
  currentContext <- getStateContext
  --prevent recursive relvar definition by resolving references to relvars in previous states
  relExpr' <- resolve relExpr
  let newRelVars = M.insert relVarName relExpr' $ relationVariables currentContext
      potentialContext = currentContext { relationVariables = newRelVars }
  --optimization: if the relexpr is unchanged, skip the update      
  if M.lookup relVarName (relationVariables currentContext) == Just relExpr then
    pure ()
    else do
    --determine when to check constraints
    graph <- dbcGraph
    tid <- dbcTransId
    case checkConstraints potentialContext tid graph of
      Left err -> dbErr err
      Right _ -> putStateContext potentialContext

--fast-path insertion- we already know that the previous relvar validated correctly, so we can validate just the relation that is being inserted for attribute matches- without this, even a single tuple relation inserted causes the entire relation to be re-validated unnecessarily
--insertIntoRelVar :: RelVarName -> GraphRefRelationalExpr -> DatabaseContextEvalMonad ()

-- it is not an error to delete a relvar which does not exist, just like it is not an error to insert a pre-existing tuple into a relation
deleteRelVar :: RelVarName -> DatabaseContextEvalMonad ()
deleteRelVar relVarName = do
  currContext <- getStateContext
  let relVars = relationVariables currContext
  if M.notMember relVarName relVars then
    pure ()
    else do
    let newRelVars = M.delete relVarName relVars
        newContext = currContext { relationVariables = newRelVars }
    graph <- dbcGraph
    tid <- dbcTransId
    case checkConstraints newContext tid graph of
      Left err -> dbErr err
      Right _ ->
        putStateContext newContext

evalGraphRefDatabaseContextExpr :: GraphRefDatabaseContextExpr -> DatabaseContextEvalMonad ()
evalGraphRefDatabaseContextExpr NoOperation = pure ()
  
evalGraphRefDatabaseContextExpr (Define relVarName attrExprs) = do
  context <- getStateContext
  relvars <- fmap relationVariables getStateContext
  tConss <- fmap typeConstructorMapping getStateContext
  graph <- dbcGraph
  let eAttrs = runGraphRefRelationalExprM gfEnv (mapM evalGraphRefAttrExpr attrExprs)
      gfEnv = freshGraphRefRelationalExprEnv (Just context) graph
  case eAttrs of
    Left err -> dbErr err
    Right attrsList -> do
      lift $ except $ validateAttributes tConss (A.attributesFromList attrsList)
      case M.member relVarName relvars of
          True -> dbErr (RelVarAlreadyDefinedError relVarName)
          False -> setRelVar relVarName (ExistingRelation emptyRelation)
            where
              attrs = A.attributesFromList attrsList
              emptyRelation = Relation attrs emptyTupleSet

evalGraphRefDatabaseContextExpr (Undefine relVarName) = deleteRelVar relVarName

evalGraphRefDatabaseContextExpr (Assign relVarName expr) = do
  graph <- re_graph <$> dbcRelationalExprEnv
  context <- getStateContext
  let existingRelVar = M.lookup relVarName (relationVariables context)
      reEnv = freshGraphRefRelationalExprEnv (Just context) graph

  case existingRelVar of
    Nothing -> do
      case runGraphRefRelationalExprM reEnv (typeForGraphRefRelationalExpr expr) of
        Left err -> dbErr err
        Right reltype -> do
          lift $ except $ validateAttributes (typeConstructorMapping context) (attributes reltype)
          setRelVar relVarName expr
    Just existingRel -> do
      let eExpectedType = runGraphRefRelationalExprM reEnv (typeForGraphRefRelationalExpr existingRel)
      case eExpectedType of
        Left err -> dbErr err
        Right expectedType -> do
      -- if we are targeting an existing rv, we can morph a MakeRelationFromExprs datum to fill in missing type variables'
          let hintedExpr = addTargetTypeHints (attributes expectedType) expr
              eNewExprType = runGraphRefRelationalExprM reEnv (typeForGraphRefRelationalExpr hintedExpr)
          case eNewExprType of
            Left err -> dbErr err
            Right newExprType -> do
              if newExprType == expectedType then do
                lift $ except $ validateAttributes (typeConstructorMapping context) (attributes newExprType)
                setRelVar relVarName hintedExpr 
              else
                dbErr (RelationTypeMismatchError (attributes expectedType) (attributes newExprType))

evalGraphRefDatabaseContextExpr (Insert relVarName relExpr) = do
  gfExpr <- relVarByName relVarName
  let optExpr = applyUnionCollapse (Union
                                    relExpr
                                     gfExpr)
  evalGraphRefDatabaseContextExpr (Assign relVarName optExpr)

evalGraphRefDatabaseContextExpr (Delete relVarName predicate) = do
  gfExpr <- relVarByName relVarName
  let optExpr = applyRestrictionCollapse (Restrict (NotPredicate predicate) gfExpr)
  setRelVar relVarName optExpr
  
--union of restricted+updated portion and the unrestricted+unupdated portion
evalGraphRefDatabaseContextExpr (Update relVarName atomExprMap pred') = do
  rvExpr <- relVarByName relVarName
  graph <- re_graph <$> dbcRelationalExprEnv  
  context <- getStateContext
  let reEnv = freshGraphRefRelationalExprEnv (Just context) graph
  --get the current attributes name in the relvar to ensure that we don't conflict when renaming
      eExprType = runGraphRefRelationalExprM reEnv (typeForGraphRefRelationalExpr rvExpr)
  exprType' <- case eExprType of
    Left err -> throwError err
    Right t -> pure t
  let unrestrictedPortion = Restrict (NotPredicate pred') rvExpr
      tmpAttr = tmpAttrC 1
      tmpAttrC :: Int -> AttributeName -> AttributeName
      tmpAttrC c attr =
        let tmpAttrName = "_tmp_" <> T.pack (show c) <> attr in
          if tmpAttrName `S.member` A.attributeNameSet (attributes exprType') then
            tmpAttrC (c+1) attr
          else 
            tmpAttrName
      updateAttr nam atomExpr = Extend (AttributeExtendTupleExpr (tmpAttr nam) atomExpr)
      projectAndRename attr expr = Rename (S.singleton (tmpAttr attr, attr)) (Project (InvertedAttributeNames (S.singleton attr)) expr)
      restrictedPortion = Restrict pred' rvExpr
      updated = foldr (\(oldname, atomExpr) accum ->
                                 let procAtomExpr = runProcessExprM UncommittedContextMarker (processAtomExpr atomExpr) in
                                  updateAttr oldname procAtomExpr accum
                              ) restrictedPortion (M.toList atomExprMap)
              -- the atomExprMap could reference other attributes, so we must perform multi-pass folds
      updatedPortion = foldr projectAndRename updated (M.keys atomExprMap)
  setRelVar relVarName (Union unrestrictedPortion updatedPortion)

evalGraphRefDatabaseContextExpr (AddInclusionDependency newDepName newDep) = do
  currContext <- getStateContext
  transId <- dbcTransId
  graph <- dbcGraph
  let currDeps = inclusionDependencies currContext
      newDeps = M.insert newDepName newDep currDeps
  if M.member newDepName currDeps then
    dbErr (InclusionDependencyNameInUseError newDepName)
    else do
      let potentialContext = currContext { inclusionDependencies = newDeps }
      -- if the potential context passes all constraints, then save it
      -- potential optimization: validate only the new constraint- all old constraints must already hold
      case checkConstraints potentialContext transId graph of
        Left err -> dbErr err
        Right _ -> 
          putStateContext potentialContext

evalGraphRefDatabaseContextExpr (RemoveInclusionDependency depName) = do
  currContext <- getStateContext
  let currDeps = inclusionDependencies currContext
      newDeps = M.delete depName currDeps
  if M.notMember depName currDeps then
    dbErr (InclusionDependencyNameNotInUseError depName)
    else 
    putStateContext $ currContext {inclusionDependencies = newDeps }
    
-- | Add a notification which will send the resultExpr when triggerExpr changes between commits.
evalGraphRefDatabaseContextExpr (AddNotification notName triggerExpr resultOldExpr resultNewExpr) = do
  currentContext <- getStateContext
  let nots = notifications currentContext
  if M.member notName nots then
    dbErr (NotificationNameInUseError notName)
    else do
      let newNotifications = M.insert notName newNotification nots
          newNotification = Notification { changeExpr = triggerExpr,
                                           reportOldExpr = resultOldExpr, 
                                           reportNewExpr = resultNewExpr}
      putStateContext $ currentContext { notifications = newNotifications }
  
evalGraphRefDatabaseContextExpr (RemoveNotification notName) = do
  currentContext <- getStateContext
  let nots = notifications currentContext
  if M.notMember notName nots then
    dbErr (NotificationNameNotInUseError notName)
    else do
    let newNotifications = M.delete notName nots
    putStateContext $ currentContext { notifications = newNotifications }


-- | Adds type and data constructors to the database context.
-- validate that the type *and* constructor names are unique! not yet implemented!
evalGraphRefDatabaseContextExpr (AddTypeConstructor tConsDef dConsDefList) = do
  currentContext <- getStateContext
  let oldTypes = typeConstructorMapping currentContext
      tConsName = TCD.name tConsDef
  -- validate that the constructor's types exist
  case validateTypeConstructorDef tConsDef dConsDefList oldTypes of
    Left err -> throwError err
    Right () | T.null tConsName || not (isUpper (T.head tConsName)) -> dbErr (InvalidAtomTypeName tConsName)
       | isJust (findTypeConstructor tConsName oldTypes) -> dbErr (AtomTypeNameInUseError tConsName)
       | otherwise -> do
      let newTypes = oldTypes ++ [(tConsDef, dConsDefList)]
      putStateContext $ currentContext { typeConstructorMapping = newTypes }

-- | Removing the atom constructor prevents new atoms of the type from being created. Existing atoms of the type remain. Thus, the atomTypes list in the DatabaseContext need not be all-inclusive.
evalGraphRefDatabaseContextExpr (RemoveTypeConstructor tConsName) = do
  currentContext <- getStateContext
  let oldTypes = typeConstructorMapping currentContext
  if isNothing (findTypeConstructor tConsName oldTypes) then
    dbErr (AtomTypeNameNotInUseError tConsName)
    else do
      let newTypes = filter (\(tCons, _) -> TCD.name tCons /= tConsName) oldTypes
      putStateContext $ currentContext { typeConstructorMapping = newTypes }

evalGraphRefDatabaseContextExpr (MultipleExpr exprs) =
  --the multiple expressions must pass the same context around- not the old unmodified context
  mapM_ evalGraphRefDatabaseContextExpr exprs

evalGraphRefDatabaseContextExpr (RemoveAtomFunction funcName') = do
  currentContext <- getStateContext
  let atomFuncs = atomFunctions currentContext
  case atomFunctionForName funcName' atomFuncs of
    Left err -> dbErr err
    Right realFunc ->
      if isScriptedAtomFunction realFunc then do
        let updatedFuncs = HS.delete realFunc atomFuncs
        putStateContext (currentContext {atomFunctions = updatedFuncs })
      else
        dbErr (PrecompiledFunctionRemoveError funcName')
      
evalGraphRefDatabaseContextExpr (RemoveDatabaseContextFunction funcName') = do      
  context <- getStateContext
  let dbcFuncs = dbcFunctions context
  case databaseContextFunctionForName funcName' dbcFuncs of
    Left err -> dbErr err
    Right realFunc ->
      if isScriptedDatabaseContextFunction realFunc then do
        let updatedFuncs = HS.delete realFunc dbcFuncs
        putStateContext (context { dbcFunctions = updatedFuncs })
      else
        dbErr (PrecompiledFunctionRemoveError funcName')
      
evalGraphRefDatabaseContextExpr (ExecuteDatabaseContextFunction funcName' atomArgExprs) = do
  context <- getStateContext
  graph <- dbcGraph
  --resolve atom arguments
  let eAtomTypes = mapM (runGraphRefRelationalExprM gfEnv . typeForGraphRefAtomExpr emptyAttributes) atomArgExprs
      eFunc = databaseContextFunctionForName funcName' (dbcFunctions context)
      gfEnv = freshGraphRefRelationalExprEnv (Just context) graph
  case eFunc of
      Left err -> dbErr err
      Right func -> do
        let expectedArgCount = length (funcType func)
            actualArgCount = length atomArgExprs
        if expectedArgCount /= actualArgCount then
          dbErr (FunctionArgumentCountMismatchError expectedArgCount actualArgCount)
          else 
          --check that the atom types are valid
          case eAtomTypes of
            Left err -> dbErr err
            Right atomTypes -> do
              let mValidTypes = zipWith (\ expType actType
                                           -> case atomTypeVerify expType actType of
                                                Left err -> Just err
                                                Right _ -> Nothing)
                                (funcType func) atomTypes
                  typeErrors = catMaybes mValidTypes
                  eAtomArgs = map (runGraphRefRelationalExprM gfEnv . evalGraphRefAtomExpr emptyTuple) atomArgExprs
              if length (lefts eAtomArgs) > 1 then
                dbErr (someErrors (lefts eAtomArgs))
                else if not (null typeErrors) then
                     dbErr (someErrors typeErrors)
                   else
                     case evalDatabaseContextFunction func (rights eAtomArgs) context of
                       Left err -> dbErr err
                       Right newContext -> putStateContext newContext

evalGraphRefDatabaseContextExpr (AddRegisteredQuery regName regExpr) = do
  context <- getStateContext
  tgraph <- dbcGraph
  tid <- dbcTransId
  case M.lookup regName (registeredQueries context) of
    Just _ -> dbErr (RegisteredQueryNameInUseError regName)
    Nothing -> do
      let context' = context { registeredQueries = M.insert regName regExpr (registeredQueries context) }
      case checkConstraints context' tid tgraph of
        Left err -> dbErr err
        Right _ -> putStateContext context'
evalGraphRefDatabaseContextExpr (RemoveRegisteredQuery regName) = do
  context <- getStateContext  
  case M.lookup regName (registeredQueries context) of
    Nothing -> dbErr (RegisteredQueryNameNotInUseError regName)
    Just _ -> putStateContext (context { registeredQueries = M.delete regName (registeredQueries context) })
  

data DatabaseContextIOEvalEnv = DatabaseContextIOEvalEnv
  { dbcio_transId :: TransactionId,
    dbcio_graph :: TransactionGraph,
    dbcio_mScriptSession :: Maybe ScriptSession,
    dbcio_mModulesDirectory :: Maybe FilePath -- ^ when running in persistent mode, this must be a Just value to a directory containing .o/.so/.dynlib files which the user has placed there for access to compiled functions
  }

type DatabaseContextIOEvalMonad a = RWST DatabaseContextIOEvalEnv () DatabaseContextEvalState IO a

runDatabaseContextIOEvalMonad :: DatabaseContextIOEvalEnv -> DatabaseContext -> DatabaseContextIOEvalMonad (Either RelationalError ()) -> IO (Either RelationalError DatabaseContextEvalState)
runDatabaseContextIOEvalMonad env ctx m = do
  res <- runRWST m env freshState
  case res of
    (Left err,_,_) -> pure (Left err)
    (Right (),s,_) -> pure (Right s)
  where
    freshState = mkDatabaseContextEvalState ctx

requireScriptSession :: DatabaseContextIOEvalMonad (Either RelationalError ScriptSession)
requireScriptSession = do
  env <- RWS.ask
  case dbcio_mScriptSession env of
    Nothing -> pure $ Left $ ScriptError ScriptCompilationDisabledError
    Just ss -> pure (Right ss)

putDBCIOContext :: DatabaseContext -> DatabaseContextIOEvalMonad (Either RelationalError ())
putDBCIOContext ctx = do
  RWS.modify (\dbstate -> dbstate { dbc_context = ctx})
  pure (Right ())

getDBCIOContext :: DatabaseContextIOEvalMonad DatabaseContext
getDBCIOContext = dbc_context <$> RWS.get

getDBCIORelationalExprEnv :: DatabaseContextIOEvalMonad RelationalExprEnv
getDBCIORelationalExprEnv = do
  context <- getDBCIOContext
  mkRelationalExprEnv context . dbcio_graph <$> RWS.ask

evalGraphRefDatabaseContextIOExpr :: GraphRefDatabaseContextIOExpr -> DatabaseContextIOEvalMonad (Either RelationalError ())
#if !defined(PM36_HASKELL_SCRIPTING)
evalGraphRefDatabaseContextIOExpr AddAtomFunction{} = pure (Left (ScriptError ScriptCompilationDisabledError))
evalGraphRefDatabaseContextIOExpr AddDatabaseContextFunction{} = pure (Left (ScriptError ScriptCompilationDisabledError))
evalGraphRefDatabaseContextIOExpr LoadAtomFunctions{} = pure (Left (ScriptError ScriptCompilationDisabledError))
evalGraphRefDatabaseContextIOExpr LoadDatabaseContextFunctions{} = pure (Left (ScriptError ScriptCompilationDisabledError))
#else
evalGraphRefDatabaseContextIOExpr (AddAtomFunction funcName' funcType' script) = do
  eScriptSession <- requireScriptSession
  currentContext <- getDBCIOContext
  case eScriptSession of
    Left err -> pure (Left err)
    Right scriptSession -> do
      res <- liftIO $ try $ runGhc (Just libdir) $ do
        setSession (hscEnv scriptSession)
        let atomFuncs = atomFunctions currentContext
        case extractAtomFunctionType funcType' of
          Left err -> pure (Left err)
          Right adjustedAtomTypeCons -> do
            --compile the function
            eCompiledFunc  <- compileScript (atomFunctionBodyType scriptSession) script
            pure $ case eCompiledFunc of
              Left err -> Left (ScriptError err)
              Right compiledFunc -> do
                funcAtomType <- mapM (\funcTypeArg -> atomTypeForTypeConstructorValidate False funcTypeArg (typeConstructorMapping currentContext) M.empty) adjustedAtomTypeCons
                let updatedFuncs = HS.insert newAtomFunc atomFuncs
                    newContext = currentContext { atomFunctions = updatedFuncs }
                    newAtomFunc = Function { funcName = funcName',
                                             funcType = funcAtomType,
                                             funcBody = FunctionScriptBody script compiledFunc }
               -- check if the name is already in use
                if HS.member funcName' (HS.map funcName atomFuncs) then
                  Left (FunctionNameInUseError funcName')
                  else 
                  Right newContext
      case res of
        Left (exc :: SomeException) -> pure $ Left (ScriptError (OtherScriptCompilationError (show exc)))
        Right eContext -> case eContext of
          Left err -> pure (Left err)
          Right context' -> putDBCIOContext context'
evalGraphRefDatabaseContextIOExpr (AddDatabaseContextFunction funcName' funcType' script) = do
  eScriptSession <- requireScriptSession
  currentContext <- getDBCIOContext
  case eScriptSession of
    Left err -> pure (Left err)
    Right scriptSession -> do
      --validate that the function signature is of the form x -> y -> ... -> DatabaseContext -> DatabaseContext
      let last2Args = reverse (take 2 (reverse funcType'))
          atomArgs = take (length funcType' - 2) funcType'
          dbContextTypeCons = ADTypeConstructor "Either" [ADTypeConstructor "DatabaseContextFunctionError" [], ADTypeConstructor "DatabaseContext" []]
          expectedType = "DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext"
          actualType = show funcType'
      if last2Args /= [ADTypeConstructor "DatabaseContext" [], dbContextTypeCons] then 
        pure (Left (ScriptError (TypeCheckCompilationError expectedType actualType)))
        else do
        res <- liftIO $ try $ runGhc (Just libdir) $ do
          setSession (hscEnv scriptSession)
          eCompiledFunc  <- compileScript (dbcFunctionBodyType scriptSession) script
          pure $ case eCompiledFunc of        
            Left err -> Left (ScriptError err)
            Right compiledFunc -> do
              --if we are here, we have validated that the written function type is X -> DatabaseContext -> DatabaseContext, so we need to munge the first elements into an array
              funcAtomType <- mapM (\funcTypeArg -> atomTypeForTypeConstructor funcTypeArg (typeConstructorMapping currentContext) M.empty) atomArgs
              let updatedDBCFuncs = HS.insert newDBCFunc (dbcFunctions currentContext)
                  newContext = currentContext { dbcFunctions = updatedDBCFuncs }
                  dbcFuncs = dbcFunctions currentContext
                  newDBCFunc = Function {
                    funcName = funcName',
                    funcType = funcAtomType,
                    funcBody = FunctionScriptBody script compiledFunc
                    }
                -- check if the name is already in use
              if HS.member funcName' (HS.map funcName dbcFuncs) then
                Left (FunctionNameInUseError funcName')
                else 
                Right newContext
        case res of
          Left (exc :: SomeException) -> pure $ Left (ScriptError (OtherScriptCompilationError (show exc)))
          Right eContext -> case eContext of
            Left err -> pure (Left err)
            Right context' -> putDBCIOContext context'
evalGraphRefDatabaseContextIOExpr (LoadAtomFunctions modName entrypointName modPath) = do

  -- when running an in-memory database, we are willing to load object files from any path- when running in persistent mode, we load modules only from the modules directory so that we can be reasonbly sure that these same modules will exist when the database is restarted from the same directory
  mModDir <- dbcio_mModulesDirectory <$> ask
  currentContext <- getDBCIOContext
  let sModName = T.unpack modName
      sEntrypointName = T.unpack entrypointName
  eLoadFunc <- liftIO $ loadFunctions sModName sEntrypointName mModDir modPath
  case eLoadFunc of
    Left LoadSymbolError -> pure (Left LoadFunctionError)
    Left SecurityLoadSymbolError -> pure (Left SecurityLoadFunctionError)
    Right atomFunctionListFunc -> do
      let newContext = currentContext { atomFunctions = mergedFuncs }
          processedAtomFunctions = processObjectLoadedFunctions sModName sEntrypointName modPath atomFunctionListFunc
          mergedFuncs = HS.union (atomFunctions currentContext) (HS.fromList processedAtomFunctions)
      putDBCIOContext newContext
evalGraphRefDatabaseContextIOExpr (LoadDatabaseContextFunctions modName entrypointName modPath) = do
  currentContext <- getDBCIOContext
  let sModName = T.unpack modName
      sEntrypointName = T.unpack entrypointName
  mModDir <- dbcio_mModulesDirectory <$> ask      
  eLoadFunc <- liftIO $ loadFunctions sModName sEntrypointName mModDir modPath
  case eLoadFunc of
    Left LoadSymbolError -> pure (Left LoadFunctionError)
    Left SecurityLoadSymbolError -> pure (Left SecurityLoadFunctionError)
    Right dbcListFunc -> let newContext = currentContext { dbcFunctions = mergedFuncs }
                             mergedFuncs = HS.union (dbcFunctions currentContext) (HS.fromList processedDBCFuncs)
                             processedDBCFuncs = processObjectLoadedFunctions sModName sEntrypointName modPath dbcListFunc
                                  in putDBCIOContext newContext
#endif
evalGraphRefDatabaseContextIOExpr (CreateArbitraryRelation relVarName attrExprs range) = do
  --Define
  currentContext <- getDBCIOContext
  env <- RWS.ask
  --create graph ref expr
  let gfExpr = Define relVarName attrExprs
      evalEnv = mkDatabaseContextEvalEnv (dbcio_transId env) (dbcio_graph env)
      graph = dbcio_graph env
  case runDatabaseContextEvalMonad currentContext evalEnv (evalGraphRefDatabaseContextExpr gfExpr) of
    Left err -> pure (Left err)
    Right dbstate -> do
         --Assign
           let existingRelVar = M.lookup relVarName relVarTable
               relVarTable = relationVariables (dbc_context dbstate)
           case existingRelVar of
                Nothing -> pure $ Left (RelVarNotDefinedError relVarName)
                Just existingRel -> do
                  let gfEnv = freshGraphRefRelationalExprEnv (Just currentContext) graph
                  case runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr existingRel) of
                    Left err -> pure (Left err)
                    Right relType -> do
                      let expectedAttributes = attributes relType
                          tcMap = typeConstructorMapping (dbc_context dbstate)
                      eitherRel <- liftIO $ generate $ runReaderT (arbitraryRelation expectedAttributes range) tcMap
                      case eitherRel of
                        Left err -> pure $ Left err
                        Right rel ->
                          case runDatabaseContextEvalMonad currentContext evalEnv (setRelVar relVarName (ExistingRelation rel)) of
                            Left err -> pure (Left err)
                            Right dbstate' -> putDBCIOContext (dbc_context dbstate')

--run verification on all constraints
checkConstraints :: DatabaseContext -> TransactionId -> TransactionGraph -> Either RelationalError ()
checkConstraints context transId graph@(TransactionGraph graphHeads transSet) = do
  mapM_ (uncurry checkIncDep) (M.toList deps)
  mapM_ checkRegisteredQuery (M.toList (registeredQueries context))
  where
    potentialGraph = TransactionGraph graphHeads (S.insert tempTrans transSet)
    tempStamp = UTCTime { utctDay = fromGregorian 2000 1 1,
                          utctDayTime = secondsToDiffTime 0 }
    tempSchemas = Schemas context M.empty
    tempTrans = Transaction U.nil  tempTransInfo tempSchemas
    tempTransInfo = TransactionInfo { parents = transId NE.:| [],
                                      stamp = tempStamp,
                                      merkleHash = mempty
                                      }
    
    deps = inclusionDependencies context
    process = runProcessExprM UncommittedContextMarker
    gfEnv = freshGraphRefRelationalExprEnv (Just context) graph
      -- no optimization available here, really? perhaps the optimizer should be passed down to here or the eval function should be passed through the environment
    checkIncDep depName (InclusionDependency subsetExpr supersetExpr) = do
      let gfSubsetExpr = process (processRelationalExpr subsetExpr)
          gfSupersetExpr = process (processRelationalExpr supersetExpr)
      --if both expressions are of a single-attribute (such as with a simple foreign key), the names of the attributes are irrelevant (they need not match) because the expression is unambiguous, but special-casing this to rename the attribute automatically would not be orthogonal behavior and probably cause confusion. Instead, special case the error to make it clear.
          runGfRel e = case runGraphRefRelationalExprM gfEnv e of
                         Left err -> Left (wrapIncDepErr (Just err))
                         Right v -> Right v
          wrapIncDepErr = InclusionDependencyCheckError depName
      typeSub <- runGfRel (typeForGraphRefRelationalExpr gfSubsetExpr)
      typeSuper <- runGfRel (typeForGraphRefRelationalExpr gfSupersetExpr)
      when (typeSub /= typeSuper) (Left (wrapIncDepErr (Just (RelationTypeMismatchError (attributes typeSub) (attributes typeSuper)))))
      let checkExpr = Equals gfSupersetExpr (Union gfSubsetExpr gfSupersetExpr)
          gfEvald = runGraphRefRelationalExprM gfEnv' (evalGraphRefRelationalExpr checkExpr)
          gfEnv' = freshGraphRefRelationalExprEnv (Just context) potentialGraph
      case gfEvald of
        Left err -> Left (wrapIncDepErr (Just err))
        Right resultRel -> if resultRel == relationTrue then
                                   pure ()
                                else 
                                  Left (wrapIncDepErr Nothing)
    --registered queries just need to typecheck- think of them as a constraints on the schema/DDL
    checkRegisteredQuery (qName, relExpr) = do
      let gfExpr = process (processRelationalExpr relExpr)
      case runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr) of
        Left err -> Left (RegisteredQueryValidationError qName err)
        Right _ -> pure ()

-- the type of a relational expression is equal to the relation attribute set returned from executing the relational expression; therefore, the type can be cheaply derived by evaluating a relational expression and ignoring and tuple processing
-- furthermore, the type of a relational expression is the resultant header of the evaluated empty-tupled relation

typeForRelationalExpr :: RelationalExpr -> RelationalExprM Relation
typeForRelationalExpr expr = do
  --replace the relationVariables context element with a cloned set of relation devoid of tuples
  --evalRelationalExpr could still return an existing relation with tuples, so strip them
  graph <- reGraph
  context <- reContext
  let gfExpr = runProcessExprM UncommittedContextMarker (processRelationalExpr expr)
      gfEnv = freshGraphRefRelationalExprEnv (Just context) graph
      runGf = runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr)
  lift $ except runGf

liftE :: (Monad m) => m (Either a b) -> ExceptT a m b
liftE v = do
  y <- lift v
  case y of
    Left err -> throwError err
    Right val -> pure val

{- used for restrictions- take the restrictionpredicate and return the corresponding filter function -}
predicateRestrictionFilter :: Attributes -> GraphRefRestrictionPredicateExpr -> GraphRefRelationalExprM RestrictionFilter
predicateRestrictionFilter attrs (AndPredicate expr1 expr2) = do
  expr1v <- predicateRestrictionFilter attrs expr1
  expr2v <- predicateRestrictionFilter attrs expr2
  pure (\x -> do
           ev1 <- expr1v x 
           ev2 <- expr2v x
           pure (ev1 && ev2))

predicateRestrictionFilter attrs (OrPredicate expr1 expr2) = do
    expr1v <- predicateRestrictionFilter attrs expr1
    expr2v <- predicateRestrictionFilter attrs expr2
    pure (\x -> do
                ev1 <- expr1v x 
                ev2 <- expr2v x
                pure (ev1 || ev2))

predicateRestrictionFilter _ TruePredicate = pure (\_ -> pure True)

predicateRestrictionFilter attrs (NotPredicate expr) = do
  exprv <- predicateRestrictionFilter attrs expr
  pure (fmap not . exprv)

--optimization opportunity: if the subexpression does not reference attributes in the top-level expression, then it need only be evaluated once, statically, outside the tuple filter- see historical implementation here
predicateRestrictionFilter _ (RelationalExprPredicate relExpr) = do
  renv <- askEnv
  let eval :: RelationTuple -> Either RelationalError Relation
      eval tup = 
        let gfEnv = mergeTuplesIntoGraphRefRelationalExprEnv tup renv in
        runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr relExpr)
  pure (\tup -> case eval tup of
    Left err -> Left err
    Right rel -> if arity rel /= 0 then
                   Left (PredicateExpressionError "Relational restriction filter must evaluate to 'true' or 'false'")
                   else
                   pure (rel == relationTrue))

predicateRestrictionFilter attrs (AttributeEqualityPredicate attrName atomExpr) = do
  env <- askEnv
  let attrs' = A.union attrs (envAttributes env)
      ctxtup' = envTuple env
  atomExprType <- typeForGraphRefAtomExpr attrs' atomExpr
  attr <- lift $ except $ case A.attributeForName attrName attrs' of
      Right attr -> Right attr
      Left (NoSuchAttributeNamesError _) -> case A.attributeForName attrName (tupleAttributes ctxtup') of 
                                              Right ctxattr -> Right ctxattr
                                              Left err2@(NoSuchAttributeNamesError _) -> Left err2
                                              Left err -> Left err
      Left err -> Left err
  if atomExprType /= A.atomType attr then do
      throwError (TupleAttributeTypeMismatchError (A.attributesFromList [attr]))
    else
      pure $ \tupleIn -> let evalAndCmp atomIn = case atomEvald of
                               Right atomCmp -> atomCmp == atomIn
                               Left _ -> False
                             atomEvald = runGraphRefRelationalExprM env (evalGraphRefAtomExpr tupleIn atomExpr)
                         in
                          pure $ case atomForAttributeName attrName tupleIn of
                            Left (NoSuchAttributeNamesError _) -> case atomForAttributeName attrName ctxtup' of
                              Left _ -> False
                              Right ctxatom -> evalAndCmp ctxatom
                            Left _ -> False
                            Right atomIn -> evalAndCmp atomIn
-- in the future, it would be useful to do typechecking on the attribute and atom expr filters in advance
predicateRestrictionFilter attrs (AtomExprPredicate atomExpr) = do
  --merge attrs into the state attributes
  renv <- askEnv
  aType <- typeForGraphRefAtomExpr attrs atomExpr
  if aType /= BoolAtomType then
      throwError (AtomTypeMismatchError aType BoolAtomType)
    else
      pure (\tupleIn ->
             case runGraphRefRelationalExprM renv (evalGraphRefAtomExpr tupleIn atomExpr) of
               Left err -> Left err
               Right boolAtomValue -> pure (boolAtomValue == BoolAtom True))

tupleExprCheckNewAttrName :: AttributeName -> Relation -> Either RelationalError Relation
tupleExprCheckNewAttrName attrName rel = if isRight (attributeForName attrName rel) then
                                           Left (AttributeNameInUseError attrName)
                                         else
                                           Right rel

extendGraphRefTupleExpressionProcessor :: Relation -> GraphRefExtendTupleExpr -> GraphRefRelationalExprM (Attributes, RelationTuple -> Either RelationalError RelationTuple)
extendGraphRefTupleExpressionProcessor relIn (AttributeExtendTupleExpr newAttrName atomExpr) = 
--  renv <- askEnv
  -- check that the attribute name is not in use
  case tupleExprCheckNewAttrName newAttrName relIn of
    Left err -> throwError err
    Right _ -> do
      atomExprType <- typeForGraphRefAtomExpr (attributes relIn) atomExpr
      atomExprType' <- verifyGraphRefAtomExprTypes relIn atomExpr atomExprType
      let newAttrs = A.attributesFromList [Attribute newAttrName atomExprType']
          newAndOldAttrs = A.addAttributes (attributes relIn) newAttrs
      env <- ask
      pure (newAndOldAttrs, \tup -> do
               let gfEnv = mergeTuplesIntoGraphRefRelationalExprEnv tup env
               atom <- runGraphRefRelationalExprM gfEnv (evalGraphRefAtomExpr tup atomExpr)
               Right (tupleAtomExtend newAttrName atom tup)
               )

  

evalGraphRefAtomExpr :: RelationTuple -> GraphRefAtomExpr -> GraphRefRelationalExprM Atom
evalGraphRefAtomExpr tupIn (AttributeAtomExpr attrName) =
  case atomForAttributeName attrName tupIn of
      Right atom -> pure atom
      Left err@(NoSuchAttributeNamesError _) -> do
        env <- askEnv
        case gre_extra env of
          Nothing -> throwError err
          Just (Left ctxtup) -> lift $ except $ atomForAttributeName attrName ctxtup
          Just (Right _) -> throwError err
      Left err -> throwError err
  
evalGraphRefAtomExpr _ (NakedAtomExpr atom) = pure atom
-- first argumentr is starting value, second argument is relationatom
evalGraphRefAtomExpr tupIn (AggregateFunctionAtomExpr funcName' (rvAttrName, aggAttributeName) arguments tid) = do
  argTypes <- mapM (typeForGraphRefAtomExpr (tupleAttributes tupIn)) arguments
  context <- gfDatabaseContextForMarker tid
  let aggFuncs = aggregateFunctions context
      --atomFuncs = atomFunctions context
  traceShowM ("evalGraphRef agg"::String, rvAttrName, aggAttributeName)      
  aggFunc <- lift $ except (Agg.functionForName funcName' aggFuncs)
  let zippedArgs = zip (safeInit (aggFuncFoldType aggFunc)) argTypes
      safeInit [] = [] -- different behavior from normal init
      safeInit xs = init xs
  mapM_ (\(expType, actType) -> 
                lift $ except (atomTypeVerify expType actType)) zippedArgs
  evaldArgs <- mapM (evalGraphRefAtomExpr tupIn) arguments
  let startingVal = head evaldArgs
  case atomForAttributeName rvAttrName tupIn of
    Left err -> throwError err
    Right (RelationAtom rel) -> do
      traceShowM ("evalGraphRefAtomExpr"::String, aggAttributeName)
      case evalAggregateFunction (aggFuncFoldFunc aggFunc) aggAttributeName startingVal [] rel of
        Left err -> throwError (AtomFunctionUserError err)
        Right v -> do
          traceShowM ("evalGraphRefAtomExpr2"::String, v)          
          pure v
    Right _ -> throwError (AttributeIsNotRelationValuedError rvAttrName)
evalGraphRefAtomExpr tupIn (FunctionAtomExpr funcName' arguments tid) = do
  argTypes <- mapM (typeForGraphRefAtomExpr (tupleAttributes tupIn)) arguments
  context <- gfDatabaseContextForMarker tid
  let functions = atomFunctions context
  func <- lift $ except (atomFunctionForName funcName' functions)
  let expectedArgCount = length (funcType func) - 1
      actualArgCount = length argTypes
      safeInit [] = [] -- different behavior from normal init
      safeInit xs = init xs
  if expectedArgCount /= actualArgCount then
      throwError (FunctionArgumentCountMismatchError expectedArgCount actualArgCount)
    else do
      let zippedArgs = zip (safeInit (funcType func)) argTypes
      mapM_ (\(expType, actType) -> 
                lift $ except (atomTypeVerify expType actType)) zippedArgs
      evaldArgs <- mapM (evalGraphRefAtomExpr tupIn) arguments
      case evalAtomFunction func evaldArgs of
        Left err -> throwError (AtomFunctionUserError err)
        Right result -> do
          --validate that the result matches the expected type
          _ <- lift $ except (atomTypeVerify (last (funcType func)) (atomTypeForAtom result))
          pure result
evalGraphRefAtomExpr tupIn (RelationAtomExpr relExpr) = do
  --merge existing state tuple context into new state tuple context to support an arbitrary number of levels, but new attributes trounce old attributes
  env <- ask
  let gfEnv = mergeTuplesIntoGraphRefRelationalExprEnv tupIn env
  relAtom <- lift $ except $ runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr relExpr)
  pure (RelationAtom relAtom)
evalGraphRefAtomExpr tupIn (IfThenAtomExpr ifExpr thenExpr elseExpr) = do
  conditional <- evalGraphRefAtomExpr tupIn ifExpr
  case conditional of
    BoolAtom True -> evalGraphRefAtomExpr tupIn thenExpr
    BoolAtom False -> evalGraphRefAtomExpr tupIn elseExpr
    otherAtom -> throwError (IfThenExprExpectedBooleanError (atomTypeForAtom otherAtom))
evalGraphRefAtomExpr _ (ConstructedAtomExpr tOrF [] _)
  | tOrF == "True" = pure (BoolAtom True)
  | tOrF == "False" = pure (BoolAtom False)
evalGraphRefAtomExpr tupIn cons@(ConstructedAtomExpr dConsName dConsArgs _) = do --why is the tid unused here? suspicious
  let mergeEnv = mergeTuplesIntoGraphRefRelationalExprEnv tupIn
  aType <- local mergeEnv (typeForGraphRefAtomExpr (tupleAttributes tupIn) cons)
  argAtoms <- local mergeEnv $
    mapM (evalGraphRefAtomExpr tupIn) dConsArgs
  pure (ConstructedAtom dConsName aType argAtoms)

typeForGraphRefAtomExpr :: Attributes -> GraphRefAtomExpr -> GraphRefRelationalExprM AtomType
typeForGraphRefAtomExpr attrs (AttributeAtomExpr attrName) = do
  renv <- askEnv
  case A.atomTypeForAttributeName attrName attrs of
    Right aType -> pure aType
    Left err@(NoSuchAttributeNamesError _) ->
      let envTup = envTuple renv
          envAttrs = envAttributes renv in
      case A.attributeForName attrName envAttrs of
        Right attr -> pure (A.atomType attr)
        Left _ -> case atomForAttributeName attrName envTup of
          Right atom -> pure (atomTypeForAtom atom)
          Left _ -> --throwError (traceStack (show ("typeForGRAtomExpr", attrs, envTup)) err)
            throwError err
    Left err -> throwError err

typeForGraphRefAtomExpr _ (NakedAtomExpr atom) = pure (atomTypeForAtom atom)
typeForGraphRefAtomExpr attrs (AggregateFunctionAtomExpr funcName' aggInfo atomArgs transId) = do
  context <- gfDatabaseContextForMarker transId
  let aggFuncs = aggregateFunctions context
  aggFunc <- lift $ except (Agg.functionForName funcName' aggFuncs)  
  let funcRetType = last (aggFuncFoldType aggFunc)
  pure funcRetType
typeForGraphRefAtomExpr attrs (FunctionAtomExpr funcName' atomArgs transId) = do
  funcs <- atomFunctions <$> gfDatabaseContextForMarker transId
  case atomFunctionForName funcName' funcs of
    Left err -> throwError err
    Right func -> do
      let funcRetType = last (funcType func)
          funcArgTypes = init (funcType func)
          funArgCount = length funcArgTypes
          inArgCount = length atomArgs
      when (funArgCount /= inArgCount) (throwError (FunctionArgumentCountMismatchError funArgCount inArgCount))
      argTypes <- mapM (typeForGraphRefAtomExpr attrs) atomArgs
      mapM_ (\(fArg,arg,argCount) -> do
                let handler :: RelationalError -> GraphRefRelationalExprM AtomType
                    handler (AtomTypeMismatchError expSubType actSubType) = throwError (AtomFunctionTypeError funcName' argCount expSubType actSubType)
                    handler err = throwError err
                lift (except $ atomTypeVerify fArg arg) `catchError` handler
            ) (zip3 funcArgTypes argTypes [1..])
      let eTvMap = resolveTypeVariables funcArgTypes argTypes
      case eTvMap of
            Left err -> throwError err
            Right tvMap ->
              lift $ except $ resolveFunctionReturnValue funcName' tvMap funcRetType
typeForGraphRefAtomExpr attrs (RelationAtomExpr relExpr) = do
  relType <- R.local (mergeAttributesIntoGraphRefRelationalExprEnv attrs) (typeForGraphRefRelationalExpr relExpr)  
  pure (RelationAtomType (attributes relType))
typeForGraphRefAtomExpr attrs (IfThenAtomExpr ifExpr thenExpr elseExpr) = do
  -- ifExpr must be BoolAtomType
  ifType <- typeForGraphRefAtomExpr attrs ifExpr
  when (ifType /= BoolAtomType) $ throwError (IfThenExprExpectedBooleanError ifType)
  -- thenExpr and elseExpr must return the same type
  thenType <- typeForGraphRefAtomExpr attrs thenExpr
  elseType <- typeForGraphRefAtomExpr attrs elseExpr
  when (thenType /= elseType) $ throwError (AtomTypeMismatchError thenType elseType)
  pure thenType
-- grab the type of the data constructor, then validate that the args match the expected types
typeForGraphRefAtomExpr _ (ConstructedAtomExpr tOrF [] _) | tOrF `elem` ["True", "False"] =
                                                            pure BoolAtomType
typeForGraphRefAtomExpr attrs (ConstructedAtomExpr dConsName dConsArgs tid) =
  do
    argsTypes <- mapM (typeForGraphRefAtomExpr attrs) dConsArgs
    tConsMap <- typeConstructorMapping <$> gfDatabaseContextForMarker tid
    lift $ except $ atomTypeForDataConstructor tConsMap dConsName argsTypes

-- | Validate that the type of the AtomExpr matches the expected type.
verifyGraphRefAtomExprTypes :: Relation -> GraphRefAtomExpr -> AtomType -> GraphRefRelationalExprM AtomType
verifyGraphRefAtomExprTypes relIn (AttributeAtomExpr attrName) expectedType = do
  env <- askEnv
  case A.atomTypeForAttributeName attrName (attributes relIn) of
    Right aType -> lift $ except $ atomTypeVerify expectedType aType
    (Left err@(NoSuchAttributeNamesError _)) ->
      let attrs' = envAttributes env in
        if attrs' == emptyAttributes then
          throwError err
        else
          case A.attributeForName attrName attrs' of
            Left err' -> throwError err'
            Right attrType -> lift $ except $ atomTypeVerify expectedType (A.atomType attrType)
    Left err -> throwError err

verifyGraphRefAtomExprTypes _ (NakedAtomExpr atom) expectedType =
  lift $ except $ atomTypeVerify expectedType (atomTypeForAtom atom)
verifyGraphRefAtomExprTypes relIn (AggregateFunctionAtomExpr funcName' aggInfo argExprs tid) expectedType  = do
  pure expectedType
verifyGraphRefAtomExprTypes relIn (FunctionAtomExpr funcName' funcArgExprs tid) expectedType = do
  context <- gfDatabaseContextForMarker tid
  let functions = atomFunctions context
  func <- lift $ except $ atomFunctionForName funcName' functions
  let expectedArgTypes = funcType func
      funcArgVerifier (atomExpr, expectedType2, argCount) = do
        let handler :: RelationalError -> GraphRefRelationalExprM AtomType
            handler (AtomTypeMismatchError expSubType actSubType) = throwError (AtomFunctionTypeError funcName' argCount expSubType actSubType)
            handler err = throwError err
        verifyGraphRefAtomExprTypes relIn atomExpr expectedType2 `catchError` handler   
  funcArgTypes <- mapM funcArgVerifier $ zip3 funcArgExprs expectedArgTypes [1..]
  if length funcArgTypes /= length expectedArgTypes - 1 then
      throwError (AtomTypeCountError funcArgTypes expectedArgTypes)
      else 
      lift $ except $ atomTypeVerify expectedType (last expectedArgTypes)
verifyGraphRefAtomExprTypes relIn (RelationAtomExpr relationExpr) expectedType =
  do
    let mergedAttrsEnv = mergeAttributesIntoGraphRefRelationalExprEnv (attributes relIn)
    relType <- R.local mergedAttrsEnv (typeForGraphRefRelationalExpr relationExpr)
    lift $ except $ atomTypeVerify expectedType (RelationAtomType (attributes relType))
verifyGraphRefAtomExprTypes relIn (IfThenAtomExpr _ifExpr thenExpr elseExpr) expectedType = do
  thenType <- typeForGraphRefAtomExpr (attributes relIn) thenExpr
  elseType <- typeForGraphRefAtomExpr (attributes relIn) elseExpr
  when (thenType /= elseType) $ throwError (AtomTypeMismatchError thenType elseType)  
  lift $ except $ atomTypeVerify expectedType thenType
verifyGraphRefAtomExprTypes rel cons@ConstructedAtomExpr{} expectedType = do
  cType <- typeForGraphRefAtomExpr (attributes rel) cons
  lift $ except $ atomTypeVerify expectedType cType

-- | Look up the type's name and create a new attribute.
evalGraphRefAttrExpr :: GraphRefAttributeExpr -> GraphRefRelationalExprM Attribute
evalGraphRefAttrExpr (AttributeAndTypeNameExpr attrName tCons transId) = do
  tConsMap <- typeConstructorMapping <$> gfDatabaseContextForMarker transId
  aType <- lift $ except $ atomTypeForTypeConstructorValidate True tCons tConsMap M.empty
  lift $ except $ validateAtomType aType tConsMap
  pure $ Attribute attrName aType
  
evalGraphRefAttrExpr (NakedAttributeExpr attr) = pure attr

-- for tuple type concrete resolution (Nothing ==> Maybe Int) when the attributes hint is Nothing, we need to first process all the tuples, then extract the concrete types on a per-attribute basis, then reprocess the tuples to include the concrete types
evalGraphRefTupleExprs :: Maybe Attributes -> GraphRefTupleExprs -> GraphRefRelationalExprM [RelationTuple]
evalGraphRefTupleExprs _ (TupleExprs _ []) = pure []
evalGraphRefTupleExprs mAttrs (TupleExprs fixedMarker tupleExprL) = do
  tuples <- mapM (evalGraphRefTupleExpr mAttrs) tupleExprL
  finalAttrs <- case mAttrs of
    Just attrs -> pure attrs
    Nothing ->
      case tuples of
        [] -> pure emptyAttributes
        (headTuple:tailTuples) -> do
      --gather up resolved atom types or throw an error if an attribute cannot be made concrete from the inferred types- this could still fail if the type cannot be inferred (e.g. from [Nothing, Nothing])
          let 
              processTupleAttrs (tupAttr, accAttr) =
                --if the attribute is a constructedatomtype, we can recurse into it to potentially resolve type variables                
                if isResolvedAttribute accAttr && tupAttr == accAttr then
                  pure accAttr
                else
                  lift $ except $ resolveAttributes accAttr tupAttr 
          mostResolvedTypes <-
                foldM (\acc tup -> do
                         let zipped = zip (V.toList . attributesVec $ tupleAttributes tup) acc
                             accNames = S.fromList $ map A.attributeName acc
                             tupNames = A.attributeNameSet (tupleAttributes tup)
                             attrNamesDiff = S.union (S.difference accNames tupNames) (S.difference tupNames accNames)
                         unless (null attrNamesDiff) (throwError (AttributeNamesMismatchError attrNamesDiff))
                         nextTupleAttrs <- mapM processTupleAttrs zipped
                         let diff = A.attributesDifference (A.attributesFromList nextTupleAttrs) (A.attributesFromList acc)
                         if diff == A.emptyAttributes then
                           pure nextTupleAttrs
                           else
                           throwError (TupleAttributeTypeMismatchError diff)
                      ) (V.toList . attributesVec $ tupleAttributes headTuple) tailTuples
          pure (A.attributesFromList mostResolvedTypes)
  --strategy: if all the tuple expr transaction markers refer to one location, then we can pass the type constructor mapping from that location, otherwise, we cannot assume that the types are the same
  tConsMap <- case singularTransactions tupleExprL of
                   SingularTransactionRef commonTransId -> 
                     typeConstructorMapping <$> gfDatabaseContextForMarker commonTransId
                   NoTransactionsRef -> 
                     typeConstructorMapping <$> gfDatabaseContextForMarker fixedMarker
  -- if there are multiple transaction markers in the TupleExprs, then we can't assume a single type constructor mapping- this could be improved in the future, but if all the tuples are fully resolved, then we don't need further resolution                     
                   _ -> throwError TupleExprsReferenceMultipleMarkersError
  lift $ except $ validateAttributes tConsMap finalAttrs
  mapM (lift . except . resolveTypesInTuple finalAttrs tConsMap) tuples


--resolveAttributes (Attribute "gonk" (ConstructedAtomType "Either" (fromList [("a",IntegerAtomType),("b",TypeVariableType "b")]))) (Attribute "gonk" (ConstructedAtomType "Either" (fromList [("a",TypeVariableType "a"),("b",TextAtomType)])))
                                                                                                                                                 
evalGraphRefTupleExpr :: Maybe Attributes -> GraphRefTupleExpr -> GraphRefRelationalExprM RelationTuple
evalGraphRefTupleExpr mAttrs (TupleExpr tupMap) = do
  -- it's not possible for AtomExprs in tuple constructors to reference other Attributes' atoms due to the necessary order-of-operations (need a tuple to pass to evalAtomExpr)- it may be possible with some refactoring of type usage or delayed evaluation- needs more thought, but not a priority
  -- I could adjust this logic so that when the attributes are not specified (Nothing), then I can attempt to extract the attributes from the tuple- the type resolution will blow up if an ambiguous data constructor is used (Left 4) and this should allow simple cases to "relation{tuple{a 4}}" to be processed
  let attrs = fromMaybe A.emptyAttributes mAttrs
      resolveOneAtom (attrName, aExpr) =
        do
          --provided when the relation header is available
          let eExpectedAtomType = A.atomTypeForAttributeName attrName attrs
          unresolvedType <- typeForGraphRefAtomExpr attrs aExpr
          resolvedType <- case eExpectedAtomType of
                            Left _ -> pure unresolvedType
                            Right typeHint -> lift $ except $ resolveAtomType typeHint unresolvedType
                          --resolve atom typevars based on resolvedType?
          newAtom <- evalGraphRefAtomExpr emptyTuple aExpr
          pure (attrName, newAtom, resolvedType)
  attrAtoms <- mapM resolveOneAtom (M.toList tupMap)
  let tupAttrs = A.attributesFromList $ map (\(attrName, _, aType) -> Attribute attrName aType) attrAtoms
      atoms = V.fromList $ map (\(_, atom, _) -> atom) attrAtoms
      tup = mkRelationTuple tupAttrs atoms
      finalAttrs = fromMaybe tupAttrs mAttrs
    --verify that the attributes match
  when (A.attributeNameSet finalAttrs /= A.attributeNameSet tupAttrs) $ do
    throwError (TupleAttributeTypeMismatchError tupAttrs)
  --we can't resolve types here- they have to be resolved at the atom level where the graph ref is held
  --tup' <- lift $ except (resolveTypesInTuple finalAttrs tConss (reorderTuple finalAttrs tup))
  let tup' = reorderTuple finalAttrs tup
  --TODO: restore type resolution
--  _ <- lift $ except (validateTuple tup' tConss)
  pure tup'

--temporary implementation until we have a proper planner+executor
evalGraphRefRelationalExpr :: GraphRefRelationalExpr -> GraphRefRelationalExprM Relation
evalGraphRefRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) = do
  mAttrs <- case mAttrExprs of
    Just _ ->
        Just . A.attributesFromList <$> mapM evalGraphRefAttrExpr (fromMaybe [] mAttrExprs)
    Nothing -> pure Nothing
  tuples <- evalGraphRefTupleExprs mAttrs tupleExprs
  let attrs = fromMaybe firstTupleAttrs mAttrs
      firstTupleAttrs = if null tuples then A.emptyAttributes else tupleAttributes (head tuples)
  lift $ except $ mkRelation attrs (RelationTupleSet tuples)
evalGraphRefRelationalExpr (MakeStaticRelation attributeSet tupleSet) = 
  lift $ except $ mkRelation attributeSet tupleSet
evalGraphRefRelationalExpr (ExistingRelation rel) = pure rel
evalGraphRefRelationalExpr (RelationVariable name tid) = do
  ctx <- gfDatabaseContextForMarker tid
  case M.lookup name (relationVariables ctx) of
    Nothing -> throwError (RelVarNotDefinedError name)
    Just rv -> evalGraphRefRelationalExpr rv
evalGraphRefRelationalExpr (RelationValuedAttribute attrName) = do
  env <- askEnv
  case gre_extra env of
    Nothing -> throwError (NoSuchAttributeNamesError (S.singleton attrName))
    Just (Left ctxtup) -> do
      atom <- lift $ except $ atomForAttributeName attrName ctxtup
      case atom of
        RelationAtom rel -> pure rel
        other -> throwError (AtomTypeMismatchError (RelationAtomType mempty) (atomTypeForAtom other))
    Just (Right _) -> throwError (NoSuchAttributeNamesError (S.singleton attrName))
evalGraphRefRelationalExpr (Project attrNames expr) = do
  attrNameSet <- evalGraphRefAttributeNames attrNames expr
  rel <- evalGraphRefRelationalExpr expr
  lift $ except $ project attrNameSet rel
evalGraphRefRelationalExpr (Union exprA exprB) = do
  relA <- evalGraphRefRelationalExpr exprA
  relB <- evalGraphRefRelationalExpr exprB
  lift $ except $ union relA relB
evalGraphRefRelationalExpr (Join exprA exprB) = do  
  relA <- evalGraphRefRelationalExpr exprA
  relB <- evalGraphRefRelationalExpr exprB
  lift $ except $ join relA relB
evalGraphRefRelationalExpr (Rename attrsSet expr) = do
  rel <- evalGraphRefRelationalExpr expr
  lift $ except $ renameMany attrsSet rel
evalGraphRefRelationalExpr (Difference exprA exprB) = do
  relA <- evalGraphRefRelationalExpr exprA
  relB <- evalGraphRefRelationalExpr exprB
  lift $ except $ difference relA relB
evalGraphRefRelationalExpr (Group groupAttrNames newAttrName expr) = do
  groupNames <- evalGraphRefAttributeNames groupAttrNames expr
  rel <- evalGraphRefRelationalExpr expr
  lift $ except $ group groupNames newAttrName rel
evalGraphRefRelationalExpr (Ungroup groupAttrName expr) = do
  rel <- evalGraphRefRelationalExpr expr
  lift $ except $ ungroup groupAttrName rel
evalGraphRefRelationalExpr (Restrict predExpr expr) = do
  rel <- evalGraphRefRelationalExpr expr
  filt <- predicateRestrictionFilter (attributes rel) predExpr
  lift $ except $ restrict filt rel
evalGraphRefRelationalExpr (Equals exprA exprB) = do
  relA <- evalGraphRefRelationalExpr exprA
  relB <- evalGraphRefRelationalExpr exprB
  pure $ if relA == relB then relationTrue else relationFalse
evalGraphRefRelationalExpr (NotEquals exprA exprB) = do
  relA <- evalGraphRefRelationalExpr exprA
  relB <- evalGraphRefRelationalExpr exprB
  pure $ if relA == relB then relationFalse else relationTrue
evalGraphRefRelationalExpr (Extend extendTupleExpr expr) = do
  rel <- evalGraphRefRelationalExpr expr
  (newAttrs, tupProc) <- extendGraphRefTupleExpressionProcessor rel extendTupleExpr
  lift $ except $ relMogrify tupProc newAttrs rel
evalGraphRefRelationalExpr expr@With{} =
  --strategy A: add relation variables to the contexts in the graph
  --strategy B: drop in macros in place (easier programmatically)
  --strategy B implementation
  evalGraphRefRelationalExpr (substituteWithNameMacros [] expr)

dbContextForTransId :: TransactionId -> TransactionGraph -> Either RelationalError DatabaseContext
dbContextForTransId tid graph = do
  trans <- transactionForId tid graph
  pure (concreteDatabaseContext trans)

transactionForId :: TransactionId -> TransactionGraph -> Either RelationalError Transaction
transactionForId tid graph 
  | tid == U.nil =
    Left RootTransactionTraversalError
  | S.null matchingTrans =
    Left $ NoSuchTransactionError tid
  | otherwise =
    Right $ head (S.toList matchingTrans)
  where
    matchingTrans = S.filter (\(Transaction idMatch _ _) -> idMatch == tid) (transactionsForGraph graph)

typeForGraphRefRelationalExpr :: GraphRefRelationalExpr -> GraphRefRelationalExprM Relation
typeForGraphRefRelationalExpr (MakeStaticRelation attrs _) = lift $ except $ mkRelation attrs emptyTupleSet
typeForGraphRefRelationalExpr (ExistingRelation rel) = pure (emptyRelationWithAttrs (attributes rel))
typeForGraphRefRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) = do
  mAttrs <- case mAttrExprs of
              Just attrExprs -> do
                attrs <- mapM evalGraphRefAttributeExpr attrExprs
                pure (Just (attributesFromList attrs))
              Nothing -> pure Nothing
  tuples <- evalGraphRefTupleExprs mAttrs tupleExprs
  let retAttrs = case tuples of
                (tup:_) -> tupleAttributes tup
                [] -> fromMaybe A.emptyAttributes mAttrs
  pure $ emptyRelationWithAttrs retAttrs
  
typeForGraphRefRelationalExpr (RelationVariable rvName tid) = do
  relVars <- relationVariables <$> gfDatabaseContextForMarker tid
  case M.lookup rvName relVars of
    Nothing -> throwError (RelVarNotDefinedError rvName)
    Just rvExpr -> 
      typeForGraphRefRelationalExpr rvExpr
typeForGraphRefRelationalExpr (RelationValuedAttribute attrName) = do
  env <- askEnv
  case gre_extra env of
    Nothing -> throwError (NoSuchAttributeNamesError (S.singleton attrName)) -- or can this be an attribute at the top-level?
    Just (Left ctxtup) -> do
      atom <- lift $ except $ atomForAttributeName attrName ctxtup
      case atom of
        RelationAtom rel -> pure (emptyRelationWithAttrs (attributes rel))
        other -> throwError (AtomTypeMismatchError (RelationAtomType mempty) (atomTypeForAtom other))
    Just (Right attrs) -> do
      case A.atomTypeForAttributeName attrName attrs of
        Left{} -> throwError (NoSuchAttributeNamesError (S.singleton attrName))
        Right typ -> do
          case typ of
            RelationAtomType relAttrs -> pure $ emptyRelationWithAttrs relAttrs
            other -> throwError (AtomTypeMismatchError (RelationAtomType A.emptyAttributes) other)
typeForGraphRefRelationalExpr (Project attrNames expr) = do
  exprType' <- typeForGraphRefRelationalExpr expr
  projectionAttrs <- evalGraphRefAttributeNames attrNames expr
  lift $ except $ project projectionAttrs exprType'
typeForGraphRefRelationalExpr (Union exprA exprB) = do
  exprA' <- typeForGraphRefRelationalExpr exprA
  exprB' <- typeForGraphRefRelationalExpr exprB
  lift $ except $ union exprA' exprB'
typeForGraphRefRelationalExpr (Join exprA exprB) = do
  exprA' <- typeForGraphRefRelationalExpr exprA
  exprB' <- typeForGraphRefRelationalExpr exprB
  lift $ except $ join exprA' exprB'
typeForGraphRefRelationalExpr (Rename attrs expr) = do
  expr' <- typeForGraphRefRelationalExpr expr
  lift $ except $ renameMany attrs expr'
typeForGraphRefRelationalExpr (Difference exprA exprB) = do  
  exprA' <- typeForGraphRefRelationalExpr exprA
  exprB' <- typeForGraphRefRelationalExpr exprB
  lift $ except $ difference exprA' exprB'
typeForGraphRefRelationalExpr (Group groupNames attrName expr) = do
  expr' <- typeForGraphRefRelationalExpr expr
  groupNames' <- evalGraphRefAttributeNames groupNames expr
  lift $ except $ group groupNames' attrName expr'
typeForGraphRefRelationalExpr (Ungroup groupAttrName expr) = do
  expr' <- typeForGraphRefRelationalExpr expr
  lift $ except $ ungroup groupAttrName expr'
typeForGraphRefRelationalExpr (Restrict pred' expr) = do
  expr' <- typeForGraphRefRelationalExpr expr
  filt <- predicateRestrictionFilter (attributes expr') pred'
  lift $ except $ restrict filt expr'
typeForGraphRefRelationalExpr Equals{} = 
  pure relationFalse
typeForGraphRefRelationalExpr NotEquals{} = 
  pure relationFalse
typeForGraphRefRelationalExpr (Extend extendTupleExpr expr) = do
  rel <- typeForGraphRefRelationalExpr expr
  evalGraphRefRelationalExpr (Extend extendTupleExpr (ExistingRelation rel))
typeForGraphRefRelationalExpr expr@(With withs _) = do
  let expr' = substituteWithNameMacros [] expr
      checkMacroName (WithNameExpr macroName tid) = do
        rvs <- relationVariables <$> gfDatabaseContextForMarker tid
        case M.lookup macroName rvs of
          Just _ -> lift $ except $ Left (RelVarAlreadyDefinedError macroName) --this error does not include the transaction marker, but should be good enough to identify the cause
          Nothing -> pure ()
  mapM_ (checkMacroName . fst) withs
  typeForGraphRefRelationalExpr expr'
  
evalGraphRefAttributeNames :: GraphRefAttributeNames -> GraphRefRelationalExpr -> GraphRefRelationalExprM (S.Set AttributeName)
evalGraphRefAttributeNames attrNames expr = do
  exprType' <- typeForGraphRefRelationalExpr expr
  let typeNameSet = S.fromList (V.toList (A.attributeNames (attributes exprType')))
  case attrNames of
    AttributeNames names ->
      case A.projectionAttributesForNames names (attributes exprType') of
        Left err -> throwError err
        Right attrs -> pure (S.fromList (V.toList (A.attributeNames attrs)))
          
    InvertedAttributeNames names -> do
          let nonExistentAttributeNames = A.attributeNamesNotContained names typeNameSet
          if not (S.null nonExistentAttributeNames) then
            throwError $ AttributeNamesMismatchError nonExistentAttributeNames
            else
            pure (A.nonMatchingAttributeNameSet names typeNameSet)
        
    UnionAttributeNames namesA namesB -> do
      nameSetA <- evalGraphRefAttributeNames namesA expr
      nameSetB <- evalGraphRefAttributeNames namesB expr
      pure (S.union nameSetA nameSetB)
        
    IntersectAttributeNames namesA namesB -> do
      nameSetA <- evalGraphRefAttributeNames namesA expr
      nameSetB <- evalGraphRefAttributeNames namesB expr
      pure (S.intersection nameSetA nameSetB)
        
    RelationalExprAttributeNames attrExpr -> do
      attrExprType <- typeForGraphRefRelationalExpr attrExpr
      pure (A.attributeNameSet (attributes attrExprType))

evalGraphRefAttributeExpr :: GraphRefAttributeExpr -> GraphRefRelationalExprM Attribute
evalGraphRefAttributeExpr (AttributeAndTypeNameExpr attrName tCons tid) = do
  tConsMap <- typeConstructorMapping <$> gfDatabaseContextForMarker tid
  case atomTypeForTypeConstructorValidate True tCons tConsMap M.empty of
    Left err -> throwError err
    Right aType -> do
      case validateAtomType aType tConsMap of
        Left err -> throwError err
        Right _ -> pure (Attribute attrName aType)
evalGraphRefAttributeExpr (NakedAttributeExpr attr) = pure attr        

mkEmptyRelVars :: RelationVariables -> RelationVariables
mkEmptyRelVars = M.map mkEmptyRelVar
  where
    mkEmptyRelVar expr@MakeRelationFromExprs{} = expr --do not truncate here because we might lose essential type information in emptying the tuples
    mkEmptyRelVar (MakeStaticRelation attrs _) = MakeStaticRelation attrs emptyTupleSet
    mkEmptyRelVar (ExistingRelation rel) = ExistingRelation (emptyRelationWithAttrs (attributes rel))
    mkEmptyRelVar x@RelationValuedAttribute{} = x
    mkEmptyRelVar rv@RelationVariable{} = Restrict (NotPredicate TruePredicate) rv
    mkEmptyRelVar (Project attrNames expr) = Project attrNames (mkEmptyRelVar expr)
    mkEmptyRelVar (Union exprA exprB) = Union (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (Join exprA exprB) = Join (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (Rename attrs expr) = Rename attrs (mkEmptyRelVar expr)
    mkEmptyRelVar (Difference exprA exprB) = Difference (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (Group attrNames attrName expr) = Group attrNames attrName (mkEmptyRelVar expr)
    mkEmptyRelVar (Ungroup attrName expr) = Ungroup attrName (mkEmptyRelVar expr)
    mkEmptyRelVar (Restrict pred' expr) = Restrict pred' (mkEmptyRelVar expr)
    mkEmptyRelVar (Equals exprA exprB) = Equals (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (NotEquals exprA exprB) = NotEquals (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (Extend extTuple expr) = Extend extTuple (mkEmptyRelVar expr)
    mkEmptyRelVar (With macros expr) = With (map (second mkEmptyRelVar) macros) (mkEmptyRelVar expr)


dbErr :: RelationalError -> DatabaseContextEvalMonad ()
dbErr err = lift (except (Left err))
      
-- | Return a Relation describing the relation variables.
relationVariablesAsRelation :: DatabaseContext -> TransactionGraph -> Either RelationalError Relation
relationVariablesAsRelation ctx graph = do
  let subrelAttrs = A.attributesFromList [Attribute "attribute" TextAtomType, Attribute "type" TextAtomType]
      attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                  Attribute "attributes" (RelationAtomType subrelAttrs)]
      relVars = relationVariables ctx
      mkRvDesc (rvName, gfExpr) = do
        let gfEnv = freshGraphRefRelationalExprEnv (Just ctx) graph
        gfType <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr)
        pure (rvName, gfType)
      relVarToAtomList (rvName, rel) = [TextAtom rvName, attributesToRel (attributesVec (attributes rel))]
      attrAtoms a = [TextAtom (A.attributeName a), TextAtom (prettyAtomType (A.atomType a))]
      attributesToRel attrl = case mkRelationFromList subrelAttrs (map attrAtoms (V.toList attrl)) of
        Left err -> error ("relationVariablesAsRelation pooped " ++ show err)
        Right rel -> RelationAtom rel
  rvs <- mapM mkRvDesc (M.toList relVars)
  let tups = map relVarToAtomList rvs
  mkRelationFromList attrs tups

-- | An unoptimized variant of evalGraphRefRelationalExpr for testing.
evalRelationalExpr :: RelationalExpr -> RelationalExprM Relation
evalRelationalExpr expr = do
  graph <- reGraph
  context <- reContext
  let expr' = runProcessExprM UncommittedContextMarker (processRelationalExpr expr)
      gfEnv = freshGraphRefRelationalExprEnv (Just context) graph
  case runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr expr') of
    Left err -> throwError err
    Right rel -> pure rel

class (MonadError RelationalError m, Monad m) => DatabaseContextM m where
  getContext :: m DatabaseContext
  
instance DatabaseContextM (ReaderT GraphRefRelationalExprEnv (ExceptT RelationalError Identity)) where
  getContext = gfDatabaseContextForMarker UncommittedContextMarker

instance DatabaseContextM (RWST DatabaseContextEvalEnv () DatabaseContextEvalState (ExceptT RelationalError Identity)) where
  getContext = getStateContext
    
relVarByName :: DatabaseContextM m => RelVarName -> m GraphRefRelationalExpr
relVarByName rvName = do
  relvars <- relationVariables <$> getContext  
  case M.lookup rvName relvars of
    Nothing -> throwError (RelVarNotDefinedError rvName)
    Just gfexpr -> pure gfexpr
  
-- | resolve UncommittedTransactionMarker whenever possible- this is important in the DatabaseContext in order to mitigate self-referencing loops for updates
class ResolveGraphRefTransactionMarker a where
  resolve :: a -> DatabaseContextEvalMonad a

-- s := s union t
instance ResolveGraphRefTransactionMarker GraphRefRelationalExpr where
  resolve (MakeRelationFromExprs mAttrs tupleExprs) =
    MakeRelationFromExprs mAttrs <$> resolve tupleExprs
  resolve orig@MakeStaticRelation{} = pure orig
  resolve orig@ExistingRelation{} = pure orig
  resolve orig@RelationValuedAttribute{} = pure orig
  resolve orig@(RelationVariable rvName UncommittedContextMarker) = do
    rvMap <- relationVariables <$> getStateContext
    case M.lookup rvName rvMap of
      Nothing -> pure orig
      Just resolvedRv -> resolve resolvedRv
  resolve orig@RelationVariable{} = pure orig
  resolve (Project attrNames relExpr) = Project <$> resolve attrNames <*> resolve relExpr
  resolve (Union exprA exprB) = Union <$> resolve exprA <*> resolve exprB
  resolve (Join exprA exprB) = Join <$> resolve exprA <*> resolve exprB
  resolve (Rename attrs expr) = Rename attrs <$> resolve expr
  resolve (Difference exprA exprB) = Difference <$> resolve exprA <*> resolve exprB
  resolve (Group namesA nameB expr) = Group <$> resolve namesA <*> pure nameB <*> resolve expr
  resolve (Ungroup nameA expr) = Ungroup nameA <$> resolve expr
  resolve (Restrict restrictExpr relExpr) = Restrict <$> resolve restrictExpr <*> resolve relExpr
  resolve (Equals exprA exprB) = Equals <$> resolve exprA <*> resolve exprB
  resolve (NotEquals exprA exprB) = NotEquals <$> resolve exprA <*> resolve exprB
  resolve (Extend extendExpr relExpr) = Extend <$> resolve extendExpr <*> resolve relExpr
  resolve (With withExprs relExpr) = With <$> mapM (\(nam, expr) -> (,) <$> resolve nam <*> resolve expr) withExprs <*> resolve relExpr

instance ResolveGraphRefTransactionMarker GraphRefTupleExprs where
  resolve (TupleExprs marker tupleExprs) =
    TupleExprs marker <$> mapM resolve tupleExprs

instance ResolveGraphRefTransactionMarker GraphRefTupleExpr where
  resolve (TupleExpr tupMap) = do
    tupMap' <- mapM (\(attrName, expr) -> (,) attrName <$> resolve expr ) (M.toList tupMap)
    pure (TupleExpr (M.fromList tupMap'))

instance ResolveGraphRefTransactionMarker GraphRefAttributeNames where
  resolve orig@AttributeNames{} = pure orig
  resolve orig@InvertedAttributeNames{} = pure orig
  resolve (UnionAttributeNames namesA namesB) = UnionAttributeNames <$> resolve namesA <*> resolve namesB
  resolve (IntersectAttributeNames namesA namesB) = IntersectAttributeNames <$> resolve namesA <*> resolve namesB
  resolve (RelationalExprAttributeNames expr) = RelationalExprAttributeNames <$> resolve expr

instance ResolveGraphRefTransactionMarker GraphRefRestrictionPredicateExpr where
  resolve TruePredicate = pure TruePredicate
  resolve (AndPredicate exprA exprB) = AndPredicate <$> resolve exprA <*> resolve exprB
  resolve (OrPredicate exprA exprB) = OrPredicate <$> resolve exprA <*> resolve exprB
  resolve (NotPredicate expr) = NotPredicate <$> resolve expr
  resolve (RelationalExprPredicate expr) = RelationalExprPredicate <$> resolve expr
  resolve (AtomExprPredicate expr) = AtomExprPredicate <$> resolve expr
  resolve (AttributeEqualityPredicate nam expr)= AttributeEqualityPredicate nam <$> resolve expr

instance ResolveGraphRefTransactionMarker GraphRefExtendTupleExpr where
  resolve (AttributeExtendTupleExpr nam atomExpr) = AttributeExtendTupleExpr nam <$> resolve atomExpr

instance ResolveGraphRefTransactionMarker GraphRefWithNameExpr where
  resolve orig@WithNameExpr{} = pure orig -- match uncommitted marker?

instance ResolveGraphRefTransactionMarker GraphRefAtomExpr where
  resolve orig@AttributeAtomExpr{} = pure orig
  resolve orig@NakedAtomExpr{} = pure orig
  resolve (AggregateFunctionAtomExpr nam aggInfo args marker) =
    AggregateFunctionAtomExpr nam aggInfo <$> mapM resolve args <*> pure marker
  resolve (FunctionAtomExpr nam atomExprs marker) =
    FunctionAtomExpr nam <$> mapM resolve atomExprs <*> pure marker
  resolve (RelationAtomExpr expr) = RelationAtomExpr <$> resolve expr
  resolve (IfThenAtomExpr ifExpr thenExpr elseExpr) = IfThenAtomExpr <$> resolve ifExpr <*> resolve thenExpr <*> resolve elseExpr
  resolve (ConstructedAtomExpr dConsName atomExprs marker) =
    ConstructedAtomExpr dConsName <$> mapM resolve atomExprs <*> pure marker

--convert series of simple Union queries into MakeStaticRelation
-- this is especially useful for long, nested applications of Union with simple tuples
-- Union (MakeRelation x y) (MakeRelation x y') -> MakeRelation x (y + y')

--MakeRelationFromExprs Nothing (TupleExprs UncommittedContextMarker [TupleExpr (fromList [("name", NakedAtomExpr (TextAtom "steve"))])])

applyUnionCollapse :: GraphRefRelationalExpr -> GraphRefRelationalExpr
applyUnionCollapse = Fold.cata opt
  where
    opt :: RelationalExprBaseF GraphRefTransactionMarker GraphRefRelationalExpr -> GraphRefRelationalExpr
    opt (UnionF exprA exprB) | exprA == exprB = exprA
    opt (UnionF
         exprA@(MakeRelationFromExprs mAttrs1 tupExprs1)
         exprB@(MakeRelationFromExprs mAttrs2 tupExprs2)) | tupExprs1 == tupExprs2 = MakeRelationFromExprs (mAttrs1 <|> mAttrs2) tupExprs1
                                                    | tupExprsNull tupExprs1 = exprB
                                                    | tupExprsNull tupExprs2 = exprA
    opt x = Fold.embed x
    tupExprsNull (TupleExprs _ []) = True
    tupExprsNull _ = False


--UPDATE optimization- find matching where clause in "lower" levels of renaming
--update x where y=1 set (x:=5,z:=10); update x where y=1 set(x:=6,z:=11)
-- =>
-- update x where y=1 set (x:=6,z:=11)
-- future opt: match individual attributes update x where y=1 set (x:=5); update x where y=1 set (z:=11) => update x where y=1 set (x:=5,z:=11)

--strategy: try to collapse the top-level update (union (restrict pred MakeRelationFromExpr) expr) if it contains the same predicate and resultant relation

--DELETE optimization
-- if a restriction matches a previous restriction, combine them
-- O(1) since it only scans at the top level, critical in benchmarks generating redundant deletions
applyRestrictionCollapse :: GraphRefRelationalExpr -> GraphRefRelationalExpr
applyRestrictionCollapse orig@(Restrict npred@(NotPredicate _) expr) =
  case expr of
    orig'@(Restrict npred'@(NotPredicate _) _) | npred == npred' -> orig'
    _ -> orig
applyRestrictionCollapse expr = expr

firstAtomForAttributeName :: AttributeName -> [RelationTuple] -> GraphRefRelationalExprM Atom
firstAtomForAttributeName attrName tuples = do
  let folder tup acc =
        case atomForAttributeName attrName tup of
          Left{} -> acc
          Right atom -> Just atom
  case foldr folder Nothing tuples of
    Nothing -> throwError (NoSuchAttributeNamesError (S.singleton attrName))
    Just match -> pure match

-- | Optionally add type hints to resolve type variables. For example, if we are inserting into a known relvar, then we have its concrete type.    
addTargetTypeHints :: Attributes -> GraphRefRelationalExpr -> GraphRefRelationalExpr
addTargetTypeHints targetAttrs expr =
  case expr of
    MakeRelationFromExprs Nothing tupExprs ->
      MakeRelationFromExprs (Just targetAttrExprs) tupExprs
    Project attrs e ->
      Project attrs (hint e)
    Union a b ->
      Union (hint a) (hint b)
    Join a b ->
      Join (hint a) (hint b)
    Rename rens e ->
      let renamedAttrs = A.renameAttributes' (S.map swap rens) targetAttrs in
      Rename rens (addTargetTypeHints renamedAttrs e)
    Difference a b ->
      Difference (hint a) (hint b)
    Group attrs gname e ->
      Group attrs gname (hint e)
    Ungroup gname e ->
      Ungroup gname (hint e)
    Restrict restriction e ->
      Restrict restriction (hint e)
    Equals a b ->
      Equals (hint a) (hint b)
    NotEquals a b ->
      NotEquals (hint a) (hint b)
    Extend tupExprs e ->
      Extend tupExprs (hint e)
    With withs e ->
      With withs (hint e)
    _ -> expr
  where
    targetAttrExprs = map NakedAttributeExpr (A.toList targetAttrs)
    hint = addTargetTypeHints targetAttrs
