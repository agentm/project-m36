{-# LANGUAGE ScopedTypeVariables #-}
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
import ProjectM36.Transaction
import qualified ProjectM36.Attribute as A
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.Set as S
import Control.Monad.Trans.RWS.Strict as RWS
import Control.Monad.Trans.Class (lift)
import Control.Exception
import Data.Maybe
import Data.Either
import Data.Char (isUpper)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified ProjectM36.TypeConstructorDef as TCD
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader as R
import Test.QuickCheck
import GHC
import GHC.Paths
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.List.NonEmpty as NE
import Data.Functor.Identity
import Control.Monad
import Control.Monad.IO.Class

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
  re_currentTransId :: TransactionId,
  re_extra :: Maybe (Either RelationTuple Attributes)
  }

stateTuple :: RelationalExprEnv -> RelationTuple
stateTuple e = fromLeft emptyTuple (fromMaybe (Left emptyTuple) (re_extra e))

stateAttributes :: RelationalExprEnv -> Attributes
stateAttributes e = fromRight emptyAttributes (fromMaybe (Right emptyAttributes) (re_extra e))
  
instance Show RelationalExprEnv where
  show e@RelationalExprEnv{} = "RelationalExprEnv " ++ show (re_extra e)

mkRelationalExprEnv :: DatabaseContext -> TransactionId -> TransactionGraph -> RelationalExprEnv
mkRelationalExprEnv ctx currentTransId graph =
  RelationalExprEnv
  { re_context = ctx,
    re_graph = graph,
    re_currentTransId = currentTransId,
    re_extra = Nothing }

askEnv :: RelationalExprState RelationalExprEnv
askEnv = R.ask

currentTrans :: RelationalExprState (Either RelationalError Transaction)
currentTrans = do
  tid <- re_currentTransId <$> R.ask
  graph <- re_graph <$> R.ask
  pure (transactionForId tid graph)
  
mergeTuplesIntoRelationalExprState :: RelationTuple -> RelationalExprEnv -> RelationalExprEnv
mergeTuplesIntoRelationalExprState tupIn e@RelationalExprEnv{} =
  e{re_extra = new_elems }
  where
    new_elems = Just (Left newTuple)
    mergedTupMap = M.union (tupleToMap tupIn) (tupleToMap (stateTuple e))
    newTuple = mkRelationTupleFromMap mergedTupMap
  
mergeAttributesIntoRelationalExprState :: Attributes -> RelationalExprEnv -> RelationalExprEnv
mergeAttributesIntoRelationalExprState attrsIn e@RelationalExprEnv{} = e{re_extra = newattrs }
  where
    newattrs = Just (Right (A.union attrsIn (stateAttributes e)))

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
mkDatabaseContextEvalEnv tid graph = DatabaseContextEvalEnv tid graph

type DatabaseContextEvalMonad a = RWST DatabaseContextEvalEnv () DatabaseContextEvalState (ExceptT RelationalError Identity) a

runDatabaseContextEvalMonad :: DatabaseContext -> DatabaseContextEvalEnv -> DatabaseContextEvalMonad () -> Either RelationalError DatabaseContextEvalState
runDatabaseContextEvalMonad ctx env m = runIdentity (runExceptT (fst <$> (execRWST m env freshEnv)))
  where
    freshEnv = mkDatabaseContextEvalState ctx
    

dbcTransId :: DatabaseContextEvalMonad TransactionId
dbcTransId = dce_transId <$> RWS.ask

dbcGraph :: DatabaseContextEvalMonad TransactionGraph
dbcGraph = dce_graph <$> RWS.ask

dbcRelationalExprEnv :: DatabaseContextEvalMonad RelationalExprEnv
dbcRelationalExprEnv = 
  mkRelationalExprEnv <$> getStateContext <*> dbcTransId <*> dbcGraph

getStateContext :: DatabaseContextEvalMonad DatabaseContext
getStateContext = dbc_context <$> get

putStateContext :: DatabaseContext -> DatabaseContextEvalMonad () 
putStateContext ctx' = do
  s <- get
  put (s {dbc_context = ctx'}) 
  
type RelationalExprState a = Reader RelationalExprEnv a

type GraphRefRelationalExprR a = Reader TransactionGraph a

envContext :: RelationalExprEnv -> DatabaseContext
envContext = re_context

setEnvContext :: RelationalExprEnv -> DatabaseContext -> RelationalExprEnv
setEnvContext e ctx = e { re_context = ctx }

{-
--full evaluation down the graph
eval :: RelationalExpr -> RelationalExprState (Either RelationalError Relation)
eval expr = do
  env <- askEnv
  eGfExpr <- processRelationalExpr expr
  case eGfExpr of
    Left err -> pure (Left err)
    Right gfExpr -> pure $ evalGraphRefRelationalExpr gfExpr (re_graph env)
-}
{-  
--relvar state is needed in evaluation of relational expression but only as read-only in order to extract current relvar values
evalRelationalExpr :: RelationalExpr -> RelationalExprState (Either RelationalError GraphRefRelationalExpr)
evalRelationalExpr (RelationVariable name _) = do
  relvarTable <- fmap (relationVariables . envContext) askEnv
  return $ case M.lookup name relvarTable of
    Just res -> Right res
    Nothing -> Left $ RelVarNotDefinedError name

evalRelationalExpr (Project attrNames expr) = do
    eAttrNameSet <- evalAttributeNames attrNames expr
    case eAttrNameSet of
      Left err -> pure (Left err)
      Right attrNameSet -> do
        rel <- evalRelationalExpr expr
        case rel of
          Right rel2 -> pure $ Right (Project (AttributeNames attrNameSet) rel2)
          Left err -> pure $ Left err

evalRelationalExpr (Union exprA exprB) = do
  relA <- evalRelationalExpr exprA
  relB <- evalRelationalExpr exprB
  case relA of
    Left err -> return $ Left err
    Right relA2 -> case relB of
      Left err -> return $ Left err
      Right relB2 -> return $ Right (Union relA2 relB2)

evalRelationalExpr (Join exprA exprB) = do
  relA <- evalRelationalExpr exprA
  relB <- evalRelationalExpr exprB
  case relA of
    Left err -> return $ Left err
    Right relA2 -> case relB of
      Left err -> return $ Left err
      Right relB2 -> return $ Right (Join relA2 relB2)
      
evalRelationalExpr (Difference exprA exprB) = do
  relA <- evalRelationalExpr exprA
  relB <- evalRelationalExpr exprB
  case relA of
    Left err -> return $ Left err
    Right relA2 -> case relB of
      Left err -> return $ Left err
      Right relB2 -> return $ Right (Difference relA2 relB2)
      
evalRelationalExpr (MakeStaticRelation attributeSet tupleSet) = 
  case mkRelation attributeSet tupleSet of
    Right rel -> return $ Right (ExistingRelation rel)
    Left err -> return $ Left err
    
evalRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) = do
  currentContext <- fmap envContext askEnv
  let tConss = typeConstructorMapping currentContext
  -- if the mAttrExprs is Nothing, then we should attempt to infer the tuple attributes from the first tuple itself- note that this is not always possible
  runExceptT $ do
    mAttrs <- case mAttrExprs of
      Just _ ->
        Just . A.attributesFromList <$> mapM evalGraphRefAttrExpr (fromMaybe [] mAttrExprs)
      Nothing -> pure Nothing
    tuples <- mapM (liftE . evalTupleExpr mAttrs) tupleExprs
    let attrs = fromMaybe firstTupleAttrs mAttrs
        firstTupleAttrs = if null tuples then A.emptyAttributes else tupleAttributes (head tuples)
    expr <- either throwE pure (mkRelation attrs (RelationTupleSet tuples))
    pure (ExistingRelation expr)
  
evalRelationalExpr (ExistingRelation rel) = pure (Right (ExistingRelation rel))

evalRelationalExpr (Rename oldAttrName newAttrName relExpr) = do
  evald <- evalRelationalExpr relExpr
  case evald of
    Right expr -> return $ Right (Rename oldAttrName newAttrName expr)
    Left err -> return $ Left err

evalRelationalExpr (Group oldAttrNames newAttrName relExpr) = do
  eOldAttrNameSet <- evalAttributeNames oldAttrNames relExpr
  case eOldAttrNameSet of
    Left err -> pure (Left err)
    Right oldAttrNameSet -> do
      evald <- evalRelationalExpr relExpr
      case evald of
        Right expr -> return $ Right (Group (AttributeNames oldAttrNameSet) newAttrName expr)
        Left err -> return $ Left err

evalRelationalExpr (Ungroup attrName relExpr) = do
  evald <- evalRelationalExpr relExpr
  case evald of
    Right expr -> return $ Right (Ungroup attrName expr)
    Left err -> return $ Left err

evalRelationalExpr (Restrict predicateExpr relExpr) = do
  evald <- evalRelationalExpr relExpr
  pred <- processRestrictionPredicateExpr predicateExpr
  case evald of
    Left err -> return $ Left err
    Right expr -> 
      pure $ Right (Restrict pred expr)

evalRelationalExpr (Equals relExprA relExprB) = do
  evaldA <- evalRelationalExpr relExprA
  evaldB <- evalRelationalExpr relExprB
  case evaldA of
    Left err -> return $ Left err
    Right exprA -> case evaldB of
      Left err -> return $ Left err
      Right exprB -> return $ Right (Equals exprA exprB)
{-
evalRelationalExpr (With views mainExpr) = do
  rstate <- ask
  
  let addScopedView ctx (vname,vexpr) = if vname `M.member` relationVariables ctx then
                                          Left (RelVarAlreadyDefinedError vname)
                                        else
                                          case runState (evalDatabaseContextExpr (Assign vname vexpr)) (freshDatabaseState ctx) of
                                            (Left err,_) -> Left err
                                            (Right (), (ctx',_,_)) -> Right ctx'

  case foldM addScopedView (stateElemsContext rstate) views of
       Left err -> return $ Left err
       Right ctx'' -> do 
         let evalMainExpr expr = runReader (evalRelationalExpr expr) (RelationalExprEnv ctx'')
         case evalMainExpr mainExpr of
              Left err -> return $ Left err
              Right rel -> return $ Right rel 
-}
--warning: copy-pasta from above- refactor
evalRelationalExpr (NotEquals relExprA relExprB) = do
  evaldA <- evalRelationalExpr relExprA
  evaldB <- evalRelationalExpr relExprB
  case evaldA of
    Left err -> return $ Left err
    Right exprA -> case evaldB of
      Left err -> return $ Left err
      Right exprB -> return $ Right (NotEquals exprA exprB)

-- extending a relation adds a single attribute with the results of the per-tuple expression evaluated
evalRelationalExpr (Extend tupleExpression relExpr) = do
  eExpr <- evalRelationalExpr relExpr
  case eExpr of
    Left err -> pure (Left err)
    Right expr -> do
      tupProc <- processExtendTupleExpr tupleExpression
      pure (Right (Extend tupProc expr))
-}
--helper function to process relation variable creation/assignment
setRelVar :: RelVarName -> GraphRefRelationalExpr -> DatabaseContextEvalMonad ()
setRelVar relVarName relExpr = do

  currentContext <- getStateContext
  let newRelVars = M.insert relVarName relExpr $ relationVariables currentContext
      potentialContext = currentContext { relationVariables = newRelVars }
  --determine when to check constraints
  graph <- dbcGraph
  tid <- dbcTransId
  case checkConstraints potentialContext tid graph of
    Left err -> dbErr err
    Right _ -> putStateContext potentialContext

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
    putStateContext newContext
    pure ()

evalGraphRefDatabaseContextExpr :: GraphRefDatabaseContextExpr -> DatabaseContextEvalMonad ()
evalGraphRefDatabaseContextExpr NoOperation = pure ()
  
evalGraphRefDatabaseContextExpr (Define relVarName attrExprs) = do
  relvars <- fmap relationVariables getStateContext
  tConss <- fmap typeConstructorMapping getStateContext
  relEnv <- dbcRelationalExprEnv
  let eAttrs = runReader (mapM evalGraphRefAttrExpr attrExprs) relEnv
      attrErrs = lefts eAttrs
      attrsList = rights eAttrs
  if not (null  attrErrs) then
    dbErr (someErrors attrErrs)
    else do
      let atomTypeErrs = lefts $ map ((`validateAtomType` tConss) . A.atomType) attrsList
      if not (null atomTypeErrs) then
        dbErr (someErrors atomTypeErrs)
        else 
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
  case existingRelVar of
    Nothing -> do
      setRelVar relVarName expr
    Just existingRel -> do
      let eExpectedType = runReader (typeForGraphRefRelationalExpr existingRel) graph
          eNewExprType = runReader (typeForGraphRefRelationalExpr expr) graph
      case eExpectedType of
        Left err -> dbErr err
        Right expectedType ->
          case eNewExprType of
            Left err -> dbErr err
            Right newExprType -> do
              if newExprType == expectedType then do
                setRelVar relVarName expr 
              else
                dbErr (RelationTypeMismatchError (attributes expectedType) (attributes newExprType))

evalGraphRefDatabaseContextExpr (Insert relVarName relExpr) = do
  transId <- dbcTransId
  evalGraphRefDatabaseContextExpr (Assign relVarName
                                   (Union
                                    (RelationVariable relVarName transId)
                                    relExpr))

evalGraphRefDatabaseContextExpr (Delete relVarName predicate) = do
  transId <- dbcTransId
  let rv = RelationVariable relVarName transId
  setRelVar relVarName (Restrict (NotPredicate predicate) rv)
  
--union of restricted+updated portion and the unrestricted+unupdated portion
evalGraphRefDatabaseContextExpr (Update relVarName atomExprMap pred') = do
  context <- getStateContext
  transId <- dbcTransId
  reEnv <- dbcRelationalExprEnv
  let relVarMap = relationVariables context 
  case M.lookup relVarName relVarMap of
        Nothing -> dbErr (RelVarNotDefinedError relVarName)
        Just _ -> do
          let unrestrictedPortion = Restrict (NotPredicate pred') rv
              rv = RelationVariable relVarName transId
              tmpAttr attr = "_tmp_" <> attr --this could certainly be improved to verify that there is no attribute name conflict
              updateAttr nam atomExpr = Extend (AttributeExtendTupleExpr (tmpAttr nam) atomExpr)
              projectAndRename attr expr = Rename (tmpAttr attr) attr (Project (InvertedAttributeNames (S.singleton attr)) expr)
              restrictedPortion = Restrict pred'
              updated = foldr (\(oldname, atomExpr) accum ->
                                 let procAtomExpr = runReader (processAtomExpr atomExpr) reEnv in
                                  updateAttr oldname procAtomExpr accum
                              ) rv (M.toList atomExprMap)
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
  case validateTypeConstructorDef tConsDef dConsDefList of
    errs@(_:_) -> dbErr (someErrors errs)
    [] | T.null tConsName || not (isUpper (T.head tConsName)) -> dbErr (InvalidAtomTypeName tConsName)
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

evalGraphRefDatabaseContextExpr (MultipleExpr _) = do
  --the multiple expressions must pass the same context around- not the old unmodified context
  unimplemented
{-  evald <- forM exprs (\expr -> evalGraphRefDatabaseContextExpr expr)
  --some lifting magic needed here
  case lefts evald of
    [] -> pure ()
    errs -> dbErr (someErrors errs)
  -}           
evalGraphRefDatabaseContextExpr (RemoveAtomFunction funcName) = do
  currentContext <- getStateContext
  let atomFuncs = atomFunctions currentContext
  case atomFunctionForName funcName atomFuncs of
    Left err -> dbErr err
    Right realFunc ->
      if isScriptedAtomFunction realFunc then do
        let updatedFuncs = HS.delete realFunc atomFuncs
        putStateContext (currentContext {atomFunctions = updatedFuncs })
      else
        dbErr (PrecompiledFunctionRemoveError funcName)
      
evalGraphRefDatabaseContextExpr (RemoveDatabaseContextFunction funcName) = do      
  context <- getStateContext
  let dbcFuncs = dbcFunctions context
  case databaseContextFunctionForName funcName dbcFuncs of
    Left err -> dbErr err
    Right realFunc ->
      if isScriptedDatabaseContextFunction realFunc then do
        let updatedFuncs = HS.delete realFunc dbcFuncs
        putStateContext (context { dbcFunctions = updatedFuncs })
      else
        dbErr (PrecompiledFunctionRemoveError funcName)
      
evalGraphRefDatabaseContextExpr (ExecuteDatabaseContextFunction funcName atomArgExprs) = do
  context <- getStateContext
  relExprEnv <- dbcRelationalExprEnv
  --resolve atom arguments
  let eAtomTypes = map (\atomExpr -> runReader (typeForGraphRefAtomExpr emptyAttributes atomExpr) relExprEnv) atomArgExprs
      eFunc = databaseContextFunctionForName funcName (dbcFunctions context)
  case eFunc of
      Left err -> dbErr err
      Right func -> do
        let expectedArgCount = length (dbcFuncType func)
            actualArgCount = length atomArgExprs
        if expectedArgCount /= actualArgCount then
          dbErr (FunctionArgumentCountMismatchError expectedArgCount actualArgCount)
          else 
          --check that the atom types are valid
          case lefts eAtomTypes of
            _:_ -> dbErr (someErrors (lefts eAtomTypes))
            [] -> do
              let atomTypes = rights eAtomTypes
              let mValidTypes = map (\(expType, actType) -> case atomTypeVerify expType actType of 
                                        Left err -> Just err
                                        Right _ -> Nothing) (zip (dbcFuncType func) atomTypes)
                  typeErrors = catMaybes mValidTypes
                  eAtomArgs = unimplemented
                  --eAtomArgs = map (\arg -> runReader (evalAtomExpr emptyTuple arg) relExprState) atomArgExprs
              if length (lefts eAtomArgs) > 1 then
                dbErr (someErrors (lefts eAtomArgs))
                else if not (null typeErrors) then
                     dbErr (someErrors typeErrors)
                   else
                     case evalDatabaseContextFunction func (rights eAtomArgs) context of
                       Left err -> dbErr err
                       Right newContext -> putStateContext newContext

data DatabaseContextIOEvalEnv = DatabaseContextIOEvalEnv
  { dbcio_transId :: TransactionId,
    dbcio_graph :: TransactionGraph,
    dbcio_mScriptSession :: Maybe ScriptSession
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
  env <- RWS.ask
  pure $ mkRelationalExprEnv context (dbcio_transId env) (dbcio_graph env)

evalGraphRefDatabaseContextIOExpr :: GraphRefDatabaseContextIOExpr -> DatabaseContextIOEvalMonad (Either RelationalError ())
evalGraphRefDatabaseContextIOExpr (AddAtomFunction funcName funcType script) = do
  eScriptSession <- requireScriptSession
  currentContext <- getDBCIOContext
  case eScriptSession of
    Left err -> pure (Left err)
    Right scriptSession -> do
      res <- liftIO $ try $ runGhc (Just libdir) $ do
        setSession (hscEnv scriptSession)
        let atomFuncs = atomFunctions currentContext
        case extractAtomFunctionType funcType of
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
                    newAtomFunc = AtomFunction { atomFuncName = funcName,
                                                 atomFuncType = funcAtomType,
                                                 atomFuncBody = AtomFunctionBody (Just script) compiledFunc }
               -- check if the name is already in use
                if HS.member funcName (HS.map atomFuncName atomFuncs) then
                  Left (FunctionNameInUseError funcName)
                  else 
                  Right newContext
      case res of
        Left (exc :: SomeException) -> pure $ Left (ScriptError (OtherScriptCompilationError (show exc)))
        Right eContext -> case eContext of
          Left err -> pure (Left err)
          Right context' -> putDBCIOContext context'
          
evalGraphRefDatabaseContextIOExpr (AddDatabaseContextFunction funcName funcType script) = do
  eScriptSession <- requireScriptSession
  currentContext <- getDBCIOContext
  case eScriptSession of
    Left err -> pure (Left err)
    Right scriptSession -> do
      --validate that the function signature is of the form x -> y -> ... -> DatabaseContext -> DatabaseContext
      let last2Args = reverse (take 2 (reverse funcType))
          atomArgs = take (length funcType - 2) funcType
          dbContextTypeCons = ADTypeConstructor "Either" [ADTypeConstructor "DatabaseContextFunctionError" [], ADTypeConstructor "DatabaseContext" []]
          expectedType = "DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext"
          actualType = show funcType
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
                  newDBCFunc = DatabaseContextFunction {
                    dbcFuncName = funcName,
                    dbcFuncType = funcAtomType,
                    dbcFuncBody = DatabaseContextFunctionBody (Just script) compiledFunc
                    }
                -- check if the name is already in use                                              
              if HS.member funcName (HS.map dbcFuncName dbcFuncs) then
                Left (FunctionNameInUseError funcName)
                else 
                Right newContext
        case res of
          Left (exc :: SomeException) -> pure $ Left (ScriptError (OtherScriptCompilationError (show exc)))
          Right eContext -> case eContext of
            Left err -> pure (Left err)
            Right context' -> putDBCIOContext context'
              
evalGraphRefDatabaseContextIOExpr (LoadAtomFunctions modName funcName modPath) = do
  currentContext <- getDBCIOContext
  eLoadFunc <- liftIO $ loadAtomFunctions (T.unpack modName) (T.unpack funcName) modPath
  case eLoadFunc of
    Left LoadSymbolError -> pure (Left LoadFunctionError)
    Right atomFunctionListFunc -> let newContext = currentContext { atomFunctions = mergedFuncs }
                                      mergedFuncs = HS.union (atomFunctions currentContext) (HS.fromList atomFunctionListFunc)
                                  in putDBCIOContext newContext
evalGraphRefDatabaseContextIOExpr (LoadDatabaseContextFunctions modName funcName modPath) = do
  currentContext <- getDBCIOContext
  eLoadFunc <- liftIO $ loadDatabaseContextFunctions (T.unpack modName) (T.unpack funcName) modPath
  case eLoadFunc of
    Left LoadSymbolError -> pure (Left LoadFunctionError)
    Right dbcListFunc -> let newContext = currentContext { dbcFunctions = mergedFuncs }
                             mergedFuncs = HS.union (dbcFunctions currentContext) (HS.fromList dbcListFunc)
                                  in putDBCIOContext newContext

evalGraphRefDatabaseContextIOExpr (CreateArbitraryRelation relVarName attrExprs range) = do
  --Define
  currentContext <- getDBCIOContext
  env <- RWS.ask
  --create graph ref expr
  let gfExpr = Define relVarName attrExprs
      evalEnv = mkDatabaseContextEvalEnv (dbcio_transId env) (dbcio_graph env)
      transId = dbcio_transId env
      graph = dbcio_graph env
  case runDatabaseContextEvalMonad currentContext evalEnv (evalGraphRefDatabaseContextExpr gfExpr) of
    Left err -> pure (Left err)
    Right state -> do
         --Assign
           let existingRelVar = M.lookup relVarName relVarTable
               relVarTable = relationVariables (dbc_context state)
           case existingRelVar of
                Nothing -> pure $ Left (RelVarNotDefinedError relVarName)
                Just existingRel -> do
                  case runReader (typeForGraphRefRelationalExpr existingRel) graph of
                    Left err -> pure (Left err)
                    Right relType -> do
                      let expectedAttributes = attributes relType
                          tcMap = typeConstructorMapping (dbc_context state)
                      eitherRel <- liftIO $ generate $ runReaderT (arbitraryRelation expectedAttributes range) tcMap
                      case eitherRel of
                        Left err -> pure $ Left err
                        Right rel ->
                          case runDatabaseContextEvalMonad currentContext evalEnv (setRelVar relVarName (ExistingRelation rel)) of
                            Left err -> pure (Left err)
                            Right state -> putDBCIOContext (dbc_context state)

updateTupleWithAtomExprs :: M.Map AttributeName AtomExpr -> DatabaseContext -> TransactionGraph -> RelationTuple -> Either RelationalError RelationTuple
updateTupleWithAtomExprs exprMap context graph tupIn = do
  --resolve all atom exprs
  atomsAssoc <- mapM (\(attrName, atomExpr) -> do
                         let atom = unimplemented
                         --atom <- runReader (evalAtomExpr tupIn atomExpr) (mkRelationalExprState context graph)
                         pure (attrName, atom)
                     ) (M.toList exprMap)
  pure (updateTupleWithAtoms (M.fromList atomsAssoc) tupIn)

--run verification on all constraints
checkConstraints :: DatabaseContext -> TransactionId -> TransactionGraph -> Either RelationalError ()
checkConstraints context transId graph@(TransactionGraph graphHeads transSet) =
  mapM_ (uncurry checkIncDep) (M.toList deps) 
  where
    potentialGraph = TransactionGraph graphHeads (S.insert tempTrans transSet)
    tempStamp = UTCTime { utctDay = fromGregorian 2000 1 1,
                          utctDayTime = secondsToDiffTime 0 }
    tempSchemas = Schemas context M.empty
    tempTrans = Transaction U.nil (TransactionInfo (transId NE.:| []) tempStamp) tempSchemas
    
    deps = inclusionDependencies context
    reEnv = mkRelationalExprEnv context transId graph
      -- no optimization available here, really? perhaps the optimizer should be passed down to here or the eval function should be passed through the environment
    checkIncDep depName (InclusionDependency subsetExpr supersetExpr) = do
      let gfSubsetExpr = runReader (processRelationalExpr subsetExpr) reEnv
          gfSupersetExpr = runReader (processRelationalExpr supersetExpr) reEnv
      --if both expressions are of a single-attribute (such as with a simple foreign key), the names of the attributes are irrelevant (they need not match) because the expression is unambiguous, but special-casing this to rename the attribute automatically would not be orthogonal behavior and probably cause confusion. Instead, special case the error to make it clear.
      typeSub <- runReader (typeForRelationalExpr subsetExpr) reEnv
      typeSuper <- runReader (typeForRelationalExpr supersetExpr) reEnv
      when (typeSub /= typeSuper) (Left (RelationTypeMismatchError (attributes typeSub) (attributes typeSuper)))
      let checkExpr = Equals gfSupersetExpr (Union gfSubsetExpr gfSupersetExpr)
      case evalGraphRefRelationalExpr checkExpr potentialGraph of
        Left err -> Left err
        Right resultRel -> if resultRel == relationTrue then
                                   pure ()
                                else 
                                  Left (InclusionDependencyCheckError depName)

-- the type of a relational expression is equal to the relation attribute set returned from executing the relational expression; therefore, the type can be cheaply derived by evaluating a relational expression and ignoring and tuple processing
-- furthermore, the type of a relational expression is the resultant header of the evaluated empty-tupled relation

typeForRelationalExpr :: RelationalExpr -> RelationalExprState (Either RelationalError Relation)
typeForRelationalExpr expr = do
  --replace the relationVariables context element with a cloned set of relation devoid of tuples
  --evalRelationalExpr could still return an existing relation with tuples, so strip them
  gfExpr <- processRelationalExpr expr
  graph <- re_graph <$> askEnv
  pure $ runReader (typeForGraphRefRelationalExpr gfExpr) graph

--returns a database context with all tuples removed
--this is useful for type checking and optimization
{-
contextWithEmptyTupleSets :: DatabaseContext -> DatabaseContext
contextWithEmptyTupleSets contextIn = contextIn { relationVariables = relVars }
  where
    relVars = M.map (\rel -> ExistingRelation (Relation (attributes rel) emptyTupleSet)) (relationVariables contextIn)
-}

liftE :: (Monad m) => m (Either a b) -> ExceptT a m b
liftE v = do
  y <- lift v
  case y of
    Left err -> throwE err
    Right val -> pure val

{- used for restrictions- take the restrictionpredicate and return the corresponding filter function -}
predicateRestrictionFilter :: Attributes -> RestrictionPredicateExprBase a -> RelationalExprState (Either RelationalError RestrictionFilter)
predicateRestrictionFilter attrs (AndPredicate expr1 expr2) = 
  runExceptT $ do
    expr1v <- liftE (predicateRestrictionFilter attrs expr1)
    expr2v <- liftE (predicateRestrictionFilter attrs expr2)
    pure (\x -> do
                ev1 <- expr1v x 
                ev2 <- expr2v x
                pure (ev1 && ev2))

predicateRestrictionFilter attrs (OrPredicate expr1 expr2) =
  runExceptT $ do
    expr1v <- liftE (predicateRestrictionFilter attrs expr1)
    expr2v <- liftE (predicateRestrictionFilter attrs expr2)
    pure (\x -> do
                ev1 <- expr1v x 
                ev2 <- expr2v x
                pure (ev1 || ev2))

predicateRestrictionFilter _ TruePredicate = pure (Right (\_ -> pure True))

predicateRestrictionFilter attrs (NotPredicate expr) = 
  runExceptT $ do
    exprv <- liftE (predicateRestrictionFilter attrs expr)
    pure (fmap not . exprv)

--optimization opportunity: if the subexpression does not reference attributes in the top-level expression, then it need only be evaluated once, statically, outside the tuple filter- see historical implementation here
{-
predicateRestrictionFilter _ (RelationalExprPredicate relExpr) = do
  rstate <- ask
  pure (Right (\tup -> case runReader (evalRelationalExpr relExpr) (mergeTuplesIntoRelationalExprState tup rstate) of
    Left err -> Left err
    Right rel -> if arity rel /= 0 then
                   Left (PredicateExpressionError "Relational restriction filter must evaluate to 'true' or 'false'")
                   else
                   pure (rel == relationTrue)))

predicateRestrictionFilter attrs (AttributeEqualityPredicate attrName atomExpr) = do
  rstate <- ask
  let attrs' = stateAttributes rstate
      ctxtup' = stateTuple rstate
  runExceptT $ do
    atomExprType <- liftE (typeFromAtomExpr attrs' atomExpr)
    attr <- either throwE pure $ case A.attributeForName attrName attrs of
      Right attr -> Right attr
      Left (NoSuchAttributeNamesError _) -> case A.attributeForName attrName (tupleAttributes ctxtup') of
        Right ctxattr -> Right ctxattr
        Left err2@(NoSuchAttributeNamesError _) -> Left err2
        Left err -> Left err
      Left err -> Left err
    if atomExprType /= A.atomType attr then
      throwE (TupleAttributeTypeMismatchError (A.attributesFromList [attr]))
      else
      pure $ \tupleIn -> let evalAndCmp atomIn = case atomEvald of
                               Right atomCmp -> atomCmp == atomIn
                               Left _ -> False
                             atomEvald = runReader (evalAtomExpr tupleIn atomExpr) rstate
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
  rstate <- ask
  runExceptT $ do
    aType <- liftE (typeFromAtomExpr attrs  atomExpr)
    if aType /= BoolAtomType then
      throwE (AtomTypeMismatchError aType BoolAtomType)
      else
      pure (\tupleIn ->
             case runReader (evalAtomExpr tupleIn atomExpr) rstate of
                  Left err -> Left err
                  Right boolAtomValue -> pure (boolAtomValue == BoolAtom True))
-}
tupleExprCheckNewAttrName :: AttributeName -> Relation -> Either RelationalError Relation
tupleExprCheckNewAttrName attrName rel = if isRight (attributeForName attrName rel) then
                                           Left (AttributeNameInUseError attrName)
                                         else
                                           Right rel

extendTupleExpressionProcessor :: Relation -> ExtendTupleExpr -> RelationalExprState (Either RelationalError (Attributes, RelationTuple -> Either RelationalError RelationTuple))
extendTupleExpressionProcessor relIn (AttributeExtendTupleExpr newAttrName atomExpr) = do
  rstate <- askEnv
  -- check that the attribute name is not in use
  case tupleExprCheckNewAttrName newAttrName relIn of
    Left err -> pure (Left err)
    Right _ -> runExceptT $ do
      atomExprType <- liftE (typeFromAtomExpr (attributes relIn) atomExpr)
      atomExprType' <- liftE (verifyAtomExprTypes relIn atomExpr atomExprType)
      let newAttrs = A.attributesFromList [Attribute newAttrName atomExprType']
          newAndOldAttrs = A.addAttributes (attributes relIn) newAttrs
      pure (newAndOldAttrs, \tup -> let substate = mergeTuplesIntoRelationalExprState tup rstate in
               case unimplemented {-runReader (evalAtomExpr tup atomExpr) substate-} of
                 Left err -> Left err
                 Right atom -> Right (tupleAtomExtend newAttrName atom tup)
               )


evalAtomExpr :: RelationTuple -> AtomExpr -> RelationalExprState (Either RelationalError Atom)
evalAtomExpr = undefined
{-

evalAtomExpr tupIn (AttributeAtomExpr attrName) = case atomForAttributeName attrName tupIn of
  Right atom -> pure (Right atom)
  err@(Left (NoSuchAttributeNamesError _)) -> do
    rstate <- ask
    let stateTup = stateTuple rstate
    pure (atomForAttributeName attrName stateTup)
  Left err -> pure (Left err)
evalAtomExpr _ (NakedAtomExpr atom) = pure (Right atom)
evalAtomExpr tupIn (FunctionAtomExpr funcName arguments ()) = do
  argTypes <- mapM (typeFromAtomExpr (tupleAttributes tupIn)) arguments
  context <- fmap stateElemsContext ask
  runExceptT $ do
    let functions = atomFunctions context
    func <- either throwE pure (atomFunctionForName funcName functions)
    let expectedArgCount = length (atomFuncType func) - 1
        actualArgCount = length argTypes
        safeInit [] = [] -- different behavior from normal init
        safeInit xs = init xs
    if expectedArgCount /= actualArgCount then
      throwE (FunctionArgumentCountMismatchError expectedArgCount actualArgCount)
      else do
      let zippedArgs = zip (safeInit (atomFuncType func)) argTypes
      mapM_ (\(expType, eActType) -> do
                actType <- either throwE pure eActType
                either throwE pure (atomTypeVerify expType actType)) zippedArgs
      evaldArgs <- mapM (liftE . evalAtomExpr tupIn) arguments
      case evalAtomFunction func evaldArgs of
        Left err -> throwE (AtomFunctionUserError err)
        Right result -> do
          --validate that the result matches the expected type
          _ <- either throwE pure (atomTypeVerify (last (atomFuncType func)) (atomTypeForAtom result))
          pure result
evalAtomExpr tupIn (RelationAtomExpr relExpr) = do
  --merge existing state tuple context into new state tuple context to support an arbitrary number of levels, but new attributes trounce old attributes
  rstate <- ask
  runExceptT $ do
    let newState = mergeTuplesIntoRelationalExprState tupIn rstate
    relAtom <- either throwE pure (runReader (evalRelationalExpr relExpr) newState)
    pure (RelationAtom relAtom)
evalAtomExpr tupIn cons@(ConstructedAtomExpr dConsName dConsArgs ()) = runExceptT $ do
  rstate <- lift ask
  let newState = mergeTuplesIntoRelationalExprState tupIn rstate
  aType <- either throwE pure (runReader (typeFromAtomExpr (tupleAttributes tupIn) cons) newState)
  argAtoms <- mapM (\arg -> either throwE pure (runReader (evalAtomExpr tupIn arg) newState)) dConsArgs
  pure (ConstructedAtom dConsName aType argAtoms)
-}
typeForGraphRefAtomExpr :: Attributes -> GraphRefAtomExpr -> RelationalExprState (Either RelationalError AtomType)
typeForGraphRefAtomExpr attrs (AttributeAtomExpr attrName) = do
  rstate <- askEnv
  case A.atomTypeForAttributeName attrName attrs of
    Right aType -> pure (Right aType)
    Left err@(NoSuchAttributeNamesError _) ->
      let stateTup = stateTuple rstate
          stateAttrs = stateAttributes rstate in
      case A.attributeForName attrName stateAttrs of
        Right attr -> pure (Right (A.atomType attr))
        Left _ -> case atomForAttributeName attrName stateTup of
          Right atom -> pure (Right (atomTypeForAtom atom))
          Left _ -> pure (Left err)

typeForGraphAtomExpr _ (NakedAtomExpr atom) = pure (Right (atomTypeForAtom atom))
typeForGraphAtomExpr attrs (FunctionAtomExpr funcName atomArgs _) = do
  context <- fmap envContext askEnv
  let funcs = atomFunctions context
  case atomFunctionForName funcName funcs of
    Left err -> pure (Left err)
    Right func -> do
      let funcRetType = last (atomFuncType func)
          funcArgTypes = init (atomFuncType func)
      eArgTypes <- mapM (typeFromAtomExpr attrs) atomArgs
      case lefts eArgTypes of                   
        errs@(_:_) -> pure (Left (someErrors errs))
        [] -> do
          let eTvMap = resolveTypeVariables funcArgTypes argTypes
              argTypes = rights eArgTypes
          case eTvMap of
            Left err -> pure (Left err)
            Right tvMap -> pure (resolveFunctionReturnValue funcName tvMap funcRetType)
typeFromAtomExpr attrs (RelationAtomExpr relExpr) = do
  relType <- R.local (mergeAttributesIntoRelationalExprState attrs) (typeForRelationalExpr relExpr)
  case relType of
    Left err -> pure (Left err)
    Right relType' ->
      pure (pure (RelationAtomType (attributes relType')))
-- grab the type of the data constructor, then validate that the args match the expected types
typeFromAtomExpr attrs (ConstructedAtomExpr dConsName dConsArgs _) = 
  runExceptT $ do
    argsTypes <- mapM (liftE . typeFromAtomExpr attrs) dConsArgs 
    context <- fmap envContext (lift askEnv)
    either throwE pure (atomTypeForDataConstructor (typeConstructorMapping context) dConsName argsTypes)

-- | Validate that the type of the AtomExpr matches the expected type.
verifyAtomExprTypes :: Relation -> AtomExpr -> AtomType -> RelationalExprState (Either RelationalError AtomType)
verifyAtomExprTypes relIn (AttributeAtomExpr attrName) expectedType = runExceptT $ do
  rstate <- lift askEnv
  case A.atomTypeForAttributeName attrName (attributes relIn) of
    Right aType -> either throwE pure (atomTypeVerify expectedType aType)
    (Left err@(NoSuchAttributeNamesError _)) ->
      let attrs' = stateAttributes rstate in
        if attrs' == emptyAttributes then
          throwE err
        else
          case A.attributeForName attrName attrs' of
            Left err' -> throwE err'
            Right attrType -> either throwE pure (atomTypeVerify expectedType (A.atomType attrType))
verifyAtomExprTypes _ (NakedAtomExpr atom) expectedType = pure (atomTypeVerify expectedType (atomTypeForAtom atom))
verifyAtomExprTypes relIn (FunctionAtomExpr funcName funcArgExprs _) expectedType = do
  rstate <- askEnv
  let functions = atomFunctions context
      context = envContext rstate
  runExceptT $ do
    func <- either throwE pure (atomFunctionForName funcName functions)
    let expectedArgTypes = atomFuncType func
    funcArgTypes <- mapM (\(atomExpr,expectedType2,argCount) -> case runReader (verifyAtomExprTypes relIn atomExpr expectedType2) rstate of
                           Left (AtomTypeMismatchError expSubType actSubType) -> throwE (AtomFunctionTypeError funcName argCount expSubType actSubType)
                           Left err -> throwE err
                           Right x -> pure x
                           ) $ zip3 funcArgExprs expectedArgTypes [1..]
    if length funcArgTypes /= length expectedArgTypes - 1 then
      throwE (AtomTypeCountError funcArgTypes expectedArgTypes)
      else 
      either throwE pure (atomTypeVerify expectedType (last expectedArgTypes))
verifyAtomExprTypes relIn (RelationAtomExpr relationExpr) expectedType = runExceptT $ do
  rstate <- lift askEnv
  relType <- either throwE pure (runReader (typeForRelationalExpr relationExpr) (mergeAttributesIntoRelationalExprState (attributes relIn) rstate))
  either throwE pure (atomTypeVerify expectedType (RelationAtomType (attributes relType)))
verifyAtomExprTypes rel cons@ConstructedAtomExpr{} expectedType = runExceptT $ do
  cType <- liftE (typeFromAtomExpr (attributes rel) cons)
  either throwE pure (atomTypeVerify expectedType cType)

-- | Look up the type's name and create a new attribute.
evalGraphRefAttrExpr :: GraphRefAttributeExpr -> RelationalExprState (Either RelationalError Attribute)
evalGraphRefAttrExpr (AttributeAndTypeNameExpr attrName tCons transId) = do
  graph <- re_graph <$> askEnv
  case transactionForId transId graph of
    Left err -> pure (Left err)
    Right trans -> do
      let tConsMap = typeConstructorMapping (concreteDatabaseContext trans)
          validate = do
            aType <- atomTypeForTypeConstructorValidate True tCons tConsMap M.empty
            validateAtomType aType tConsMap
            pure aType
      case validate of
        Left err -> pure (Left err)
        Right aType ->
          pure $ Right (Attribute attrName aType)
  
evalGraphRefAttrExpr (NakedAttributeExpr attr) = pure $ Right attr
  
evalTupleExpr :: Maybe Attributes -> TupleExpr -> RelationalExprState (Either RelationalError RelationTuple)
evalTupleExpr mAttrs (TupleExpr tupMap) = do
  context <- fmap envContext askEnv
  runExceptT $ do
  -- it's not possible for AtomExprs in tuple constructors to reference other Attributes' atoms due to the necessary order-of-operations (need a tuple to pass to evalAtomExpr)- it may be possible with some refactoring of type usage or delayed evaluation- needs more thought, but not a priority
  -- I could adjust this logic so that when the attributes are not specified (Nothing), then I can attempt to extract the attributes from the tuple- the type resolution will blow up if an ambiguous data constructor is used (Left 4) and this should allow simple cases to "relation{tuple{a 4}}" to be processed
    let attrs = fromMaybe A.emptyAttributes mAttrs
    attrAtoms <- mapM (\(attrName, aExpr) -> do
                          --provided when the relation header is available
                          let eExpectedAtomType = A.atomTypeForAttributeName attrName attrs
                          unresolvedType <- liftE (typeFromAtomExpr attrs aExpr)
                          resolvedType <- case eExpectedAtomType of
                            Left _ -> pure unresolvedType
                            Right typeHint -> either throwE pure (resolveAtomType typeHint unresolvedType)
                          --resolve atom typevars based on resolvedType?
                          newAtom <- liftE (evalAtomExpr emptyTuple aExpr)
                          pure (attrName, newAtom, resolvedType)
                      ) (M.toList tupMap)
    let tupAttrs = A.attributesFromList $ map (\(attrName, _, aType) -> Attribute attrName aType) attrAtoms
        atoms = V.fromList $ map (\(_, atom, _) -> atom) attrAtoms
        tup = mkRelationTuple tupAttrs atoms
        tConss = typeConstructorMapping context
        finalAttrs = fromMaybe tupAttrs mAttrs
    --verify that the attributes match
    when (A.attributeNameSet finalAttrs /= A.attributeNameSet tupAttrs) $ throwE (TupleAttributeTypeMismatchError tupAttrs)
    tup' <- either throwE pure (resolveTypesInTuple finalAttrs tConss (reorderTuple finalAttrs tup))
    _ <- either throwE pure (validateTuple tup' tConss)
    pure tup'

evalAttributeNames :: AttributeNames -> RelationalExpr -> RelationalExprState (Either RelationalError (S.Set AttributeName))
evalAttributeNames attrNames expr = do
  eExprType <- typeForRelationalExpr expr
  case eExprType of
    Left err -> pure (Left err)
    Right exprTyp -> do
      let typeNameSet = S.fromList (V.toList (A.attributeNames (attributes exprTyp)))
      case attrNames of
        AttributeNames names ->
          case A.projectionAttributesForNames names (attributes exprTyp) of
            Left err -> pure (Left err)
            Right attrs -> pure (Right (S.fromList (V.toList (A.attributeNames attrs))))
          
        InvertedAttributeNames names -> do
          let nonExistentAttributeNames = A.attributeNamesNotContained names typeNameSet
          if not (S.null nonExistentAttributeNames) then
            pure (Left (AttributeNamesMismatchError nonExistentAttributeNames))
            else
            pure (Right (A.nonMatchingAttributeNameSet names typeNameSet))
        
        UnionAttributeNames namesA namesB -> do
          eNameSetA <- evalAttributeNames namesA expr
          case eNameSetA of
            Left err -> pure (Left err)
            Right nameSetA -> do
              eNameSetB <- evalAttributeNames namesB expr
              case eNameSetB of
                Left err -> pure (Left err)
                Right nameSetB ->           
                  pure (Right (S.union nameSetA nameSetB))
        
        IntersectAttributeNames namesA namesB -> do
          eNameSetA <- evalAttributeNames namesA expr
          case eNameSetA of
            Left err -> pure (Left err)
            Right nameSetA -> do
              eNameSetB <- evalAttributeNames namesB expr
              case eNameSetB of
                Left err -> pure (Left err)
                Right nameSetB ->           
                  pure (Right (S.intersection nameSetA nameSetB))
        
        RelationalExprAttributeNames attrExpr -> do
          eAttrExprType <- typeForRelationalExpr attrExpr
          case eAttrExprType of
            Left err -> pure (Left err)
            Right attrExprType -> pure (Right (A.attributeNameSet (attributes attrExprType)))

evalGraphRefRelationalExpr :: GraphRefRelationalExpr -> TransactionGraph -> Either RelationalError Relation
--evalGraphRefRelationalExpr (MakeRelationaFromExprs
evalGraphRefRelationalExpr (RelationVariable name tid) graph = do
  ctx <- dbContextForTransId tid graph
  case M.lookup name (relationVariables ctx) of
    Nothing -> Left (RelVarNotDefinedError name)
    Just rv -> evalGraphRefRelationalExpr rv graph


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

processRestrictionPredicateExpr :: RestrictionPredicateExpr -> RelationalExprState GraphRefRestrictionPredicateExpr
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

processExtendTupleExpr :: ExtendTupleExpr -> RelationalExprState GraphRefExtendTupleExpr
processExtendTupleExpr (AttributeExtendTupleExpr nam atomExpr) =
  AttributeExtendTupleExpr nam <$> processAtomExpr atomExpr

typeForGraphRefRelationalExpr :: GraphRefRelationalExpr -> GraphRefRelationalExprR (Either RelationalError Relation)
typeForGraphRefRelationalExpr (MakeStaticRelation attrs _) = pure $ mkRelation attrs emptyTupleSet
typeForGraphRefRelationalExpr (ExistingRelation rel) = pure (Right (emptyRelationWithAttrs (attributes rel)))
typeForGraphRefRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) =
  case mAttrExprs of
    Just attrExprs -> do
      eAttrs <- mapM (\e -> evalGraphRefAttributeExpr e) attrExprs
      case lefts eAttrs of
        [] -> pure (Right (emptyRelationWithAttrs (attributesFromList (rights eAttrs))))
        _ -> pure (Left (someErrors (lefts eAttrs)))
typeForGraphRefRelationalExpr (RelationVariable rvName tid) = do
  graph <- R.ask
  let eTrans = transactionForId tid graph
  case eTrans of
    Left err -> pure (Left err)
    Right trans -> do
      let relVars = relationVariables (concreteDatabaseContext trans)
      case M.lookup rvName relVars of
        Nothing -> pure (Left (RelVarNotDefinedError rvName))
        Just rvExpr -> typeForGraphRefRelationalExpr rvExpr
typeForGraphRefRelationalExpr (Project attrNames expr) = do
  eExprType <- typeForGraphRefRelationalExpr expr
  case eExprType of
    Left err -> pure (Left err)
    Right exprType -> do
      eProjectionAttrs <- evalGraphRefAttributeNames attrNames expr
      pure $ case eProjectionAttrs of
               Left err -> Left err
               Right projectionAttrs -> project projectionAttrs exprType

evalGraphRefAttributeNames :: GraphRefAttributeNames -> GraphRefRelationalExpr -> GraphRefRelationalExprR (Either RelationalError (S.Set AttributeName))
evalGraphRefAttributeNames attrNames expr = do
  eExprType <- typeForGraphRefRelationalExpr expr
  case eExprType of
    Left err -> pure (Left err)
    Right exprTyp -> do
      let typeNameSet = S.fromList (V.toList (A.attributeNames (attributes exprTyp)))
      case attrNames of
        AttributeNames names ->
          case A.projectionAttributesForNames names (attributes exprTyp) of
            Left err -> pure (Left err)
            Right attrs -> pure (Right (S.fromList (V.toList (A.attributeNames attrs))))
          
        InvertedAttributeNames names -> do
          let nonExistentAttributeNames = A.attributeNamesNotContained names typeNameSet
          if not (S.null nonExistentAttributeNames) then
            pure (Left (AttributeNamesMismatchError nonExistentAttributeNames))
            else
            pure (pure (A.nonMatchingAttributeNameSet names typeNameSet))
        
        UnionAttributeNames namesA namesB -> do
          eNameSetA <- evalGraphRefAttributeNames namesA expr
          case eNameSetA of
            Left err -> pure (Left err)
            Right nameSetA -> do
              eNameSetB <- evalGraphRefAttributeNames namesB expr
              case eNameSetB of
                Left err -> pure (Left err)
                Right nameSetB ->           
                  pure (Right (S.union nameSetA nameSetB))
        
        IntersectAttributeNames namesA namesB -> do
          eNameSetA <- evalGraphRefAttributeNames namesA expr
          case eNameSetA of
            Left err -> pure (Left err)
            Right nameSetA -> do
              eNameSetB <- evalGraphRefAttributeNames namesB expr
              case eNameSetB of
                Left err -> pure (Left err)
                Right nameSetB ->           
                  pure (Right (S.intersection nameSetA nameSetB))
        
        RelationalExprAttributeNames attrExpr -> do
          eAttrExprType <- typeForGraphRefRelationalExpr attrExpr
          case eAttrExprType of
            Left err -> pure (Left err)
            Right attrExprType -> pure (Right (A.attributeNameSet (attributes attrExprType)))
        

  

processAtomExpr :: AtomExpr -> RelationalExprState GraphRefAtomExpr
processAtomExpr (AttributeAtomExpr nam) = pure $ AttributeAtomExpr nam
processAtomExpr (NakedAtomExpr atom) = pure $ NakedAtomExpr atom
processAtomExpr (FunctionAtomExpr fName atomExprs ()) = do
  tid <- re_currentTransId <$> askEnv
  FunctionAtomExpr fName <$> mapM processAtomExpr atomExprs  <*> pure tid
processAtomExpr (RelationAtomExpr expr) = RelationAtomExpr <$> processRelationalExpr expr
processAtomExpr (ConstructedAtomExpr dConsName atomExprs ()) = ConstructedAtomExpr dConsName <$> mapM processAtomExpr atomExprs <*> (re_currentTransId <$> askEnv)

processTupleExpr :: TupleExpr -> RelationalExprState GraphRefTupleExpr
processTupleExpr (TupleExpr tMap) = 
  TupleExpr . M.fromList <$> mapM (\(k,v) -> (,) k <$> processAtomExpr v) (M.toList tMap)

evalGraphRefAttributeExpr :: GraphRefAttributeExpr -> GraphRefRelationalExprR (Either RelationalError Attribute)
evalGraphRefAttributeExpr (AttributeAndTypeNameExpr attrName tCons tid) = do
  graph <- R.ask
  case transactionForId tid graph of
    Left err -> pure (Left err)
    Right trans -> do
      let tConsMap = typeConstructorMapping (concreteDatabaseContext trans)
      case atomTypeForTypeConstructorValidate True tCons tConsMap M.empty of
        Left err -> pure (Left err)
        Right aType -> do
          case validateAtomType aType tConsMap of
            Left err -> pure (Left err)
            Right _ -> pure (Right (Attribute attrName aType))

--convert AttributeExpr to GraphRefAttributeExpr
processAttributeExpr :: AttributeExpr -> RelationalExprState GraphRefAttributeExpr
processAttributeExpr (AttributeAndTypeNameExpr nam tCons ()) = do
  tid <- re_currentTransId <$> askEnv
  pure $ AttributeAndTypeNameExpr nam tCons tid
processAttributeExpr (NakedAttributeExpr attr) = pure $ NakedAttributeExpr attr

-- convert a RelationalExpr into a GraphRefRelationalExpr using the current trans Id
-- can this be done without any RelationalErrors as opposed to evalRelationalExpr?
processRelationalExpr :: RelationalExpr -> RelationalExprState GraphRefRelationalExpr
processRelationalExpr (MakeRelationFromExprs mAttrs tupleExprs) = do
  mAttrs' <- case mAttrs of
                  Nothing -> pure Nothing
                  Just mAttrs'' -> Just <$> mapM processAttributeExpr mAttrs''
  MakeRelationFromExprs mAttrs' <$> mapM processTupleExpr tupleExprs
processRelationalExpr (MakeStaticRelation attrs tupSet) = pure (MakeStaticRelation attrs tupSet)
processRelationalExpr (ExistingRelation rel) = pure (ExistingRelation rel)
processRelationalExpr (RelationVariable rv ()) = RelationVariable rv <$> fmap re_currentTransId askEnv
processRelationalExpr (Project attrNames expr) = Project <$> processAttributeNames attrNames <*> processRelationalExpr expr
processRelationalExpr (Union exprA exprB) = Union <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Join exprA exprB) = Join <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Rename attrA attrB expr) =
  Rename attrA attrB <$> processRelationalExpr expr
processRelationalExpr (Difference exprA exprB) = Difference <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Group attrNames attrName expr) = Group <$> processAttributeNames attrNames <*> pure attrName <*> processRelationalExpr expr
processRelationalExpr (Ungroup attrName expr) = Ungroup attrName <$> processRelationalExpr expr
processRelationalExpr (Restrict pred expr) = Restrict <$> processRestrictionPredicateExpr pred <*> processRelationalExpr expr
processRelationalExpr (Equals exprA exprB) =
  Equals <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (NotEquals exprA exprB) =   
  NotEquals <$> processRelationalExpr exprA <*> processRelationalExpr exprB
processRelationalExpr (Extend extendExpr expr) =
  Extend <$> processExtendTupleExpr extendExpr <*> processRelationalExpr expr
processRelationalExpr (With macros expr) =
  With <$> mapM (\(nam, wexpr) -> (,) nam <$> processRelationalExpr wexpr) macros <*> processRelationalExpr expr

mkEmptyRelVars :: RelationVariables -> RelationVariables
mkEmptyRelVars rvMap = M.map mkEmptyRelVar rvMap
  where
    mkEmptyRelVar (MakeRelationFromExprs mAttrs _) = MakeRelationFromExprs mAttrs []
    mkEmptyRelVar (MakeStaticRelation attrs _) = MakeStaticRelation attrs emptyTupleSet
    mkEmptyRelVar (ExistingRelation rel) = ExistingRelation (emptyRelationWithAttrs (attributes rel))
    mkEmptyRelVar rv@RelationVariable{} = Restrict (NotPredicate TruePredicate) rv
    mkEmptyRelVar (Project attrNames expr) = Project attrNames (mkEmptyRelVar expr)
    mkEmptyRelVar (Union exprA exprB) = Union (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (Join exprA exprB) = Join (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (Rename nameA nameB expr) = Rename nameA nameB (mkEmptyRelVar expr)
    mkEmptyRelVar (Difference exprA exprB) = Difference (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (Group attrNames attrName expr) = Group attrNames attrName (mkEmptyRelVar expr)
    mkEmptyRelVar (Ungroup attrName expr) = Ungroup attrName (mkEmptyRelVar expr)
    mkEmptyRelVar (Restrict pred expr) = Restrict pred (mkEmptyRelVar expr)
    mkEmptyRelVar (Equals exprA exprB) = Equals (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (NotEquals exprA exprB) = NotEquals (mkEmptyRelVar exprA) (mkEmptyRelVar exprB)
    mkEmptyRelVar (Extend extTuple expr) = Extend extTuple (mkEmptyRelVar expr)
    mkEmptyRelVar (With macros expr) = With (map (\(rv, expr) -> (rv, mkEmptyRelVar expr)) macros) (mkEmptyRelVar expr)

processAttributeNames :: AttributeNames -> RelationalExprState GraphRefAttributeNames
processAttributeNames (AttributeNames nameSet) = pure $ AttributeNames nameSet
processAttributeNames (UnionAttributeNames attrNamesA attrNamesB) = UnionAttributeNames <$> processAttributeNames attrNamesA <*> processAttributeNames attrNamesB
processAttributeNames (IntersectAttributeNames attrNamesA attrNamesB) = IntersectAttributeNames <$> processAttributeNames attrNamesA <*> processAttributeNames attrNamesB
processAttributeNames (RelationalExprAttributeNames expr) = RelationalExprAttributeNames <$> processRelationalExpr expr

processDatabaseContextExpr :: DatabaseContextExpr -> RelationalExprState GraphRefDatabaseContextExpr
processDatabaseContextExpr expr =
  case expr of
    NoOperation -> pure NoOperation
    Define nam attrExprs -> Define nam <$> mapM processAttributeExpr attrExprs
    Undefine nam -> pure (Undefine nam)
    Assign nam rexpr -> Assign nam <$> processRelationalExpr rexpr
    Insert nam rexpr -> Insert nam <$> processRelationalExpr rexpr
    Delete nam pred -> Delete nam <$> processRestrictionPredicateExpr pred
    Update nam attrMap pred -> Update nam attrMap <$> processRestrictionPredicateExpr pred

    AddInclusionDependency nam dep -> pure (AddInclusionDependency nam dep)
    RemoveInclusionDependency nam -> pure (RemoveInclusionDependency nam)
    AddNotification nam exprA exprB exprC -> pure (AddNotification nam exprA exprB exprC)
    RemoveNotification nam -> pure (RemoveNotification nam)
    AddTypeConstructor tyDef consDefs -> pure (AddTypeConstructor tyDef consDefs)
    RemoveTypeConstructor tyName -> pure (RemoveTypeConstructor tyName)

    RemoveAtomFunction aFuncName -> pure (RemoveAtomFunction aFuncName)
    RemoveDatabaseContextFunction funcName -> pure (RemoveDatabaseContextFunction funcName)
    ExecuteDatabaseContextFunction funcName atomExprs -> ExecuteDatabaseContextFunction funcName <$> mapM processAtomExpr atomExprs
    MultipleExpr exprs -> MultipleExpr <$> mapM processDatabaseContextExpr exprs

dbErr :: RelationalError -> DatabaseContextEvalMonad ()
dbErr err = lift (except (Left err))
      
processDatabaseContextIOExpr :: DatabaseContextIOExpr -> RelationalExprState GraphRefDatabaseContextIOExpr
processDatabaseContextIOExpr (AddAtomFunction f tcs sc) =
  pure (AddAtomFunction f tcs sc)
processDatabaseContextIOExpr (LoadAtomFunctions mod fun file) =
  pure (LoadAtomFunctions mod fun file)
processDatabaseContextIOExpr (AddDatabaseContextFunction mod fun path) =
  pure (AddDatabaseContextFunction mod fun path)
processDatabaseContextIOExpr (LoadDatabaseContextFunctions mod fun path) =
  pure (LoadDatabaseContextFunctions mod fun path)
processDatabaseContextIOExpr (CreateArbitraryRelation rvName attrExprs range) =
  CreateArbitraryRelation rvName <$> mapM processAttributeExpr attrExprs <*> pure range
  
-- | Return a Relation describing the relation variables.
relationVariablesAsRelation :: RelationVariables -> TransactionGraph -> Either RelationalError Relation
relationVariablesAsRelation relVarMap graph = do
  let subrelAttrs = A.attributesFromList [Attribute "attribute" TextAtomType, Attribute "type" TextAtomType]
      attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                  Attribute "attributes" (RelationAtomType subrelAttrs)]
      mkRvDesc (rvName, gfExpr) = do
        gfType <- runReader (typeForGraphRefRelationalExpr gfExpr) graph
        pure (rvName, gfType)
      relVarToAtomList (rvName, rel) = [TextAtom rvName, attributesToRel (attributes rel)]
      attrAtoms a = [TextAtom (A.attributeName a), TextAtom (prettyAtomType (A.atomType a))]
      attributesToRel attrl = case mkRelationFromList subrelAttrs (map attrAtoms (V.toList attrl)) of
        Left err -> error ("relationVariablesAsRelation pooped " ++ show err)
        Right rel -> RelationAtom rel
  rvs <- mapM mkRvDesc (M.toList relVarMap)
  let tups = map relVarToAtomList rvs
  mkRelationFromList attrs tups

