{-# LANGUAGE ScopedTypeVariables #-}
module ProjectM36.RelationalExpression where
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Base
import qualified Data.UUID as U
import ProjectM36.Error
import ProjectM36.AtomType
import ProjectM36.Attribute (emptyAttributes)
import ProjectM36.ScriptSession
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunction
import ProjectM36.DatabaseContextFunction
import ProjectM36.Arbitrary
import qualified ProjectM36.Attribute as A
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.Set as S
import Control.Monad.State hiding (join)
import Control.Exception
import Data.Maybe
import Data.Either
import Data.Char (isUpper)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified ProjectM36.TypeConstructorDef as TCD
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Test.QuickCheck
import GHC
import GHC.Paths

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
freshDatabaseState :: DatabaseContext -> DatabaseStateElems
freshDatabaseState context = DatabaseStateElems {
  context = context,
  accum = M.empty,
  dirty = False
  } --future work: propagate return accumulator

-- we need to pass around a higher level RelationTuple and Attributes in order to solve #52
data RelationalExprStateElems = RelationalExprStateTupleElems DatabaseContext RelationTuple | -- used when fully evaluating a relexpr
                                RelationalExprStateAttrsElems DatabaseContext Attributes | --used when evaluating the type of a relexpr
                                RelationalExprStateElems DatabaseContext --used by default at the top level of evaluation
                                
instance Show RelationalExprStateElems where                                
  show (RelationalExprStateTupleElems _ tup) = "RelationalExprStateTupleElems " ++ show tup
  show (RelationalExprStateAttrsElems _ attrs) = "RelationalExprStateAttrsElems" ++ show attrs
  show (RelationalExprStateElems _) = "RelationalExprStateElems"
                                
mkRelationalExprState :: DatabaseContext -> RelationalExprStateElems
mkRelationalExprState = RelationalExprStateElems

mergeTuplesIntoRelationalExprState :: RelationTuple -> RelationalExprStateElems -> RelationalExprStateElems
mergeTuplesIntoRelationalExprState tupIn (RelationalExprStateElems ctx) = RelationalExprStateTupleElems ctx tupIn
mergeTuplesIntoRelationalExprState _ st@(RelationalExprStateAttrsElems _ _) = st
mergeTuplesIntoRelationalExprState tupIn (RelationalExprStateTupleElems ctx existingTuple) = let mergedTupMap = M.union (tupleToMap tupIn) (tupleToMap existingTuple) in
  RelationalExprStateTupleElems ctx (mkRelationTupleFromMap mergedTupMap)
  
mergeAttributesIntoRelationalExprState :: Attributes -> RelationalExprStateElems -> RelationalExprStateElems
mergeAttributesIntoRelationalExprState attrs (RelationalExprStateElems ctx) = RelationalExprStateAttrsElems ctx attrs
mergeAttributesIntoRelationalExprState _ st@(RelationalExprStateTupleElems _ _) = st
mergeAttributesIntoRelationalExprState attrsIn (RelationalExprStateAttrsElems ctx attrs) = RelationalExprStateAttrsElems ctx (A.union attrsIn attrs)

type ResultAccumName = StringType

type ResultAccumFunc = (RelationTuple -> Relation -> Relation) -> Relation -> Relation

data ResultAccum = ResultAccum { resultAccumFunc :: ResultAccumFunc,
                                 resultAccumResult :: Relation
                                 }

data DatabaseStateElems = DatabaseStateElems {
  context :: DatabaseContext, --new, alterable context for a new transaction
  accum :: M.Map ResultAccumName ResultAccum,
  dirty :: DirtyFlag
  }

type DatabaseState a = State DatabaseStateElems a

getStateContext :: DatabaseState DatabaseContext
getStateContext = context <$> get

putStateContext :: DatabaseContext -> DatabaseState () 
putStateContext ctx' = do
  s <- get
  put (s {context = ctx'}) 
  
type RelationalExprState a = Reader RelationalExprStateElems a

stateElemsContext :: RelationalExprStateElems -> DatabaseContext
stateElemsContext (RelationalExprStateTupleElems ctx _) = ctx
stateElemsContext (RelationalExprStateElems ctx) = ctx
stateElemsContext (RelationalExprStateAttrsElems ctx _) = ctx

setStateElemsContext :: RelationalExprStateElems -> DatabaseContext -> RelationalExprStateElems
setStateElemsContext (RelationalExprStateTupleElems _ tup) ctx = RelationalExprStateTupleElems ctx tup
setStateElemsContext (RelationalExprStateElems _) ctx = RelationalExprStateElems ctx
setStateElemsContext (RelationalExprStateAttrsElems _ attrs) ctx = RelationalExprStateAttrsElems ctx attrs

--relvar state is needed in evaluation of relational expression but only as read-only in order to extract current relvar values
evalRelationalExpr :: RelationalExpr -> RelationalExprState (Either RelationalError GraphRefRelationalExpr)
evalRelationalExpr (RelationVariable name _) = do
  relvarTable <- fmap (relationVariables . stateElemsContext) ask
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
  currentContext <- fmap stateElemsContext ask
  let tConss = typeConstructorMapping currentContext
  -- if the mAttrExprs is Nothing, then we should attempt to infer the tuple attributes from the first tuple itself- note that this is not always possible
  runExceptT $ do
    mAttrs <- case mAttrExprs of
      Just _ ->
        Just . A.attributesFromList <$> mapM (either throwE pure . evalAttrExpr tConss) (fromMaybe [] mAttrExprs)
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
  ePred <- processRestrictionPredicateExpr predicateExpr
  case evald of
    Left err -> return $ Left err
    Right expr -> case ePred of
      Left err -> pure (Left err)
      Right pred -> pure $ Right (Restrict pred expr)

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
         let evalMainExpr expr = runReader (evalRelationalExpr expr) (RelationalExprStateElems ctx'')
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
      eTupProc <- processExtendTupleExpr tupleExpression
      case eTupProc of
        Left err -> pure (Left err)
        Right tupProc -> pure (Right (Extend tupProc expr))

--helper function to process relation variable creation/assignment
setRelVar :: RelVarName -> GraphRefRelationalExpr -> DatabaseState (Either RelationalError ())
setRelVar relVarName relExpr = do
  currentContext <- getStateContext
  let newRelVars = M.insert relVarName relExpr $ relationVariables currentContext
      potentialContext = currentContext { relationVariables = newRelVars }
  --determine when to check constraints
  
  case checkConstraints potentialContext of
    Left err -> pure (Left err)
    Right _ -> do
      putStateContext potentialContext
      return (Right ())

-- it is not an error to delete a relvar which does not exist, just like it is not an error to insert a pre-existing tuple into a relation
deleteRelVar :: RelVarName -> DatabaseState (Either RelationalError ())
deleteRelVar relVarName = do
  currContext <- getStateContext
  let relVars = relationVariables currContext
  if M.notMember relVarName relVars then
    pure (Right ())
    else do
    let newRelVars = M.delete relVarName relVars
        newContext = currContext { relationVariables = newRelVars }
    putStateContext newContext
    pure (Right ())

evalDatabaseContextExpr :: DatabaseContextExpr -> TransactionId -> TransactionGraph -> DatabaseState (Either RelationalError ())
evalDatabaseContextExpr NoOperation _ _ = pure (Right ())
  
evalDatabaseContextExpr (Define relVarName attrExprs) _ _ = do
  relvars <- fmap relationVariables getStateContext
  tConss <- fmap typeConstructorMapping getStateContext
  let eAttrs = map (evalAttrExpr tConss) attrExprs
      attrErrs = lefts eAttrs
      attrsList = rights eAttrs
  if not (null  attrErrs) then
    pure (Left (someErrors attrErrs))
    else do
      let atomTypeErrs = lefts $ map ((`validateAtomType` tConss) . A.atomType) attrsList
      if not (null atomTypeErrs) then
        pure (Left (someErrors atomTypeErrs))
        else 
        case M.member relVarName relvars of
          True -> pure (Left (RelVarAlreadyDefinedError relVarName))
          False -> setRelVar relVarName (ExistingRelation emptyRelation) >> pure (Right ())
            where
              attrs = A.attributesFromList attrsList
              emptyRelation = Relation attrs emptyTupleSet

evalDatabaseContextExpr (Undefine relVarName) _ _ = deleteRelVar relVarName

evalDatabaseContextExpr (Assign relVarName expr) transId graph = do
  context <- getStateContext
  let existingRelVar = M.lookup relVarName (relationVariables context)
      relExprState = mkRelationalExprState context
  case existingRelVar of
    Nothing -> do
      let ervExpr = runReader (evalRelationalExpr expr) relExprState
      case ervExpr of
        Left err -> pure (Left err)
        Right rvExpr -> setRelVar relVarName rvExpr
    Just existingRel -> do
      let eExpectedType = typeForGraphRefRelationalExpr existingRel transId graph
          eNewExprType = runReader (typeForRelationalExpr expr) relExprState
      case eExpectedType of
        Left err -> pure (Left err)
        Right expectedType ->
          case eNewExprType of
            Left err -> pure (Left err)
            Right newExprType -> do
              if newExprType == expectedType then do
                let ervExpr = runReader (evalRelationalExpr expr) relExprState
                case ervExpr of
                  Left err -> pure (Left err)
                  Right rvExpr -> setRelVar relVarName rvExpr
              else
                pure (Left (RelationTypeMismatchError (attributes expectedType) (attributes newExprType)))

evalDatabaseContextExpr (Insert relVarName relExpr) tid graph = 
  evalDatabaseContextExpr (Assign relVarName (Union (RelationVariable relVarName ())
                                             relExpr)) tid graph

evalDatabaseContextExpr (Delete relVarName predicate) transId _ = do
  ctx <- getStateContext
  let rv = RelationVariable relVarName transId
      rvState = mkRelationalExprState ctx
  case runReader (processRestrictionPredicateExpr predicate) rvState of
    Left err -> pure (Left err)
    Right graphRefPredicate ->    
      setRelVar relVarName (Restrict (NotPredicate graphRefPredicate) rv)
  
--union of restricted+updated portion and the unrestricted+unupdated portion
evalDatabaseContextExpr (Update relVarName atomExprMap restrictionPredicateExpr) transId _ = do
  context <- getStateContext
  let rvState = mkRelationalExprState context
      relVarTable = relationVariables context 
  case runReader (processRestrictionPredicateExpr restrictionPredicateExpr) rvState of
    Left err -> pure (Left err)
    Right graphRefPredicate ->    
      case M.lookup relVarName relVarTable of
        Nothing -> pure (Left (RelVarNotDefinedError relVarName))
        Just existingRvExpr -> do
          let unrestrictedPortion = Restrict (NotPredicate graphRefPredicate) rv
              rv = RelationVariable relVarName transId
              tmpAttr attr = "_tmp_" <> attr --this could certainly be improved to verify that there is no attribute name conflict
              updateAttr nam atomExpr = Extend (AttributeExtendTupleExpr (tmpAttr nam) atomExpr)
              projectAndRename attr expr = Rename (tmpAttr attr) attr (Project (InvertedAttributeNames (S.singleton attr)) expr)
              restrictedPortion = Restrict graphRefPredicate 
              updated = foldr (\(oldname, atomExpr) accum ->
                                 let procAtomExpr = processAtomExpr atomExpr transId in
                                  updateAttr oldname procAtomExpr accum
                              ) rv (M.toList atomExprMap)
              -- the atomExprMap could reference other attributes, so we must perform multi-pass folds
              updatedPortion = foldr projectAndRename updated (M.keys atomExprMap)
          setRelVar relVarName (Union unrestrictedPortion updatedPortion)

evalDatabaseContextExpr (AddInclusionDependency newDepName newDep) _ _ = do
  currContext <- getStateContext
  let currDeps = inclusionDependencies currContext
      newDeps = M.insert newDepName newDep currDeps
  if M.member newDepName currDeps then
    pure (Left (InclusionDependencyNameInUseError newDepName))
    else do
      let potentialContext = currContext { inclusionDependencies = newDeps }
      case checkConstraints potentialContext of
        Left err -> pure (Left err)
        Right _ -> do
          putStateContext potentialContext
          return (Right ())

evalDatabaseContextExpr (RemoveInclusionDependency depName) _ _ = do
  currContext <- getStateContext
  let currDeps = inclusionDependencies currContext
      newDeps = M.delete depName currDeps
  if M.notMember depName currDeps then
    pure (Left (InclusionDependencyNameNotInUseError depName))
    else do
    putStateContext $ currContext {inclusionDependencies = newDeps }
    return (Right ())
    
-- | Add a notification which will send the resultExpr when triggerExpr changes between commits.
evalDatabaseContextExpr (AddNotification notName triggerExpr resultOldExpr resultNewExpr) _ _ = do
  currentContext <- getStateContext
  let nots = notifications currentContext
  if M.member notName nots then
    pure (Left (NotificationNameInUseError notName))
    else do
      let newNotifications = M.insert notName newNotification nots
          newNotification = Notification { changeExpr = triggerExpr,
                                           reportOldExpr = resultOldExpr, 
                                           reportNewExpr = resultNewExpr}
      putStateContext $ currentContext { notifications = newNotifications }
      return (Right ())
  
evalDatabaseContextExpr (RemoveNotification notName) _ _ = do
  currentContext <- getStateContext
  let nots = notifications currentContext
  if M.notMember notName nots then
    pure (Left (NotificationNameNotInUseError notName))
    else do
    let newNotifications = M.delete notName nots
    putStateContext $ currentContext { notifications = newNotifications }
    pure (Right ())

-- | Adds type and data constructors to the database context.
-- validate that the type *and* constructor names are unique! not yet implemented!
evalDatabaseContextExpr (AddTypeConstructor tConsDef dConsDefList) _ _ = do
  currentContext <- getStateContext
  let oldTypes = typeConstructorMapping currentContext
      tConsName = TCD.name tConsDef
  -- validate that the constructor's types exist
  case validateTypeConstructorDef tConsDef dConsDefList of
    errs@(_:_) -> pure (Left (someErrors errs))
    [] | T.null tConsName || not (isUpper (T.head tConsName)) -> pure (Left (InvalidAtomTypeName tConsName))
       | isJust (findTypeConstructor tConsName oldTypes) -> pure (Left (AtomTypeNameInUseError tConsName))
       | otherwise -> do
      let newTypes = oldTypes ++ [(tConsDef, dConsDefList)]
      putStateContext $ currentContext { typeConstructorMapping = newTypes }
      pure (Right ())

-- | Removing the atom constructor prevents new atoms of the type from being created. Existing atoms of the type remain. Thus, the atomTypes list in the DatabaseContext need not be all-inclusive.
evalDatabaseContextExpr (RemoveTypeConstructor tConsName) _ _ = do
  currentContext <- getStateContext
  let oldTypes = typeConstructorMapping currentContext
  if isNothing (findTypeConstructor tConsName oldTypes) then
    pure (Left (AtomTypeNameNotInUseError tConsName))
    else do
      let newTypes = filter (\(tCons, _) -> TCD.name tCons /= tConsName) oldTypes
      putStateContext $ currentContext { typeConstructorMapping = newTypes }
      pure (Right ())

evalDatabaseContextExpr (MultipleExpr exprs) transId graph = do
  --the multiple expressions must pass the same context around- not the old unmodified context
  evald <- forM exprs (\expr -> evalDatabaseContextExpr expr transId graph)
  --some lifting magic needed here
  case lefts evald of
    [] -> pure (Right ())
    errs -> pure (Left (someErrors errs))
             
evalDatabaseContextExpr (RemoveAtomFunction funcName) _ _ = do
  currentContext <- getStateContext
  let atomFuncs = atomFunctions currentContext
  case atomFunctionForName funcName atomFuncs of
    Left err -> pure (Left err)
    Right realFunc -> if isScriptedAtomFunction realFunc then do
      let updatedFuncs = HS.delete realFunc atomFuncs
      putStateContext (currentContext {atomFunctions = updatedFuncs })
      pure (Right ())
                      else
                        pure (Left (PrecompiledFunctionRemoveError funcName))
      
evalDatabaseContextExpr (RemoveDatabaseContextFunction funcName) _ _ = do      
  context <- getStateContext
  let dbcFuncs = dbcFunctions context
  case databaseContextFunctionForName funcName dbcFuncs of
    Left err -> pure (Left err)
    Right realFunc -> if isScriptedDatabaseContextFunction realFunc then do
      let updatedFuncs = HS.delete realFunc dbcFuncs
      putStateContext (context { dbcFunctions = updatedFuncs })
      pure (Right ())
                      else
                        pure (Left (PrecompiledFunctionRemoveError funcName))
      
evalDatabaseContextExpr (ExecuteDatabaseContextFunction funcName atomArgExprs) _ _ = do
  context <- getStateContext
  --resolve atom arguments
  let relExprState = mkRelationalExprState context
      eAtomTypes = map (\atomExpr -> runReader (typeFromAtomExpr emptyAttributes atomExpr) relExprState) atomArgExprs
      eFunc = databaseContextFunctionForName funcName (dbcFunctions context)
  case eFunc of
      Left err -> pure (Left err)
      Right func -> do
        let expectedArgCount = length (dbcFuncType func)
            actualArgCount = length atomArgExprs
        if expectedArgCount /= actualArgCount then
          pure (Left (FunctionArgumentCountMismatchError expectedArgCount actualArgCount))
          else 
          --check that the atom types are valid
          case lefts eAtomTypes of
            _:_ -> pure (Left (someErrors (lefts eAtomTypes)))
            [] -> do
              let atomTypes = rights eAtomTypes
              let mValidTypes = map (\(expType, actType) -> case atomTypeVerify expType actType of 
                                        Left err -> Just err
                                        Right _ -> Nothing) (zip (dbcFuncType func) atomTypes)
                  typeErrors = catMaybes mValidTypes 
                  eAtomArgs = map (\arg -> runReader (evalAtomExpr emptyTuple arg) relExprState) atomArgExprs
              if length (lefts eAtomArgs) > 1 then
                pure (Left (someErrors (lefts eAtomArgs)))
                else if not (null typeErrors) then
                     pure (Left (someErrors typeErrors))                   
                   else
                     case evalDatabaseContextFunction func (rights eAtomArgs) context of
                       Left err -> pure (Left err)
                       Right newContext -> putStateContext newContext >> pure (Right ())
      
evalDatabaseContextIOExpr :: Maybe ScriptSession -> DatabaseContext -> TransactionId -> TransactionGraph -> DatabaseContextIOExpr -> IO (Either RelationalError DatabaseContext)
evalDatabaseContextIOExpr mScriptSession currentContext _ _ (AddAtomFunction funcName funcType script) = 
  case mScriptSession of
    Nothing -> pure (Left (ScriptError ScriptCompilationDisabledError))
    Just scriptSession -> do
      res <- try $ runGhc (Just libdir) $ do
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
          Right context' -> pure (Right context')
          
evalDatabaseContextIOExpr mScriptSession currentContext _ _ (AddDatabaseContextFunction funcName funcType script) = 
  case mScriptSession of
    Nothing -> pure (Left (ScriptError ScriptCompilationDisabledError))
    Just scriptSession -> do
      --validate that the function signature is of the form x -> y -> ... -> DatabaseContext -> DatabaseContext
      let last2Args = reverse (take 2 (reverse funcType))
          atomArgs = take (length funcType - 2) funcType
          dbContextTypeCons = ADTypeConstructor "Either" [ADTypeConstructor "DatabaseContextFunctionError" [], ADTypeConstructor "DatabaseContext" []]
          expectedType = "DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext"
          actualType = show funcType
      if last2Args /= [ADTypeConstructor "DatabaseContext" [], dbContextTypeCons] then 
        pure (Left (ScriptError (TypeCheckCompilationError expectedType actualType)))
        else do
        res <- try $ runGhc (Just libdir) $ do
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
            Right context' -> pure (Right context')
evalDatabaseContextIOExpr _ currentContext _ _ (LoadAtomFunctions modName funcName modPath) = do
  eLoadFunc <- loadAtomFunctions (T.unpack modName) (T.unpack funcName) modPath
  case eLoadFunc of
    Left LoadSymbolError -> pure (Left LoadFunctionError)
    Right atomFunctionListFunc -> let newContext = currentContext { atomFunctions = mergedFuncs }
                                      mergedFuncs = HS.union (atomFunctions currentContext) (HS.fromList atomFunctionListFunc)
                                  in pure (Right newContext)
evalDatabaseContextIOExpr _ currentContext _ _ (LoadDatabaseContextFunctions modName funcName modPath) = do
  eLoadFunc <- loadDatabaseContextFunctions (T.unpack modName) (T.unpack funcName) modPath
  case eLoadFunc of
    Left LoadSymbolError -> pure (Left LoadFunctionError)
    Right dbcListFunc -> let newContext = currentContext { dbcFunctions = mergedFuncs }
                             mergedFuncs = HS.union (dbcFunctions currentContext) (HS.fromList dbcListFunc)
                                  in pure (Right newContext)

evalDatabaseContextIOExpr _ currentContext transId graph (CreateArbitraryRelation relVarName attrExprs range) =
  --Define
  case runState (evalDatabaseContextExpr (Define relVarName attrExprs) transId graph) (freshDatabaseState currentContext) of
    (Left err,_) -> pure (Left err)
    (Right (), elems) -> do
         --Assign
           let existingRelVar = M.lookup relVarName relVarTable
               relVarTable = relationVariables (context elems)
           case existingRelVar of
                Nothing -> pure $ Left (RelVarNotDefinedError relVarName)
                Just existingRel -> do
                  case typeForGraphRefRelationalExpr existingRel transId graph of
                    Left err -> pure (Left err)
                    Right relType -> do
                      let expectedAttributes = attributes relType
                          tcMap = typeConstructorMapping (context elems)
                      eitherRel <- generate $ runReaderT (arbitraryRelation expectedAttributes range) tcMap
                      case eitherRel of
                        Left err -> pure $ Left err
                        Right rel ->
                          case runState (setRelVar relVarName (ExistingRelation rel)) elems of
                            (Left err,_) -> pure (Left err)
                            (Right (), elems') -> pure $ Right (context elems)

updateTupleWithAtomExprs :: M.Map AttributeName AtomExpr -> DatabaseContext -> RelationTuple -> Either RelationalError RelationTuple
updateTupleWithAtomExprs exprMap context tupIn = do
  --resolve all atom exprs
  atomsAssoc <- mapM (\(attrName, atomExpr) -> do
                         atom <- runReader (evalAtomExpr tupIn atomExpr) (RelationalExprStateElems context)
                         pure (attrName, atom)
                     ) (M.toList exprMap)
  pure (updateTupleWithAtoms (M.fromList atomsAssoc) tupIn)

--run verification on all constraints
checkConstraints :: DatabaseContext -> Either RelationalError ()
checkConstraints context =
  mapM_ (uncurry checkIncDep) (M.toList deps) 
  where
    deps = inclusionDependencies context
    eval expr = evalReader (evalRelationalExpr expr)
    evalReader f = runReader f (RelationalExprStateElems context)
    checkIncDep depName (InclusionDependency subsetExpr supersetExpr) = do
      --if both expressions are of a single-attribute (such as with a simple foreign key), the names of the attributes are irrelevant (they need not match) because the expression is unambiguous, but special-casing this to rename the attribute automatically would not be orthogonal behavior and probably cause confusion. Instead, special case the error to make it clear.
      typeSub <- evalReader (typeForRelationalExpr subsetExpr)
      typeSuper <- evalReader (typeForRelationalExpr supersetExpr)
      when (typeSub /= typeSuper) (Left (RelationTypeMismatchError (attributes typeSub) (attributes typeSuper)))
      let checkExpr = Equals supersetExpr (Union subsetExpr supersetExpr)
      case eval checkExpr of
        Left err -> Left err
        Right resultRel -> if resultRel == relationTrue then
                                   pure ()
                                else 
                                  Left (InclusionDependencyCheckError depName)

-- the type of a relational expression is equal to the relation attribute set returned from executing the relational expression; therefore, the type can be cheaply derived by evaluating a relational expression and ignoring and tuple processing
-- furthermore, the type of a relational expression is the resultant header of the evaluated empty-tupled relation

typeForRelationalExpr :: RelationalExpr -> RelationalExprState (Either RelationalError Relation)
typeForRelationalExpr expr = do
  rstate <- ask
  let context = stateElemsContext rstate
  --replace the relationVariables context element with a cloned set of relation devoid of tuples
  let context' = contextWithEmptyTupleSets context
      rstate' = setStateElemsContext rstate context'
  --evalRelationalExpr could still return an existing relation with tuples, so strip them
  pure $ case runReader (evalRelationalExpr expr) rstate' of
    Left err -> Left err
    Right typeRel -> Right (relationWithEmptyTupleSet typeRel)

--returns a database context with all tuples removed
--this is useful for type checking and optimization
contextWithEmptyTupleSets :: DatabaseContext -> DatabaseContext
contextWithEmptyTupleSets contextIn = contextIn { relationVariables = relVars }
  where
    relVars = M.map (\rel -> Relation (attributes rel) emptyTupleSet) (relationVariables contextIn)

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
  let (attrs', ctxtup') = case rstate of
                    RelationalExprStateElems _ -> (attrs, emptyTuple)
                    RelationalExprStateAttrsElems _ _ -> (attrs, emptyTuple)
                    RelationalExprStateTupleElems _ ctxtup -> (A.union attrs (tupleAttributes ctxtup), ctxtup)
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

tupleExprCheckNewAttrName :: AttributeName -> Relation -> Either RelationalError Relation
tupleExprCheckNewAttrName attrName rel = if isRight (attributeForName attrName rel) then
                                           Left (AttributeNameInUseError attrName)
                                         else
                                           Right rel

extendTupleExpressionProcessor :: Relation -> ExtendTupleExpr -> RelationalExprState (Either RelationalError (Attributes, RelationTuple -> Either RelationalError RelationTuple))
extendTupleExpressionProcessor relIn (AttributeExtendTupleExpr newAttrName atomExpr) = do
  rstate <- ask
  -- check that the attribute name is not in use
  case tupleExprCheckNewAttrName newAttrName relIn of
    Left err -> pure (Left err)
    Right _ -> runExceptT $ do
      atomExprType <- liftE (typeFromAtomExpr (attributes relIn) atomExpr)
      atomExprType' <- liftE (verifyAtomExprTypes relIn atomExpr atomExprType)
      let newAttrs = A.attributesFromList [Attribute newAttrName atomExprType']
          newAndOldAttrs = A.addAttributes (attributes relIn) newAttrs
      pure (newAndOldAttrs, \tup -> let substate = mergeTuplesIntoRelationalExprState tup rstate in case runReader (evalAtomExpr tup atomExpr) substate of
                 Left err -> Left err
                 Right atom -> Right (tupleAtomExtend newAttrName atom tup)
               )

evalAtomExpr :: RelationTuple -> AtomExpr -> RelationalExprState (Either RelationalError Atom)
evalAtomExpr tupIn (AttributeAtomExpr attrName) = case atomForAttributeName attrName tupIn of
  Right atom -> pure (Right atom)
  err@(Left (NoSuchAttributeNamesError _)) -> do
    rstate <- ask
    case rstate of
      RelationalExprStateElems _ -> pure err
      RelationalExprStateAttrsElems _ _ -> pure err
      RelationalExprStateTupleElems _ ctxtup -> pure (atomForAttributeName attrName ctxtup)
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

typeFromAtomExpr :: Attributes -> AtomExpr -> RelationalExprState (Either RelationalError AtomType)
typeFromAtomExpr attrs (AttributeAtomExpr attrName) = do
  rstate <- ask
  case A.atomTypeForAttributeName attrName attrs of
    Right aType -> pure (Right aType)
    Left err@(NoSuchAttributeNamesError _) -> case rstate of
        RelationalExprStateAttrsElems _ attrs' -> case A.attributeForName attrName attrs' of
          Left err' -> pure (Left err')
          Right attr -> pure (Right (A.atomType attr))
        RelationalExprStateElems _ -> pure (Left err)
        RelationalExprStateTupleElems _ tup -> case atomForAttributeName attrName tup of
          Left err' -> pure (Left err')
          Right atom -> pure (Right (atomTypeForAtom atom))
    Left err -> pure (Left err)
typeFromAtomExpr _ (NakedAtomExpr atom) = pure (Right (atomTypeForAtom atom))
typeFromAtomExpr attrs (FunctionAtomExpr funcName atomArgs _) = do
  context <- fmap stateElemsContext ask
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
typeFromAtomExpr attrs (RelationAtomExpr relExpr) = runExceptT $ do
  rstate <- lift ask
  relType <- either throwE pure (runReader (typeForRelationalExpr relExpr) (mergeAttributesIntoRelationalExprState attrs rstate))
  pure (RelationAtomType (attributes relType))
-- grab the type of the data constructor, then validate that the args match the expected types
typeFromAtomExpr attrs (ConstructedAtomExpr dConsName dConsArgs _) = 
  runExceptT $ do
    argsTypes <- mapM (liftE . typeFromAtomExpr attrs) dConsArgs 
    context <- fmap stateElemsContext (lift ask)
    either throwE pure (atomTypeForDataConstructor (typeConstructorMapping context) dConsName argsTypes)

-- | Validate that the type of the AtomExpr matches the expected type.
verifyAtomExprTypes :: Relation -> AtomExpr -> AtomType -> RelationalExprState (Either RelationalError AtomType)
verifyAtomExprTypes relIn (AttributeAtomExpr attrName) expectedType = runExceptT $ do
  rstate <- lift ask
  case A.atomTypeForAttributeName attrName (attributes relIn) of
    Right aType -> either throwE pure (atomTypeVerify expectedType aType)
    (Left err@(NoSuchAttributeNamesError _)) -> case rstate of
      RelationalExprStateTupleElems _ _ -> throwE err
      RelationalExprStateElems _ -> throwE err
      RelationalExprStateAttrsElems _ attrs -> case A.attributeForName attrName attrs of
        Left err' -> throwE err'
        Right attrType -> either throwE pure (atomTypeVerify expectedType (A.atomType attrType))
    Left err -> throwE err
verifyAtomExprTypes _ (NakedAtomExpr atom) expectedType = pure (atomTypeVerify expectedType (atomTypeForAtom atom))
verifyAtomExprTypes relIn (FunctionAtomExpr funcName funcArgExprs _) expectedType = do
  rstate <- ask
  let functions = atomFunctions context
      context = stateElemsContext rstate
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
  rstate <- lift ask
  relType <- either throwE pure (runReader (typeForRelationalExpr relationExpr) (mergeAttributesIntoRelationalExprState (attributes relIn) rstate))
  either throwE pure (atomTypeVerify expectedType (RelationAtomType (attributes relType)))
verifyAtomExprTypes rel cons@ConstructedAtomExpr{} expectedType = runExceptT $ do
  cType <- liftE (typeFromAtomExpr (attributes rel) cons)
  either throwE pure (atomTypeVerify expectedType cType)

-- | Look up the type's name and create a new attribute.
evalAttrExpr :: TypeConstructorMapping -> AttributeExpr -> Either RelationalError Attribute
evalAttrExpr aTypes (AttributeAndTypeNameExpr attrName tCons ()) = do
  aType <- atomTypeForTypeConstructorValidate True tCons aTypes M.empty
  validateAtomType aType aTypes
  Right (Attribute attrName aType)
  
evalAttrExpr _ (NakedAttributeExpr attr) = Right attr
  
evalTupleExpr :: Maybe Attributes -> TupleExpr -> RelationalExprState (Either RelationalError RelationTuple)
evalTupleExpr mAttrs (TupleExpr tupMap) = do
  context <- fmap stateElemsContext ask
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
evalGraphRefRelationalExpr (RelationVariable name tid) graph = do
  ctx <- dbContextForTransId tid graph
  case M.lookup name (relationVariables ctx) of
    Nothing -> Left (RelVarNotDefinedError name)
    Just rv -> evalGraphRefRelationalExpr rv graph

dbContextForTransId :: TransactionId -> TransactionGraph -> Either RelationalError DatabaseContext
dbContextForTransId tid graph = do
  trans <- transactionForId tid graph
  pure (concreteDatabaseContext ctx)

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

processRestrictionPredicateExpr :: RestrictionPredicateExpr -> RelationalExprState (Either RelationalError GraphRefRestrictionPredicateExpr)
processRestrictionPredicateExpr TruePredicate = TruePredicate
processRestrictionPredicateExpr (AndPredicate a b) = pure $ AndPredicate <$> processRestrictionPredicateExpr a <*> processRestrictionPredicateExpr b
processRestrictionPredicateExpr (OrPredicate a b) = OrPredicate <$> processRestrictionPredicateExpr a <*> processRestrictionPredicateExpr b
processRestrictionPredicateExpr (NotPredicate a) = pure $ NotPredicate <$> processRestrictionPredicateExpr a
processRestrictionPredicateExpr (RelationalExprPredicate expr) = pure $ RelationalExprPredicate <$> evalRelationalExpr expr
processRestrictionPredicateExpr (AtomExprPredicate expr) = AtomExprPredicate <$> evalAtomExpr expr
processRestrictionPredicateExpr (AttributeEqualityPredicate nam expr) = AttributeEqualityPredicate nam <$> evalAtomExpr expr

processExtendTupleExpr :: ExtendTupleExpr -> RelationalExprState (Either RelationalError (ExtendTupleExprBase a))
processExtendTupleExpr (AttributeExtendTupleExpr nam atomExpr) = do
  eExpr <- evalAtomExpr atomExpr
  case eExpr of
    Left err -> pure (Left err)
    Right expr -> pure (Right (AttributeExtendTupleExpr nam expr))

typeForGraphRefRelationalExpr :: GraphRefRelationalExpr -> TransactionId -> TransactionGraph -> Either RelationalError Relation
typeForGraphRefRelationalExpr (MakeStaticRelation attrs _) = mkRelation attrs emptyTupleSet
typeForGraphRefRelationalExpr (ExistingRelation rel) = pure (emptyRelationWithAttrs (attributes rel))
typeForGraphRefRelationalExpr (MakeRelationFromExprs mAttrs tupleExprs) = unimplemented

processAtomExpr :: AtomExpr -> TransactionId -> GraphRefAtomExpr
processAtomExpr = unimplemented
