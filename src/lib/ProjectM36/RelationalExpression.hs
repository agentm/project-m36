{-# LANGUAGE ScopedTypeVariables #-}
module ProjectM36.RelationalExpression where
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Base
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
freshDatabaseState ctx = (ctx, M.empty, False) --future work: propagate return accumulator

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

type DatabaseStateElems = (DatabaseContext, M.Map ResultAccumName ResultAccum, DirtyFlag)

type DatabaseState a = State DatabaseStateElems a

getStateContext :: DatabaseState DatabaseContext
getStateContext = do
  (ctx,_, _) <- get
  pure ctx
  
putStateContext :: DatabaseContext -> DatabaseState () 
putStateContext ctx = do
  (_, accum, _) <- get
  put (ctx, accum, True)
  
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
evalRelationalExpr :: RelationalExpr -> RelationalExprState (Either RelationalError Relation)
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
          Right rel2 -> pure $ project attrNameSet rel2
          Left err -> pure $ Left err

evalRelationalExpr (Union exprA exprB) = do
  relA <- evalRelationalExpr exprA
  relB <- evalRelationalExpr exprB
  case relA of
    Left err -> return $ Left err
    Right relA2 -> case relB of
      Left err -> return $ Left err
      Right relB2 -> return $ union relA2 relB2

evalRelationalExpr (Join exprA exprB) = do
  relA <- evalRelationalExpr exprA
  relB <- evalRelationalExpr exprB
  case relA of
    Left err -> return $ Left err
    Right relA2 -> case relB of
      Left err -> return $ Left err
      Right relB2 -> return $ join relA2 relB2
      
evalRelationalExpr (Difference exprA exprB) = do
  relA <- evalRelationalExpr exprA
  relB <- evalRelationalExpr exprB
  case relA of
    Left err -> return $ Left err
    Right relA2 -> case relB of
      Left err -> return $ Left err
      Right relB2 -> return $ difference relA2 relB2
      
evalRelationalExpr (MakeStaticRelation attributeSet tupleSet) = 
  case mkRelation attributeSet tupleSet of
    Right rel -> return $ Right rel
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
    either throwE pure (mkRelation attrs (RelationTupleSet tuples))
  
evalRelationalExpr (ExistingRelation rel) = pure (Right rel)

evalRelationalExpr (Rename oldAttrName newAttrName relExpr) = do
  evald <- evalRelationalExpr relExpr
  case evald of
    Right rel -> return $ rename oldAttrName newAttrName rel
    Left err -> return $ Left err

evalRelationalExpr (Group oldAttrNames newAttrName relExpr) = do
  eOldAttrNameSet <- evalAttributeNames oldAttrNames relExpr
  case eOldAttrNameSet of
    Left err -> pure (Left err)
    Right oldAttrNameSet -> do
      evald <- evalRelationalExpr relExpr
      case evald of
        Right rel -> return $ group oldAttrNameSet newAttrName rel
        Left err -> return $ Left err

evalRelationalExpr (Ungroup attrName relExpr) = do
  evald <- evalRelationalExpr relExpr
  case evald of
    Right rel -> return $ ungroup attrName rel
    Left err -> return $ Left err

evalRelationalExpr (Restrict predicateExpr relExpr) = do
  evald <- evalRelationalExpr relExpr
  case evald of
    Left err -> return $ Left err
    Right rel -> do
      eFilterFunc <- predicateRestrictionFilter (attributes rel) predicateExpr
      case eFilterFunc of
        Left err -> return $ Left err
        Right filterfunc -> 
          pure (restrict filterfunc rel)

evalRelationalExpr (Equals relExprA relExprB) = do
  evaldA <- evalRelationalExpr relExprA
  evaldB <- evalRelationalExpr relExprB
  case evaldA of
    Left err -> return $ Left err
    Right relA -> case evaldB of
      Left err -> return $ Left err
      Right relB -> return $ Right $ if relA == relB then relationTrue else relationFalse

--warning: copy-pasta from above- refactor
evalRelationalExpr (NotEquals relExprA relExprB) = do
  evaldA <- evalRelationalExpr relExprA
  evaldB <- evalRelationalExpr relExprB
  case evaldA of
    Left err -> return $ Left err
    Right relA -> case evaldB of
      Left err -> return $ Left err
      Right relB -> return $ Right $ if relA /= relB then relationTrue else relationFalse

-- extending a relation adds a single attribute with the results of the per-tuple expression evaluated
evalRelationalExpr (Extend tupleExpression relExpr) = do
  rstate <- ask
  let evald = runReader (evalRelationalExpr relExpr) rstate
  case evald of
    Left err -> pure (Left err)
    Right rel -> do
      tupProc <- extendTupleExpressionProcessor rel tupleExpression
      case tupProc of
        Left err -> pure (Left err)
        Right (newAttrs, tupProc') -> pure $ relMogrify tupProc' newAttrs rel

--helper function to process relation variable creation/assignment
setRelVar :: RelVarName -> Relation -> DatabaseState (Either RelationalError ())
setRelVar relVarName rel = do
  currentContext <- getStateContext
  let newRelVars = M.insert relVarName rel $ relationVariables currentContext
      potentialContext = currentContext { relationVariables = newRelVars }
                        
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

evalDatabaseContextExpr :: DatabaseContextExpr -> DatabaseState (Either RelationalError ())
evalDatabaseContextExpr NoOperation = pure (Right ())
  
evalDatabaseContextExpr (Define relVarName attrExprs) = do
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
          False -> setRelVar relVarName emptyRelation >> pure (Right ())
            where
              attrs = A.attributesFromList attrsList
              emptyRelation = Relation attrs emptyTupleSet

evalDatabaseContextExpr (Undefine relVarName) = deleteRelVar relVarName

evalDatabaseContextExpr (Assign relVarName expr) = do
  -- in the future, it would be nice to get types from the RelationalExpr instead of needing to evaluate it
  context <- getStateContext
  let existingRelVar = M.lookup relVarName relVarTable
      relVarTable = relationVariables context
      value = runReader (evalRelationalExpr expr) (RelationalExprStateElems context)
  case value of
    Left err -> pure (Left err)
    Right rel -> case existingRelVar of
      Nothing -> setRelVar relVarName rel
      Just existingRel -> let expectedAttributes = attributes existingRel
                              foundAttributes = attributes rel in
                          if A.attributesEqual expectedAttributes foundAttributes then
                            setRelVar relVarName rel
                          else
                            pure (Left (RelationTypeMismatchError expectedAttributes foundAttributes))

evalDatabaseContextExpr (Insert relVarName relExpr) = do
  context <- getStateContext
  let unionexp = Union relExpr rv
      rv = RelationVariable relVarName ()
      unioned = runReader (evalRelationalExpr unionexp) (RelationalExprStateElems context)
      origRel = runReader (evalRelationalExpr rv) (RelationalExprStateElems context)
  case unioned of
    Left err -> pure (Left err)
    Right unioned' -> case origRel of
      Left err -> pure (Left err)
      Right origRel' -> if cardinality unioned' == cardinality origRel' then --no tuples actually inserted
                          pure (Right ())
                        else
                          evalDatabaseContextExpr $ Assign relVarName (ExistingRelation unioned')

evalDatabaseContextExpr (Delete relVarName predicate) = do
  context <- getStateContext
  let rv = RelationVariable relVarName ()
  let updatedRel = runReader (evalRelationalExpr (Restrict (NotPredicate predicate) rv)) (RelationalExprStateElems context)
      origRel = runReader (evalRelationalExpr rv) (RelationalExprStateElems context)
  case updatedRel of
    Left err -> pure (Left err)
    Right updatedRel' -> case origRel of
                      Left err -> pure (Left err)
                      Right origRel' -> if cardinality origRel' == cardinality updatedRel' then
                                          pure (Right ())
                                        else
                                          setRelVar relVarName updatedRel'

--union of restricted+updated portion and the unrestricted+unupdated portion
evalDatabaseContextExpr (Update relVarName atomExprMap restrictionPredicateExpr) = do
  context <- getStateContext
  let relVarTable = relationVariables context
  case M.lookup relVarName relVarTable of
    Nothing -> pure (Left (RelVarNotDefinedError relVarName))
    Just rel -> 
      case runReader (predicateRestrictionFilter (attributes rel) restrictionPredicateExpr) (RelationalExprStateElems context) of
        Left err -> pure (Left err)
        Right predicateFunc -> do
          let ret = do
                restrictedPortion <- restrict predicateFunc rel
                if cardinality restrictedPortion == Finite 0 then 
                  pure Nothing
                  else do
                  unrestrictedPortion <- restrict (predicateFunc >=> (pure . not)) rel
                  updatedPortion <- relMap (updateTupleWithAtomExprs atomExprMap context) restrictedPortion
                  updatedRel <- updatedPortion `union` unrestrictedPortion
                  pure (Just updatedRel)
          case ret of 
            Left err -> pure (Left err)
            Right Nothing -> pure (Right ())
            Right (Just updatedRel) -> setRelVar relVarName updatedRel

evalDatabaseContextExpr (AddInclusionDependency newDepName newDep) = do
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

evalDatabaseContextExpr (RemoveInclusionDependency depName) = do
  currContext <- getStateContext
  let currDeps = inclusionDependencies currContext
      newDeps = M.delete depName currDeps
  if M.notMember depName currDeps then
    pure (Left (InclusionDependencyNameNotInUseError depName))
    else do
    putStateContext $ currContext {inclusionDependencies = newDeps }
    return (Right ())
    
-- | Add a notification which will send the resultExpr when triggerExpr changes between commits.
evalDatabaseContextExpr (AddNotification notName triggerExpr resultOldExpr resultNewExpr) = do
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
  
evalDatabaseContextExpr (RemoveNotification notName) = do
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
evalDatabaseContextExpr (AddTypeConstructor tConsDef dConsDefList) = do
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
evalDatabaseContextExpr (RemoveTypeConstructor tConsName) = do
  currentContext <- getStateContext
  let oldTypes = typeConstructorMapping currentContext
  if isNothing (findTypeConstructor tConsName oldTypes) then
    pure (Left (AtomTypeNameNotInUseError tConsName))
    else do
      let newTypes = filter (\(tCons, _) -> TCD.name tCons /= tConsName) oldTypes
      putStateContext $ currentContext { typeConstructorMapping = newTypes }
      pure (Right ())

evalDatabaseContextExpr (MultipleExpr exprs) = do
  --the multiple expressions must pass the same context around- not the old unmodified context
  evald <- forM exprs evalDatabaseContextExpr
  --some lifting magic needed here
  case lefts evald of
    [] -> pure (Right ())
    errs -> pure (Left (someErrors errs))
             
evalDatabaseContextExpr (RemoveAtomFunction funcName) = do
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
      
evalDatabaseContextExpr (RemoveDatabaseContextFunction funcName) = do      
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
      
evalDatabaseContextExpr (ExecuteDatabaseContextFunction funcName atomArgExprs) = do
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
      
evalDatabaseContextIOExpr :: Maybe ScriptSession -> DatabaseContext -> DatabaseContextIOExpr -> IO (Either RelationalError DatabaseContext)
evalDatabaseContextIOExpr mScriptSession currentContext (AddAtomFunction funcName funcType script) = 
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
                funcAtomType <- mapM (\funcTypeArg -> atomTypeForTypeConstructor funcTypeArg (typeConstructorMapping currentContext) M.empty) adjustedAtomTypeCons
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
          
evalDatabaseContextIOExpr mScriptSession currentContext (AddDatabaseContextFunction funcName funcType script) = 
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
evalDatabaseContextIOExpr _ currentContext (LoadAtomFunctions modName funcName modPath) = do
  eLoadFunc <- loadAtomFunctions (T.unpack modName) (T.unpack funcName) modPath
  case eLoadFunc of
    Left LoadSymbolError -> pure (Left LoadFunctionError)
    Right atomFunctionListFunc -> let newContext = currentContext { atomFunctions = mergedFuncs }
                                      mergedFuncs = HS.union (atomFunctions currentContext) (HS.fromList atomFunctionListFunc)
                                  in pure (Right newContext)
evalDatabaseContextIOExpr _ currentContext (LoadDatabaseContextFunctions modName funcName modPath) = do
  eLoadFunc <- loadDatabaseContextFunctions (T.unpack modName) (T.unpack funcName) modPath
  case eLoadFunc of
    Left LoadSymbolError -> pure (Left LoadFunctionError)
    Right dbcListFunc -> let newContext = currentContext { dbcFunctions = mergedFuncs }
                             mergedFuncs = HS.union (dbcFunctions currentContext) (HS.fromList dbcListFunc)
                                  in pure (Right newContext)

evalDatabaseContextIOExpr _ currentContext (CreateArbitraryRelation relVarName attrExprs range)  = do
  --Define
  case runState ( evalDatabaseContextExpr (Define relVarName attrExprs)) (freshDatabaseState currentContext) of
    (Left err,_) -> pure (Left err)
    (Right (), elems@(ctx,_,_)) -> do
         --Assign
           let existingRelVar = M.lookup relVarName relVarTable
               relVarTable = relationVariables ctx 
           case existingRelVar of
                Nothing -> pure $ Left (RelVarNotDefinedError relVarName)
                Just existingRel -> do
                                      let expectedAttributes = attributes existingRel
                                      let tcMap = typeConstructorMapping ctx
                                      eitherRel <- generate $ runReaderT (arbitraryRelation expectedAttributes range) tcMap
                                      case eitherRel of
                                           Left err -> pure $ Left err
                                           Right rel -> do
                                             case runState (setRelVar relVarName rel) elems of
                                                  (Left err,_) -> pure (Left err)
                                                  (Right (), (ctx',_,_)) -> pure $ Right ctx'

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
checkConstraints context = do
  mapM_ (uncurry checkIncDep) (M.toList deps) 
  pure ()
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
  pure (runReader (evalRelationalExpr expr) rstate')

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
predicateRestrictionFilter :: Attributes -> RestrictionPredicateExpr -> RelationalExprState (Either RelationalError RestrictionFilter)
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
    aType <- liftE (typeFromAtomExpr attrs atomExpr)
    if aType /= BoolAtomType then
      throwE (AtomTypeMismatchError aType BoolAtomType)
      else
      pure (\tupleIn ->
                pure $ case runReader (evalAtomExpr tupleIn atomExpr) rstate of
                  Left _ -> False
                  Right boolAtomValue -> boolAtomValue == BoolAtom True)

tupleExprCheckNewAttrName :: AttributeName -> Relation -> Either RelationalError Relation
tupleExprCheckNewAttrName attrName rel = if isRight (attributeForName attrName rel) then
                                           Left (error "SPAMMIT" $ AttributeNameInUseError attrName)
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
        safeInit [_] = []
        safeInit [] = [] -- different behavior from normal init
        safeInit (_:xs) = safeInit xs
    if expectedArgCount /= actualArgCount then
      throwE (FunctionArgumentCountMismatchError expectedArgCount actualArgCount)
      else do
      mapM_ (\(expType, actType) -> either throwE pure (atomTypeVerify expType actType)) (safeInit (zip (atomFuncType func) argTypes))
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
  aType <- atomTypeForTypeConstructor tCons aTypes M.empty
  validateAtomType aType aTypes
  Right (Attribute attrName aType)
  
evalAttrExpr _ (NakedAttributeExpr attr) = Right attr
  
evalTupleExpr :: Maybe Attributes -> TupleExpr -> RelationalExprState (Either RelationalError RelationTuple)
evalTupleExpr attrs (TupleExpr tupMap) = do
  context <- fmap stateElemsContext ask
  runExceptT $ do
  -- it's not possible for AtomExprs in tuple constructors to reference other Attributes' atoms due to the necessary order-of-operations (need a tuple to pass to evalAtomExpr)- it may be possible with some refactoring of type usage or delayed evaluation- needs more thought, but not a priority
  -- I could adjust this logic so that when the attributes are not specified (Nothing), then I can attempt to extract the attributes from the tuple- the type resolution will blow up if an ambiguous data constructor is used (Left 4) and this should allow simple cases to "relation{tuple{a 4}}" to be processed
    attrAtoms <- mapM (\(attrName, aExpr) -> do
                          newAtomType <- liftE (typeFromAtomExpr A.emptyAttributes aExpr)
                          newAtom <- liftE (evalAtomExpr emptyTuple aExpr)
                          pure (attrName, newAtom, newAtomType)
                      ) (M.toList tupMap)
    let tupAttrs = A.attributesFromList $ map (\(attrName, _, aType) -> Attribute attrName aType) attrAtoms
        atoms = V.fromList $ map (\(_, atom, _) -> atom) attrAtoms
        tup = mkRelationTuple tupAttrs atoms
        tConss = typeConstructorMapping context
        finalAttrs = fromMaybe tupAttrs attrs
    --verify that the attributes match
    when (A.attributeNameSet finalAttrs /= A.attributeNameSet tupAttrs) $ throwE (TupleAttributeTypeMismatchError tupAttrs)
    tup' <- either throwE pure (resolveTypesInTuple finalAttrs (reorderTuple finalAttrs tup))
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
              
