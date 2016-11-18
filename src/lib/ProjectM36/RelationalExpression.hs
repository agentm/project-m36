{-# LANGUAGE ScopedTypeVariables #-}
module ProjectM36.RelationalExpression where
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AtomType
import ProjectM36.AtomFunctionBody
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunction
import qualified ProjectM36.Attribute as A
import qualified Data.Map as M
import qualified Data.HashSet as HS
import Control.Monad.State hiding (join)
import Control.Exception
import Data.Maybe
import Data.Either
import Data.Char (isUpper)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified ProjectM36.TypeConstructorDef as TCD
import Control.Monad.Trans.Except

import GHC
import GHC.Paths
--import Debug.Trace

-- we need to pass around a higher level RelationTuple and Attributes in order to solve #52
data RelationalExprStateElems = RelationalExprStateTupleElems DatabaseContext RelationTuple | -- used when fully evaluating a relexpr
                                RelationalExprStateAttrsElems DatabaseContext Attributes | --used when evaluating the type of a relexpr
                                RelationalExprStateElems DatabaseContext --used by default at the top level of evaluation
type DatabaseState a = State DatabaseContext a

type RelationalExprState a = State RelationalExprStateElems a

stateContext :: RelationalExprStateElems -> DatabaseContext
stateContext (RelationalExprStateTupleElems ctx _) = ctx
stateContext (RelationalExprStateAttrsElems ctx _) = ctx
stateContext (RelationalExprStateElems ctx) = ctx

setStateContext :: RelationalExprStateElems -> DatabaseContext -> RelationalExprStateElems
setStateContext (RelationalExprStateTupleElems _ tup) ctx = RelationalExprStateTupleElems ctx tup
setStateContext (RelationalExprStateAttrsElems _ attrs) ctx = RelationalExprStateAttrsElems ctx attrs
setStateContext (RelationalExprStateElems _) ctx = RelationalExprStateElems ctx

--relvar state is needed in evaluation of relational expression but only as read-only in order to extract current relvar values
evalRelationalExpr :: RelationalExpr -> RelationalExprState (Either RelationalError Relation)
evalRelationalExpr (RelationVariable name _) = do
  relvarTable <- liftM (relationVariables . stateContext) get
  return $ case M.lookup name relvarTable of
    Just res -> Right res
    Nothing -> Left $ RelVarNotDefinedError name

evalRelationalExpr (Project attrNames expr) = do
    rel <- evalRelationalExpr expr
    case rel of
      Right rel2 -> return $ project attrNames rel2
      Left err -> return $ Left err

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
      
evalRelationalExpr (MakeStaticRelation attributeSet tupleSet) = do
  case mkRelation attributeSet tupleSet of
    Right rel -> return $ Right rel
    Left err -> return $ Left err
    
evalRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) = do
  currentContext <- liftM stateContext get
  let tConss = typeConstructorMapping currentContext
  -- if the mAttrExprs is Nothing, then we should attempt to infer the tuple attributes from the first tuple itself- note that this is not always possible
  runExceptT $ do
    attrs <- mapM (\expr -> either throwE pure (evalAttrExpr tConss expr)) (fromMaybe [] mAttrExprs)
    let mAttrs = case mAttrExprs of
          Nothing -> Nothing
          Just _ -> Just (A.attributesFromList attrs)
    tuples <- mapM (\expr -> liftE (evalTupleExpr mAttrs expr)) tupleExprs
    let attrs = fromMaybe firstTupleAttrs mAttrs
        firstTupleAttrs = if length tuples == 0 then A.emptyAttributes else tupleAttributes (head tuples)
    either throwE pure (mkRelation attrs (RelationTupleSet tuples))
  
evalRelationalExpr (ExistingRelation rel) = pure (Right rel)

evalRelationalExpr (Rename oldAttrName newAttrName relExpr) = do
  evald <- evalRelationalExpr relExpr
  case evald of
    Right rel -> return $ rename oldAttrName newAttrName rel
    Left err -> return $ Left err

evalRelationalExpr (Group oldAttrNameSet newAttrName relExpr) = do
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
        Right filterfunc -> return $ restrict filterfunc rel

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
  state <- get
  let evald = evalState (evalRelationalExpr relExpr) state
  case evald of
    Left err -> return $ Left err
    Right rel -> do
      tupProc <- extendTupleExpressionProcessor rel tupleExpression
      case tupProc of
        Left err -> pure (Left err)
        Right (newAttrs, tupProc) -> pure $ relMogrify tupProc newAttrs rel

emptyDatabaseContext :: DatabaseContext
emptyDatabaseContext = DatabaseContext { inclusionDependencies = M.empty,
                                         relationVariables = M.empty,
                                         atomFunctions = HS.empty,
                                         notifications = M.empty,
                                         typeConstructorMapping = []
                                         }



--helper function to process relation variable creation/assignment
setRelVar :: RelVarName -> Relation -> DatabaseState (Maybe RelationalError)
setRelVar relVarName rel = do
  currentContext <- get
  let newRelVars = M.insert relVarName rel $ relationVariables currentContext
      potentialContext = currentContext { relationVariables = newRelVars }
                        
  case checkConstraints potentialContext of
    Just err -> return $ Just err
    Nothing -> do
      put potentialContext
      return Nothing

-- it is not an error to delete a relvar which does not exist, just like it is not an error to insert a pre-existing tuple into a relation
deleteRelVar :: RelVarName -> DatabaseState (Maybe RelationalError)
deleteRelVar relVarName = do
  currContext <- get
  let newRelVars = M.delete relVarName (relationVariables currContext)
      newContext = currContext { relationVariables = newRelVars }
  put newContext
  return Nothing

evalContextExpr :: DatabaseContextExpr -> DatabaseState (Maybe RelationalError)
evalContextExpr NoOperation = pure Nothing
  
evalContextExpr (Define relVarName attrExprs) = do
  relvars <- liftM relationVariables get
  tConss <- liftM typeConstructorMapping get
  let eAttrs = map (evalAttrExpr tConss) attrExprs
  case lefts eAttrs of
    err:_ -> pure (Just err)
    [] -> case M.member relVarName relvars of
      True -> return (Just (RelVarAlreadyDefinedError relVarName))
      False -> setRelVar relVarName emptyRelation
        where
          attrs = A.attributesFromList (rights eAttrs)
          emptyRelation = Relation attrs emptyTupleSet

evalContextExpr (Undefine relVarName) = do
  deleteRelVar relVarName

evalContextExpr (Assign relVarName expr) = do
  -- in the future, it would be nice to get types from the RelationalExpr instead of needing to evaluate it
  context <- get
  let existingRelVar = M.lookup relVarName relVarTable
      relVarTable = relationVariables context
      value = evalState (evalRelationalExpr expr) (RelationalExprStateElems context)
  case value of
    Left err -> return $ Just err
    Right rel -> case existingRelVar of
      Nothing -> setRelVar relVarName rel
      Just existingRel -> let expectedAttributes = attributes existingRel
                              foundAttributes = attributes rel in
                          if A.attributesEqual expectedAttributes foundAttributes then
                            setRelVar relVarName rel
                          else
                            return $ Just (RelVarAssignmentTypeMismatchError expectedAttributes foundAttributes)

evalContextExpr (Insert relVarName relExpr) = evalContextExpr $ Assign relVarName (Union relExpr (RelationVariable relVarName ()))

evalContextExpr (Delete relVarName predicate) = do
  context <- get
  let updatedRel = evalState (evalRelationalExpr (Restrict (NotPredicate predicate) (RelationVariable relVarName ()))) (RelationalExprStateElems context)
  case updatedRel of
    Left err -> return $ Just err
    Right rel -> setRelVar relVarName rel

--union of restricted+updated portion and the unrestricted+unupdated portion
evalContextExpr (Update relVarName atomExprMap restrictionPredicateExpr) = do
  context <- get
  let relVarTable = relationVariables context
  case M.lookup relVarName relVarTable of
    Nothing -> return $ Just (RelVarNotDefinedError relVarName)
    Just rel -> do
      case evalState (predicateRestrictionFilter (attributes rel) restrictionPredicateExpr) (RelationalExprStateElems context) of
        Left err -> return $ Just err
        Right predicateFunc -> do
          case makeUpdatedRel rel of
            Left err -> return $ Just err
            Right updatedRel -> setRelVar relVarName updatedRel
          where
            makeUpdatedRel relin = do
              restrictedPortion <- restrict predicateFunc relin
              unrestrictedPortion <- restrict (not . predicateFunc) relin
              updatedPortion <- relMap (updateTupleWithAtomExprs atomExprMap context) restrictedPortion
              union updatedPortion unrestrictedPortion

evalContextExpr (AddInclusionDependency newDepName newDep) = do
  currContext <- get
  let currDeps = inclusionDependencies currContext
      newDeps = M.insert newDepName newDep currDeps
  if M.member newDepName currDeps then
    return $ Just (InclusionDependencyNameInUseError newDepName)
    else do
      let potentialContext = currContext { inclusionDependencies = newDeps }
      case checkConstraints potentialContext of
        Just err -> return $ Just err
        Nothing -> do
          put potentialContext
          return Nothing

evalContextExpr (RemoveInclusionDependency depName) = do
  currContext <- get
  let currDeps = inclusionDependencies currContext
      newDeps = M.delete depName currDeps
  if M.notMember depName currDeps then
    return $ Just (InclusionDependencyNameNotInUseError depName)
    else do
    put $ currContext {inclusionDependencies = newDeps }
    return Nothing
    
-- | Add a notification which will send the resultExpr when triggerExpr changes between commits.
evalContextExpr (AddNotification notName triggerExpr resultExpr) = do
  currentContext <- get
  let nots = notifications currentContext
  if M.member notName nots then
    return $ Just (NotificationNameInUseError notName)
    else do
      let newNotifications = M.insert notName newNotification nots
          newNotification = Notification { changeExpr = triggerExpr,
                                           reportExpr = resultExpr }
      put $ currentContext { notifications = newNotifications }
      return Nothing
  
evalContextExpr (RemoveNotification notName) = do
  currentContext <- get
  let nots = notifications currentContext
  if M.notMember notName nots then
    return $ Just (NotificationNameNotInUseError notName)
    else do
    let newNotifications = M.delete notName nots
    put $ currentContext { notifications = newNotifications }
    return Nothing

-- | Adds type and data constructors to the database context.
-- validate that the type *and* constructor names are unique! not yet implemented!
evalContextExpr (AddTypeConstructor tConsDef dConsDefList) = do
  currentContext <- get
  let oldTypes = typeConstructorMapping currentContext
      tConsName = TCD.name tConsDef
  -- validate that the constructor's types exist
  case validateTypeConstructorDef tConsDef dConsDefList of
    errs@(_:_) -> pure $ Just (someErrors errs)
    [] -> do
      if T.length tConsName < 1 || not (isUpper (T.head tConsName)) then
        pure $ Just (InvalidAtomTypeName tConsName)
        else if isJust (findTypeConstructor tConsName oldTypes) then
               pure $ Just (AtomTypeNameInUseError tConsName)
             else do
               let newTypes = oldTypes ++ [(tConsDef, dConsDefList)]
               put $ currentContext { typeConstructorMapping = newTypes }
               pure Nothing

-- | Removing the atom constructor prevents new atoms of the type from being created. Existing atoms of the type remain. Thus, the atomTypes list in the DatabaseContext need not be all-inclusive.
evalContextExpr (RemoveTypeConstructor tConsName) = do
  currentContext <- get
  let oldTypes = typeConstructorMapping currentContext
  if findTypeConstructor tConsName oldTypes == Nothing then
    pure $ Just (AtomTypeNameNotInUseError tConsName)
    else do
      let newTypes = filter (\(tCons, _) -> TCD.name tCons /= tConsName) oldTypes
      put $ currentContext { typeConstructorMapping = newTypes }
      pure Nothing

evalContextExpr (MultipleExpr exprs) = do
  --the multiple expressions must pass the same context around- not the old unmodified context
  evald <- forM exprs evalContextExpr
  --some lifting magic needed here
  case catMaybes evald of
    [] -> return $ Nothing
    err:_ -> return $ Just err
             
evalContextExpr (RemoveAtomFunction funcName) = do
  currentContext <- get
  let atomFuncs = atomFunctions currentContext
      dudFunc = emptyAtomFunction funcName -- just for lookup in the hashset
  if HS.member dudFunc atomFuncs then do
    let updatedFuncs = HS.delete dudFunc atomFuncs
    put (currentContext {atomFunctions = updatedFuncs })
    pure Nothing
    else
      pure (Just (AtomFunctionNameNotInUseError funcName))
      
evalDatabaseContextIOExpr :: Maybe ScriptSession -> DatabaseContext -> DatabaseContextIOExpr -> IO (Either RelationalError DatabaseContext)
evalDatabaseContextIOExpr mScriptSession currentContext (AddAtomFunction funcName funcType script) = do
  case mScriptSession of
    Nothing -> pure (Left (AtomFunctionBodyScriptError ScriptCompilationDisabledError))
    Just scriptSession -> do
      res <- try $ runGhc (Just libdir) $ do
        setSession (hscEnv scriptSession)
        let atomFuncs = atomFunctions currentContext
        --compile the function
        eCompiledFunc  <- compileAtomFunctionScript scriptSession script
        pure $ case eCompiledFunc of
          Left err -> Left (AtomFunctionBodyScriptError err)
          Right compiledFunc -> do
            funcAtomType <- mapM (\funcTypeArg -> atomTypeForTypeConstructor funcTypeArg (typeConstructorMapping currentContext)) funcType
            let updatedFuncs = HS.insert newAtomFunc atomFuncs
                newContext = currentContext { atomFunctions = updatedFuncs }
                newAtomFunc = AtomFunction { atomFuncName = funcName,
                                           atomFuncType = funcAtomType,
                                           atomFuncBody = AtomFunctionBody (Just script) compiledFunc }
      
            -- check if the name is already in use
            if HS.member funcName (HS.map atomFuncName atomFuncs) then
              Left (AtomFunctionNameInUseError funcName)
              else do
              Right newContext
      case res of
        Left (exc :: SomeException) -> pure $ Left (AtomFunctionBodyScriptError (OtherScriptCompilationError (show exc)))
        Right eContext -> case eContext of
          Left err -> pure (Left err)
          Right context' -> pure (Right context')
    
updateTupleWithAtomExprs :: (M.Map AttributeName AtomExpr) -> DatabaseContext -> RelationTuple -> Either RelationalError RelationTuple
updateTupleWithAtomExprs exprMap context tupIn = do
  --resolve all atom exprs
  atomsAssoc <- mapM (\(attrName, atomExpr) -> do
                         atom <- evalState (evalAtomExpr tupIn atomExpr) (RelationalExprStateElems context)
                         pure (attrName, atom)
                     ) (M.toList exprMap)
  pure (updateTupleWithAtoms (M.fromList atomsAssoc) tupIn)

--run verification on all constraints
checkConstraints :: DatabaseContext -> Maybe RelationalError
checkConstraints context = case failures of
  [] -> Nothing
  l:_ -> Just l
  where
    failures = M.elems $ M.mapMaybeWithKey checkIncDep deps
    deps = inclusionDependencies context
    eval expr = runState (evalRelationalExpr expr) (RelationalExprStateElems context)
    checkIncDep depName (InclusionDependency subsetExpr supersetExpr) = case evaldSub of
        (Left err, _) -> Just err
        (Right relSub, _) -> case evaldSuper of
          (Left err, _) -> Just err
          (Right relSuper, _) -> case union relSub relSuper of
            Left err -> Just err
            Right resultRel -> if resultRel == relSuper then
                                 Nothing
                               else
                                 Just $ InclusionDependencyCheckError depName
       where
         evaldSub = eval subsetExpr
         evaldSuper = eval supersetExpr

{-
checkConstraint :: InclusionDependency -> DatabaseState (Maybe RelationalError)
checkConstraint (InclusionDependency name subDep superDep) = do
  evalSub <- evalRelationalExpr subDep
  evalSuper <- evalRelationalExpr superDep
  case evalSub of
    Left err -> Just err
    Right relSub
  result <- liftM2 union evalSub evalSuper
  return $ Nothing
-}

-- the type of a relational expression is equal to the relation attribute set returned from executing the relational expression; therefore, the type can be cheaply derived by evaluating a relational expression and ignoring and tuple processing
-- furthermore, the type of a relational expression is the resultant header of the evaluated empty-tupled relation

typeForRelationalExpr :: RelationalExpr -> RelationalExprState (Either RelationalError Relation)
typeForRelationalExpr expr = do
  state <- get
  let context = stateContext state
  --replace the relationVariables context element with a cloned set of relation devoid of tuples
  let context' = contextWithEmptyTupleSets context
      state' = setStateContext state context'
  pure (evalState (evalRelationalExpr expr) state')

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
predicateRestrictionFilter :: Attributes -> RestrictionPredicateExpr -> RelationalExprState (Either RelationalError (RelationTuple -> Bool))
predicateRestrictionFilter attrs (AndPredicate expr1 expr2) = do
  runExceptT $ do
    expr1v <- liftE (predicateRestrictionFilter attrs expr1)
    expr2v <- liftE (predicateRestrictionFilter attrs expr2)
    pure (\x -> expr1v x && expr2v x)

predicateRestrictionFilter attrs (OrPredicate expr1 expr2) = do
  runExceptT $ do
    expr1v <- liftE (predicateRestrictionFilter attrs expr1)
    expr2v <- liftE (predicateRestrictionFilter attrs expr2)
    pure (\x -> expr1v x || expr2v x)

predicateRestrictionFilter _ TruePredicate = pure (Right (\_ -> True))

predicateRestrictionFilter attrs (NotPredicate expr) = do
  runExceptT $ do
    exprv <- liftE (predicateRestrictionFilter attrs expr)
    pure (\x -> not (exprv x))

predicateRestrictionFilter attrs (RelationalExprPredicate relExpr) = runExceptT $ do
    rel <- liftE (evalRelationalExpr relExpr)
    if rel == relationTrue then
      pure (\_ -> True)
      else if rel == relationFalse then
             pure (\_ -> False)
           else
             throwE (PredicateExpressionError "Relational restriction filter must evaluate to 'true' or 'false'")

predicateRestrictionFilter attrs (AttributeEqualityPredicate attrName atomExpr) = do
  state <- get
  runExceptT $ do
    atomExprType <- liftE (typeFromAtomExpr attrs atomExpr)
    attr <- either throwE pure (A.attributeForName attrName attrs)
    if atomExprType /= A.atomType attr then
      throwE (TupleAttributeTypeMismatchError (A.attributesFromList [attr]))
      else
      pure $ \tupleIn -> case atomForAttributeName attrName tupleIn of
        Left _ -> False
        Right atomIn -> 
          let atomEvald = evalState (evalAtomExpr tupleIn atomExpr) state in
          case atomEvald of
            Left _ -> False
            Right atomCmp -> atomCmp == atomIn

-- in the future, it would be useful to do typechecking on the attribute and atom expr filters in advance
predicateRestrictionFilter attrs (AtomExprPredicate atomExpr) = do
  state <- get
  runExceptT $ do
    aType <- liftE (typeFromAtomExpr attrs atomExpr)
    if aType /= BoolAtomType then
      throwE (AtomTypeMismatchError aType BoolAtomType)
      else
      pure (\tupleIn -> do
                case evalState (evalAtomExpr tupleIn atomExpr) state of
                  Left _ -> False
                  Right boolAtomValue -> boolAtomValue == BoolAtom True)

tupleExprCheckNewAttrName :: AttributeName -> Relation -> Either RelationalError Relation
tupleExprCheckNewAttrName attrName rel = if isRight $ attributeForName attrName rel then
                                           Left $ AttributeNameInUseError attrName
                                         else
                                           Right rel

extendTupleExpressionProcessor :: Relation -> ExtendTupleExpr -> RelationalExprState (Either RelationalError (Attributes, RelationTuple -> Either RelationalError RelationTuple))
extendTupleExpressionProcessor relIn (AttributeExtendTupleExpr newAttrName atomExpr) = do
  state <- get
  -- check that the attribute name is not in use
  case tupleExprCheckNewAttrName newAttrName relIn of
    Left err -> pure (Left err)
    Right _ -> runExceptT $ do
      atomExprType <- liftE (typeFromAtomExpr (attributes relIn) atomExpr)
      _ <- liftE (verifyAtomExprTypes relIn atomExpr atomExprType)
      let newAttrs = A.attributesFromList [Attribute newAttrName atomExprType]
          newAndOldAttrs = A.addAttributes (attributes relIn) newAttrs
      pure $ (newAndOldAttrs, \tup -> case evalState (evalAtomExpr tup atomExpr) state of
                 Left err -> Left err
                 Right atom -> Right (tupleAtomExtend newAttrName atom tup))

evalAtomExpr :: RelationTuple -> AtomExpr -> RelationalExprState (Either RelationalError Atom)
evalAtomExpr tupIn (AttributeAtomExpr attrName) = pure (atomForAttributeName attrName tupIn)
evalAtomExpr _ (NakedAtomExpr atom) = pure (Right atom)
evalAtomExpr tupIn (FunctionAtomExpr funcName arguments ()) = do
  argTypes <- mapM (typeFromAtomExpr (tupleAttributes tupIn)) arguments
  context <- liftM stateContext get                    
  runExceptT $ do
    let functions = atomFunctions context
    func <- either throwE pure (atomFunctionForName funcName functions)
    let expectedArgCount = length (atomFuncType func) - 1
        actualArgCount = length argTypes
        safeInit [_] = []
        safeInit [] = [] -- different behavior from normal init
        safeInit (_:xs) = safeInit xs
    if expectedArgCount /= actualArgCount then
      throwE (AtomFunctionArgumentCountMismatch expectedArgCount actualArgCount)
      else do
      _ <- mapM (\(expType, actType) -> either throwE pure (atomTypeVerify expType actType)) (safeInit (zip (atomFuncType func) argTypes))
      evaldArgs <- mapM (\arg -> liftE (evalAtomExpr tupIn arg)) arguments
      pure $ (evalAtomFunction func) evaldArgs
evalAtomExpr tupIn (RelationAtomExpr relExpr) = do
  --merge existing state tuple context into new state tuple context to support an arbitrary number of levels, but new attributes trounce old attributes
  runExceptT $ do
    relAtom <- liftE (evalRelationalExpr relExpr)
    pure (RelationAtom relAtom)
evalAtomExpr tupIn cons@(ConstructedAtomExpr dConsName dConsArgs ()) = runExceptT $ do
  aType <- liftE (typeFromAtomExpr (tupleAttributes tupIn) cons)
  argAtoms <- mapM (\arg -> liftE (evalAtomExpr tupIn arg)) dConsArgs
  pure (ConstructedAtom dConsName aType argAtoms)

typeFromAtomExpr :: Attributes -> AtomExpr -> RelationalExprState (Either RelationalError AtomType)
typeFromAtomExpr attrs (AttributeAtomExpr attrName) = pure (A.atomTypeForAttributeName attrName attrs)
typeFromAtomExpr _ (NakedAtomExpr atom) = pure (Right (atomTypeForAtom atom))
typeFromAtomExpr _ (FunctionAtomExpr funcName _ _) = do
  context <- liftM stateContext get
  let funcs = atomFunctions context
  runExceptT $ do
    func <- either throwE pure (atomFunctionForName funcName funcs)
    pure (last (atomFuncType func))
typeFromAtomExpr attrs (RelationAtomExpr relExpr) = runExceptT $ do
  relType <- liftE (typeForRelationalExpr relExpr)
  pure (RelationAtomType (attributes relType))
-- grab the type of the data constructor, then validate that the args match the expected types
typeFromAtomExpr attrs (ConstructedAtomExpr dConsName dConsArgs _) = 
  runExceptT $ do
    argsTypes <- mapM (\arg -> liftE (typeFromAtomExpr attrs arg)) dConsArgs  
    context <- liftM stateContext get
    aType <- either throwE pure (atomTypeForDataConstructor (typeConstructorMapping context) dConsName argsTypes)
    pure aType

-- | Validate that the type of the AtomExpr matches the expected type.
verifyAtomExprTypes :: Relation -> AtomExpr -> AtomType -> RelationalExprState (Either RelationalError AtomType)
verifyAtomExprTypes relIn (AttributeAtomExpr attrName) expectedType = runExceptT $ do
  attrType <- either throwE pure (A.atomTypeForAttributeName attrName (attributes relIn))
  either throwE pure (atomTypeVerify expectedType attrType)
verifyAtomExprTypes _ (NakedAtomExpr atom) expectedType = pure (atomTypeVerify expectedType (atomTypeForAtom atom))
verifyAtomExprTypes relIn (FunctionAtomExpr funcName funcArgExprs _) expectedType = do
  state <- get
  let functions = atomFunctions context
      context = stateContext state
  runExceptT $ do
    func <- either throwE pure (atomFunctionForName funcName functions)
    let expectedArgTypes = atomFuncType func
    funcArgTypes <- mapM (\(atomExpr,expectedType2,argCount) -> case evalState (verifyAtomExprTypes relIn atomExpr expectedType2) state of
                           Left (AtomTypeMismatchError expSubType actSubType) -> throwE (AtomFunctionTypeError funcName argCount expSubType actSubType)
                           Left err -> throwE (err)
                           Right x -> pure x
                           ) $ zip3 funcArgExprs expectedArgTypes [1..]
    if length funcArgTypes /= length expectedArgTypes - 1 then
      throwE (AtomTypeCountError funcArgTypes expectedArgTypes)
      else do
      either throwE pure (atomTypeVerify expectedType (last expectedArgTypes))
verifyAtomExprTypes _ (RelationAtomExpr relationExpr) expectedType = runExceptT $ do
  relType <- liftE (typeForRelationalExpr relationExpr)
  either throwE pure (atomTypeVerify expectedType (RelationAtomType (attributes relType)))
verifyAtomExprTypes rel cons@(ConstructedAtomExpr _ _ _) expectedType = runExceptT $ do
  cType <- liftE (typeFromAtomExpr (attributes rel) cons)
  either throwE pure (atomTypeVerify expectedType cType)

-- | Look up the type's name and create a new attribute.
evalAttrExpr :: TypeConstructorMapping -> AttributeExpr -> Either RelationalError Attribute
evalAttrExpr aTypes (AttributeAndTypeNameExpr attrName tCons ()) = do
  aType <- atomTypeForTypeConstructor tCons aTypes
  Right (Attribute attrName aType)
  
evalAttrExpr _ (NakedAttributeExpr attr) = Right attr
  
evalTupleExpr :: Maybe Attributes -> TupleExpr -> RelationalExprState (Either RelationalError RelationTuple)
evalTupleExpr attrs (TupleExpr tupMap) = runExceptT $ do
  -- it's not possible for AtomExprs in tuple constructors to reference other Attributes' atoms due to the necessary order-of-operations (need a tuple to pass to evalAtomExpr)- it may be possible with some refactoring of type usage or delayed evaluation- needs more thought, but not a priority
  -- I could adjust this logic so that when the attributes are not specified (Nothing), then I can attempt to extract the attributes from the tuple- the type resolution will blow up if an ambiguous data constructor is used (Left 4) and this should allow simple cases to "relation{tuple{a 4}}" to be processed
  context <- liftM stateContext get
  attrAtoms <- mapM (\(attrName, aExpr) -> do
                        newAtom <- liftE (evalAtomExpr emptyTuple aExpr)
                        newAtomType <- liftE (typeFromAtomExpr A.emptyAttributes aExpr)
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

