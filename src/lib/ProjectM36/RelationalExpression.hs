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

import GHC
import GHC.Paths
--import Debug.Trace

--relvar state is needed in evaluation of relational expression but only as read-only in order to extract current relvar values
evalRelationalExpr :: RelationalExpr -> DatabaseState (Either RelationalError Relation)
evalRelationalExpr (RelationVariable name) = do
  relvarTable <- liftM relationVariables get
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
  currentContext <- get
  tConss <- liftM typeConstructorMapping get
  -- if the mAttrExprs is Nothing, then we should attempt to infer the tuple attributes from the first tuple itself- note that this is not always possible
  let eAttrs = map (evalAttrExpr tConss) (fromMaybe [] mAttrExprs)
  case lefts eAttrs of
    [] -> do
      let mAttrs = case mAttrExprs of
            Nothing -> Nothing
            Just _ -> Just (A.attributesFromList (rights eAttrs))
          eTuples = map (evalTupleExpr currentContext mAttrs) tupleExprs
      case lefts eTuples of
        [] -> do
          let attrs = fromMaybe firstTupleAttrs mAttrs
              tupList = rights eTuples
              firstTupleAttrs = if length tupList == 0 then A.emptyAttributes else tupleAttributes (head tupList)
          pure $ mkRelation attrs (RelationTupleSet tupList)
        errs -> pure $ Left (someErrors errs)
    errs -> pure $ Left (someErrors errs)
  
evalRelationalExpr (ExistingRelation rel) = return (Right rel)

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
  context <- get
  case evald of
    Left err -> return $ Left err
    Right rel -> case predicateRestrictionFilter context (attributes rel) predicateExpr of
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
  evald <- evalRelationalExpr relExpr
  context <- get
  case evald of
    Left err -> return $ Left err
    Right rel -> do
      case extendTupleExpressionProcessor rel tupleExpression context of
        Left err -> return $ Left err
        Right (newAttrs, tupProc) -> return $ relMogrify tupProc newAttrs rel

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
  currcontext <- get
  let newRelVars = M.delete relVarName (relationVariables currcontext)
  put $ currcontext { relationVariables = newRelVars }
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
  relVarTable <- liftM relationVariables get
  let existingRelVar = M.lookup relVarName relVarTable
  value <- evalRelationalExpr expr
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

evalContextExpr (Insert relVarName relExpr) = evalContextExpr $ Assign relVarName (Union relExpr (RelationVariable relVarName))

--assign empty rel until restriction is implemented
evalContextExpr (Delete relVarName predicate) = do
  updatedRel <- evalRelationalExpr (Restrict (NotPredicate predicate) (RelationVariable relVarName))
  case updatedRel of
    Left err -> return $ Just err
    Right rel -> setRelVar relVarName rel

--union of restricted+updated portion and the unrestricted+unupdated portion
evalContextExpr (Update relVarName atomExprMap restrictionPredicateExpr) = do
  context <- get
  let relVarTable = relationVariables context
  case M.lookup relVarName relVarTable of
    Nothing -> return $ Just (RelVarNotDefinedError relVarName)
    Just rel -> case predicateRestrictionFilter context (attributes rel) restrictionPredicateExpr of
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
      
evalDatabaseContextIOExpr :: ScriptSession -> DatabaseContext -> DatabaseContextIOExpr -> IO (Either RelationalError DatabaseContext)
evalDatabaseContextIOExpr scriptSession currentContext (AddAtomFunction funcName funcType script) = do
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
        if HS.member newAtomFunc atomFuncs then
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
  let (errors, atoms) = M.mapEither (evalAtomExpr tupIn context) exprMap
  if length errors > 0 then
    Left (MultipleErrors (M.elems errors))
    else
    Right (updateTupleWithAtoms atoms tupIn)

--run verification on all constraints
checkConstraints :: DatabaseContext -> Maybe RelationalError
checkConstraints context = case failures of
  [] -> Nothing
  l:_ -> Just l
  where
    failures = M.elems $ M.mapMaybeWithKey checkIncDep deps
    deps = inclusionDependencies context
    eval expr = runState (evalRelationalExpr expr) context
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

typeForRelationalExpr :: RelationalExpr -> DatabaseState (Either RelationalError Relation)
typeForRelationalExpr expr = do
  currstate <- get
  --replace the relationVariables context element with a cloned set of relation devoid of tuples
  put $ contextWithEmptyTupleSets currstate
  evalRelationalExpr expr

--returns a database context with all tuples removed
--this is useful for type checking and optimization
contextWithEmptyTupleSets :: DatabaseContext -> DatabaseContext
contextWithEmptyTupleSets contextIn = contextIn { relationVariables = relVars }
  where
    relVars = M.map (\rel -> Relation (attributes rel) emptyTupleSet) (relationVariables contextIn)

{- used for restrictions- take the restrictionpredicate and return the corresponding filter function -}
predicateRestrictionFilter :: DatabaseContext -> Attributes -> RestrictionPredicateExpr -> Either RelationalError (RelationTuple -> Bool)
predicateRestrictionFilter context attrs (AndPredicate expr1 expr2) = do
  expr1v <- predicateRestrictionFilter context attrs expr1
  expr2v <- predicateRestrictionFilter context attrs expr2
  return $ \x -> expr1v x && expr2v x

predicateRestrictionFilter context attrs (OrPredicate expr1 expr2) = do
  expr1v <- predicateRestrictionFilter context attrs expr1
  expr2v <- predicateRestrictionFilter context attrs expr2
  return $ \x -> expr1v x || expr2v x

predicateRestrictionFilter _ _ TruePredicate = Right $ \_ -> True

predicateRestrictionFilter context attrs (NotPredicate expr) = do
  exprv <- predicateRestrictionFilter context attrs expr
  return $ \x -> not (exprv x)

predicateRestrictionFilter context _ (RelationalExprPredicate relExpr) = case runState (evalRelationalExpr relExpr) context of
  (Left err, _) -> Left err
  (Right rel, _) -> if rel == relationTrue then
                      Right $ \_ -> True
                    else if rel == relationFalse then
                     Right $ \_ -> False
                         else
                           Left $ PredicateExpressionError "Relational restriction filter must evaluate to 'true' or 'false'"

predicateRestrictionFilter context attrs (AttributeEqualityPredicate attrName atomExpr) = do
  attr <- A.attributeForName attrName attrs
  atomExprType <- typeFromAtomExpr attrs context atomExpr
  if atomExprType /= A.atomType attr then
    Left $ TupleAttributeTypeMismatchError (A.attributesFromList [attr])
    else
    Right $ \tupleIn -> case atomForAttributeName attrName tupleIn of
      Left _ -> False
      Right atomIn ->
        case evalAtomExpr tupleIn context atomExpr of
          Left _ -> False
          Right atomCmp -> atomCmp == atomIn

-- in the future, it would be useful to do typechecking on the attribute and atom expr filters in advance
predicateRestrictionFilter context attrs (AtomExprPredicate atomExpr) = do
  aType <- typeFromAtomExpr attrs context atomExpr
  if aType /= BoolAtomType then
    Left $ AtomTypeMismatchError aType BoolAtomType
    else
    Right (\tupleIn -> case evalAtomExpr tupleIn context atomExpr of
                Left _ -> False
                Right boolAtomValue -> boolAtomValue == BoolAtom True)

tupleExprCheckNewAttrName :: AttributeName -> Relation -> Either RelationalError Relation
tupleExprCheckNewAttrName attrName rel = if isRight $ attributeForName attrName rel then
                                           Left $ AttributeNameInUseError attrName
                                         else
                                           Right rel

extendTupleExpressionProcessor :: Relation -> ExtendTupleExpr -> DatabaseContext -> Either RelationalError (Attributes, RelationTuple -> RelationTuple)
extendTupleExpressionProcessor relIn
  (AttributeExtendTupleExpr newAttrName atomExpr) context = do
    _ <- tupleExprCheckNewAttrName newAttrName relIn
    atomExprType <- typeFromAtomExpr (attributes relIn) context atomExpr
    _ <- verifyAtomExprTypes relIn context atomExpr atomExprType
    let newAttrs = A.attributesFromList [Attribute newAttrName atomExprType]
        newAndOldAttrs = A.addAttributes (attributes relIn) newAttrs
    return $ (newAndOldAttrs, \tup -> case evalAtomExpr tup context atomExpr of
                 Left _ -> error "logic failure in extendTupleExpressionProcessor"
                 Right atom -> tupleAtomExtend newAttrName atom tup)

evalAtomExpr :: RelationTuple -> DatabaseContext -> AtomExpr -> Either RelationalError Atom
evalAtomExpr tupIn _ (AttributeAtomExpr attrName) = atomForAttributeName attrName tupIn
evalAtomExpr _ _ (NakedAtomExpr atom) = Right atom
evalAtomExpr tupIn context (FunctionAtomExpr funcName arguments) = do
  let functions = atomFunctions context
  func <- atomFunctionForName funcName functions
  argTypes <- mapM (typeFromAtomExpr (tupleAttributes tupIn) context) arguments
  _ <- mapM (uncurry atomTypeVerify) $ init (zip (atomFuncType func) argTypes)
  let expectedArgCount = length (atomFuncType func) - 1
      actualArgCount = length argTypes
  if expectedArgCount /= actualArgCount then
    Left (AtomFunctionArgumentCountMismatch expectedArgCount actualArgCount)
    else do
    evaldArgs <- mapM (evalAtomExpr tupIn context) arguments
    pure $ (evalAtomFunction func) evaldArgs
evalAtomExpr _ context (RelationAtomExpr relExpr) = do
  relAtom <- evalState (evalRelationalExpr relExpr) context
  return $ RelationAtom relAtom
evalAtomExpr tupIn context cons@(ConstructedAtomExpr dConsName dConsArgs) = do
  aType <- typeFromAtomExpr (tupleAttributes tupIn) context cons
  argAtoms <- mapM (evalAtomExpr tupIn context) dConsArgs
  pure (ConstructedAtom dConsName aType argAtoms)

typeFromAtomExpr :: Attributes -> DatabaseContext -> AtomExpr -> Either RelationalError AtomType
typeFromAtomExpr attrs _ (AttributeAtomExpr attrName) = A.atomTypeForAttributeName attrName attrs
typeFromAtomExpr _ _ (NakedAtomExpr atom) = Right (atomTypeForAtom atom)
typeFromAtomExpr _ context (FunctionAtomExpr funcName _) = do
  let funcs = atomFunctions context
  func <- atomFunctionForName funcName funcs
  return $ last (atomFuncType func)
typeFromAtomExpr _ context (RelationAtomExpr relExpr) = do
  relType <- evalState (typeForRelationalExpr relExpr) context
  return $ RelationAtomType (attributes relType)

-- grab the type of the data constructor, then validate that the args match the expected types
typeFromAtomExpr attrs context (ConstructedAtomExpr dConsName dConsArgs) = do
  argsTypes <- mapM (typeFromAtomExpr attrs context) dConsArgs  
  atomTypeForDataConstructor (typeConstructorMapping context) dConsName argsTypes

-- | Validate that the type of the AtomExpr matches the expected type.
verifyAtomExprTypes :: Relation -> DatabaseContext -> AtomExpr -> AtomType -> Either RelationalError AtomType
verifyAtomExprTypes relIn _ (AttributeAtomExpr attrName) expectedType = do
  attrType <- A.atomTypeForAttributeName attrName (attributes relIn)
  atomTypeVerify expectedType attrType
verifyAtomExprTypes _ _ (NakedAtomExpr atom) expectedType = atomTypeVerify expectedType (atomTypeForAtom atom)
verifyAtomExprTypes relIn context (FunctionAtomExpr funcName funcArgExprs) expectedType = do
  let functions = atomFunctions context
  func <- atomFunctionForName funcName functions
  let expectedArgTypes = atomFuncType func
  funcArgTypes <- mapM (\(atomExpr,expectedType2,argCount) -> case verifyAtomExprTypes relIn context atomExpr expectedType2 of
                           Left (AtomTypeMismatchError expSubType actSubType) -> Left $ AtomFunctionTypeError funcName argCount expSubType actSubType
                           Left err -> Left err
                           Right x -> Right x
                           ) $ zip3 funcArgExprs expectedArgTypes [1..]
  if length funcArgTypes /= length expectedArgTypes - 1 then
    Left $ AtomTypeCountError funcArgTypes expectedArgTypes
    else do
    atomTypeVerify expectedType (last expectedArgTypes)
verifyAtomExprTypes _ context (RelationAtomExpr relationExpr) expectedType = do
  case runState (typeForRelationalExpr relationExpr) context of
    (Left err, _) -> Left err
    (Right relType, _) -> atomTypeVerify expectedType (RelationAtomType (attributes relType))
verifyAtomExprTypes rel context cons@(ConstructedAtomExpr _ _) expectedType = do
  cType <- typeFromAtomExpr (attributes rel) context cons
  atomTypeVerify expectedType cType
  

-- | Look up the type's name and create a new attribute.
evalAttrExpr :: TypeConstructorMapping -> AttributeExpr -> Either RelationalError Attribute
evalAttrExpr aTypes (AttributeAndTypeNameExpr attrName tCons) = do
  aType <- atomTypeForTypeConstructor tCons aTypes
  Right (Attribute attrName aType)
  
evalAttrExpr _ (NakedAttributeExpr attr) = Right attr
  
evalTupleExpr :: DatabaseContext -> Maybe Attributes -> TupleExpr -> Either RelationalError RelationTuple
evalTupleExpr context attrs (TupleExpr tupMap) = do
  -- it's not possible for AtomExprs in tuple constructors to reference other Attributes' atoms due to the necessary order-of-operations (need a tuple to pass to evalAtomExpr)- it may be possible with some refactoring of type usage or delayed evaluation- needs more thought, but not a priority
  -- I could adjust this logic so that when the attributes are not specified (Nothing), then I can attempt to extract the attributes from the tuple- the type resolution will blow up if an ambiguous data constructor is used (Left 4) and this should allow simple cases to "relation{tuple{a 4}}" to be processed
  attrAtoms <- mapM (\(attrName, aExpr) -> do
                        newAtom <- evalAtomExpr emptyTuple context aExpr
                        newAtomType <- typeFromAtomExpr A.emptyAttributes context aExpr
                        pure $ (attrName, newAtom, newAtomType)
                          ) (M.toList tupMap)
  let tupAttrs = A.attributesFromList $ map (\(attrName, _, aType) -> Attribute attrName aType) attrAtoms
      atoms = V.fromList $ map (\(_, atom, _) -> atom) attrAtoms
      tup = mkRelationTuple tupAttrs atoms
      tConss = typeConstructorMapping context
      finalAttrs = fromMaybe tupAttrs attrs
  --verify that the attributes match
  when (A.attributeNameSet finalAttrs /= A.attributeNameSet tupAttrs) $ Left (TupleAttributeTypeMismatchError tupAttrs)
  tup' <- resolveTypesInTuple finalAttrs (reorderTuple finalAttrs tup)
  _ <- validateTuple tup' tConss
  pure tup'
