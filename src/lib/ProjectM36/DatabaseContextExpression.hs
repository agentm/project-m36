{-# LANGUAGE ScopedTypeVariables #-}
module ProjectM36.DatabaseContextExpression where
import ProjectM36.RelationalExpression
import ProjectM36.RelationalExpressionState
import ProjectM36.Relation
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AtomFunctionBody
import ProjectM36.TupleSet
import ProjectM36.Tuple
import ProjectM36.AtomType
import ProjectM36.AtomFunction
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS
import Control.Exception
import Data.Char (isUpper)
import qualified Data.Text as T
import qualified ProjectM36.TypeConstructorDef as TCD
import Control.Monad.State
import qualified Data.Map as M
import Data.Either
import Data.Maybe

import GHC
import GHC.Paths

type ConstraintValidator = DatabaseContextExpr -> DatabaseContext -> Maybe RelationalError

data DatabaseContextEvalState = DatabaseContextEvalState {
  dbcontext :: DatabaseContext,
  constraintValidator :: ConstraintValidator
  }
                                
type DatabaseState a = State DatabaseContextEvalState a

getDatabaseContext :: DatabaseState DatabaseContext
getDatabaseContext = liftM dbcontext get

getConstraintValidator :: DatabaseState ConstraintValidator
getConstraintValidator = liftM constraintValidator get

putDatabaseContext :: DatabaseContext -> DatabaseState ()
putDatabaseContext context = do
  dbstate <- get
  put (dbstate {dbcontext = context})

emptyDatabaseContext :: DatabaseContext
emptyDatabaseContext = DatabaseContext { inclusionDependencies = M.empty,
                                         relationVariables = M.empty,
                                         atomFunctions = HS.empty,
                                         notifications = M.empty,
                                         typeConstructorMapping = []
                                         }



--helper function to process relation variable creation/assignment
--the dbcontextexpr is useful for optimizing the constraint validation
setRelVar :: DatabaseContextExpr -> RelVarName -> Relation -> DatabaseState (Maybe RelationalError)
setRelVar expr relVarName rel = do
  currentContext <- getDatabaseContext
  let newRelVars = M.insert relVarName rel $ relationVariables currentContext
      potentialContext = currentContext { relationVariables = newRelVars }
  validateConstraints expr potentialContext
      
validateConstraints :: DatabaseContextExpr -> DatabaseContext -> DatabaseState (Maybe RelationalError)
validateConstraints expr potentialContext = do
  validator <- getConstraintValidator
  case validator expr potentialContext of
    Just err -> pure (Just err)
    Nothing -> do
      putDatabaseContext potentialContext
      pure Nothing

-- it is not an error to delete a relvar which does not exist, just like it is not an error to insert a pre-existing tuple into a relation
deleteRelVar :: DatabaseContextExpr -> RelVarName -> DatabaseState (Maybe RelationalError)
deleteRelVar expr relVarName = do
  currContext <- getDatabaseContext
  let newRelVars = M.delete relVarName (relationVariables currContext)
      newContext = currContext { relationVariables = newRelVars }
  validateConstraints expr newContext

evalContextExpr :: DatabaseContextExpr -> DatabaseState (Maybe RelationalError)
evalContextExpr NoOperation = pure Nothing
  
evalContextExpr expr@(Define relVarName attrExprs) = do
  relvars <- liftM relationVariables getDatabaseContext
  tConss <- liftM typeConstructorMapping getDatabaseContext
  let eAttrs = map (evalAttrExpr tConss) attrExprs
  case lefts eAttrs of
    err:_ -> pure (Just err)
    [] -> case M.member relVarName relvars of
      True -> return (Just (RelVarAlreadyDefinedError relVarName))
      False -> setRelVar expr relVarName emptyRelation
        where
          attrs = A.attributesFromList (rights eAttrs)
          emptyRelation = Relation attrs emptyTupleSet

evalContextExpr expr@(Undefine relVarName) = do
  deleteRelVar expr relVarName

evalContextExpr ex@(Assign relVarName expr) = do
  -- in the future, it would be nice to get types from the RelationalExpr instead of needing to evaluate it
  context <- getDatabaseContext
  let existingRelVar = M.lookup relVarName relVarTable
      relVarTable = relationVariables context
      value = evalState (evalRelationalExpr expr) (RelationalExprStateElems context)
  case value of
    Left err -> return $ Just err
    Right rel -> case existingRelVar of
      Nothing -> setRelVar ex relVarName rel
      Just existingRel -> let expectedAttributes = attributes existingRel
                              foundAttributes = attributes rel in
                          if A.attributesEqual expectedAttributes foundAttributes then
                            setRelVar ex relVarName rel
                          else
                            return $ Just (RelVarAssignmentTypeMismatchError expectedAttributes foundAttributes)

evalContextExpr (Insert relVarName relExpr) = evalContextExpr $ Assign relVarName (Union relExpr (RelationVariable relVarName ()))

evalContextExpr expr@(Delete relVarName predicate) = do
  context <- getDatabaseContext
  let updatedRel = evalState (evalRelationalExpr (Restrict (NotPredicate predicate) (RelationVariable relVarName ()))) (RelationalExprStateElems context)
  case updatedRel of
    Left err -> return $ Just err
    Right rel -> setRelVar expr relVarName rel

--union of restricted+updated portion and the unrestricted+unupdated portion
evalContextExpr expr@(Update relVarName atomExprMap restrictionPredicateExpr) = do
  context <- getDatabaseContext
  let relVarTable = relationVariables context
  case M.lookup relVarName relVarTable of
    Nothing -> return $ Just (RelVarNotDefinedError relVarName)
    Just rel -> do
      case evalState (predicateRestrictionFilter (attributes rel) restrictionPredicateExpr) (RelationalExprStateElems context) of
        Left err -> return $ Just err
        Right predicateFunc -> do
          case makeUpdatedRel rel of
            Left err -> return $ Just err
            Right updatedRel -> setRelVar expr relVarName updatedRel
          where
            makeUpdatedRel relin = do
              restrictedPortion <- restrict predicateFunc relin
              unrestrictedPortion <- restrict (not . predicateFunc) relin
              updatedPortion <- relMap (updateTupleWithAtomExprs atomExprMap context) restrictedPortion
              union updatedPortion unrestrictedPortion

evalContextExpr expr@(AddInclusionDependency newDepName newDep) = do
  currContext <- getDatabaseContext
  let currDeps = inclusionDependencies currContext
      newDeps = M.insert newDepName newDep currDeps
  if M.member newDepName currDeps then
    return $ Just (InclusionDependencyNameInUseError newDepName)
    else do
      let potentialContext = currContext { inclusionDependencies = newDeps }
      --when adding a constraint, it is new and must be unconditionally validated without being optimized away
      validateConstraints expr potentialContext

evalContextExpr (RemoveInclusionDependency depName) = do
  currContext <- getDatabaseContext
  let currDeps = inclusionDependencies currContext
      newDeps = M.delete depName currDeps
  if M.notMember depName currDeps then
    return $ Just (InclusionDependencyNameNotInUseError depName)
    else do
    putDatabaseContext $ currContext {inclusionDependencies = newDeps }
    return Nothing
    
-- | Add a notification which will send the resultExpr when triggerExpr changes between commits.
evalContextExpr (AddNotification notName triggerExpr resultExpr) = do
  currentContext <- getDatabaseContext
  let nots = notifications currentContext
  if M.member notName nots then
    return $ Just (NotificationNameInUseError notName)
    else do
      let newNotifications = M.insert notName newNotification nots
          newNotification = Notification { changeExpr = triggerExpr,
                                           reportExpr = resultExpr }
      putDatabaseContext $ currentContext { notifications = newNotifications }
      return Nothing
  
evalContextExpr (RemoveNotification notName) = do
  currentContext <- getDatabaseContext
  let nots = notifications currentContext
  if M.notMember notName nots then
    return $ Just (NotificationNameNotInUseError notName)
    else do
    let newNotifications = M.delete notName nots
    putDatabaseContext $ currentContext { notifications = newNotifications }
    return Nothing

-- | Adds type and data constructors to the database context.
-- validate that the type *and* constructor names are unique! not yet implemented!
evalContextExpr (AddTypeConstructor tConsDef dConsDefList) = do
  currentContext <- getDatabaseContext
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
               putDatabaseContext $ currentContext { typeConstructorMapping = newTypes }
               pure Nothing

-- | Removing the atom constructor prevents new atoms of the type from being created. Existing atoms of the type remain. Thus, the atomTypes list in the DatabaseContext need not be all-inclusive.
evalContextExpr (RemoveTypeConstructor tConsName) = do
  currentContext <- getDatabaseContext
  let oldTypes = typeConstructorMapping currentContext
  if findTypeConstructor tConsName oldTypes == Nothing then
    pure $ Just (AtomTypeNameNotInUseError tConsName)
    else do
      let newTypes = filter (\(tCons, _) -> TCD.name tCons /= tConsName) oldTypes
      putDatabaseContext $ currentContext { typeConstructorMapping = newTypes }
      pure Nothing

evalContextExpr (MultipleExpr exprs) = do
  --the multiple expressions must pass the same context around- not the old unmodified context
  evald <- forM exprs evalContextExpr
  --some lifting magic needed here
  case catMaybes evald of
    [] -> return $ Nothing
    err:_ -> return $ Just err
             
evalContextExpr (RemoveAtomFunction funcName) = do
  currentContext <- getDatabaseContext
  let atomFuncs = atomFunctions currentContext
      dudFunc = emptyAtomFunction funcName -- just for lookup in the hashset
  if HS.member dudFunc atomFuncs then do
    let updatedFuncs = HS.delete dudFunc atomFuncs
    putDatabaseContext (currentContext {atomFunctions = updatedFuncs })
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

-- the type of a relational expression is equal to the relation attribute set returned from executing the relational expression; therefore, the type can be cheaply derived by evaluating a relational expression and ignoring and tuple processing
-- furthermore, the type of a relational expression is the resultant header of the evaluated empty-tupled relation
