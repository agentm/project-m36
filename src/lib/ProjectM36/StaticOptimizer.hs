module ProjectM36.StaticOptimizer where
import ProjectM36.Base
import ProjectM36.RelationalExpression
import ProjectM36.Relation
import ProjectM36.Error
import qualified ProjectM36.Attribute as A
import qualified ProjectM36.AttributeNames as AS
import ProjectM36.TupleSet
import Control.Monad.State hiding (join)
import Data.Either (rights, lefts)
import Control.Monad.Trans.Reader
import qualified Data.Map as M
import qualified Data.Set as S

-- the static optimizer performs optimizations which need not take any specific-relation statistics into account
-- apply optimizations which merely remove steps to become no-ops: example: projection of a relation across all of its attributes => original relation

--should optimizations offer the possibility to pure errors? If they perform the up-front type-checking, maybe so
applyStaticRelationalOptimization :: RelationalExpr -> RelationalExprState (Either RelationalError RelationalExpr)
applyStaticRelationalOptimization e@(MakeStaticRelation _ _) = pure $ Right e

applyStaticRelationalOptimization e@(MakeRelationFromExprs _ _) = pure $ Right e

applyStaticRelationalOptimization e@(ExistingRelation _) = pure $ Right e

applyStaticRelationalOptimization e@(RelationVariable _ _) = pure $ Right e
  
--remove project of attributes which removes no attributes
applyStaticRelationalOptimization (Project attrNameSet expr) = do
  relType <- typeForRelationalExpr expr
  case relType of
    Left err -> pure $ Left err
    Right relType2 
      | AS.all == attrNameSet ->                
        applyStaticRelationalOptimization expr
      | AttributeNames (attributeNames relType2) == attrNameSet ->
        applyStaticRelationalOptimization expr
      | otherwise -> do
        optimizedSubExpression <- applyStaticRelationalOptimization expr 
        case optimizedSubExpression of
          Left err -> pure $ Left err
          Right optSubExpr -> applyStaticJoinElimination (Project attrNameSet optSubExpr)
                           
applyStaticRelationalOptimization (Union exprA exprB) = do
  eOptExprA <- applyStaticRelationalOptimization exprA
  eOptExprB <- applyStaticRelationalOptimization exprB
  case eOptExprA of 
    Left err -> pure $ Left err
    Right optExprA -> case eOptExprB of
      Left err -> pure $ Left err
      Right optExprB -> -- (x where pred1) union (x where pred2) -> (x where pred1 or pred2)
        case (optExprA, optExprB) of 
          (Restrict predA (RelationVariable nameA ()),
           Restrict predB (RelationVariable nameB ())) | nameA == nameB -> pure (Right (Restrict (AndPredicate predA predB) (RelationVariable nameA ())))
          _ -> if optExprA == optExprB then           
            pure (Right optExprA)
            else
            pure $ Right $ Union optExprA optExprB
                            
applyStaticRelationalOptimization (Join exprA exprB) = do
  eOptExprA <- applyStaticRelationalOptimization exprA
  eOptExprB <- applyStaticRelationalOptimization exprB
  case eOptExprA of
    Left err -> pure $ Left err
    Right optExprA -> case eOptExprB of
      Left err -> pure $ Left err
      Right optExprB -> 
        -- if the relvars to join are the same but with predicates, then just AndPredicate the predicates
        case (optExprA, optExprB) of
          (Restrict predA (RelationVariable nameA ()),
           Restrict predB (RelationVariable nameB ())) | nameA == nameB -> pure (Right (Restrict  (AndPredicate predA predB) (RelationVariable nameA ())))
          _ -> if optExprA == optExprB then --A join A == A
                           pure (Right optExprA)
                         else
                           pure (Right (Join optExprA optExprB))
                           
applyStaticRelationalOptimization (Difference exprA exprB) = do
  optExprA <- applyStaticRelationalOptimization exprA
  optExprB <- applyStaticRelationalOptimization exprB
  case optExprA of
    Left err -> pure $ Left err
    Right optExprA2 -> case optExprB of
      Left err -> pure $ Left err
      Right optExprB2 -> if optExprA == optExprB then do --A difference A == A where false
                           eEmptyRel <- typeForRelationalExpr optExprA2
                           case eEmptyRel of
                             Left err -> pure (Left err)
                             Right emptyRel -> pure (Right (ExistingRelation emptyRel))
                         else
                           pure $ Right (Difference optExprA2 optExprB2)
                           
applyStaticRelationalOptimization e@Rename{} = pure $ Right e

applyStaticRelationalOptimization (Group oldAttrNames newAttrName expr) =
  pure $ Right $ Group oldAttrNames newAttrName expr
  
applyStaticRelationalOptimization (Ungroup attrName expr) =
  pure $ Right $ Ungroup attrName expr
  
--remove restriction of nothing
applyStaticRelationalOptimization (Restrict predicate expr) = do
  optimizedPredicate <- applyStaticPredicateOptimization predicate
  case optimizedPredicate of
    Left err -> pure $ Left err
    Right optimizedPredicate2 
      | isTrueExpr optimizedPredicate2 -> applyStaticRelationalOptimization expr -- remove predicate entirely
      | isFalseExpr optimizedPredicate2 -> do -- replace where false predicate with empty relation with attributes from relexpr
        attributesRel <- typeForRelationalExpr expr
        case attributesRel of 
          Left err -> pure $ Left err
          Right attributesRelA -> pure $ Right $ MakeStaticRelation (attributes attributesRelA) emptyTupleSet
      | otherwise -> do
        optimizedSubExpression <- applyStaticRelationalOptimization expr
        case optimizedSubExpression of
          Left err -> pure $ Left err
          Right optSubExpr -> pure $ Right $ Restrict optimizedPredicate2 optSubExpr
  
applyStaticRelationalOptimization e@(Equals _ _) = pure $ Right e 

applyStaticRelationalOptimization e@(NotEquals _ _) = pure $ Right e 
  
applyStaticRelationalOptimization e@(Extend _ _) = pure $ Right e  

applyStaticDatabaseOptimization :: DatabaseContextExpr -> DatabaseState (Either RelationalError DatabaseContextExpr)
applyStaticDatabaseOptimization x@NoOperation = pure $ Right x
applyStaticDatabaseOptimization x@(Define _ _) = pure $ Right x

applyStaticDatabaseOptimization x@(Undefine _) = pure $ Right x

applyStaticDatabaseOptimization (Assign name expr) = do
  context <- getStateContext
  let optimizedExpr = runReader (applyStaticRelationalOptimization expr) (RelationalExprStateElems context)
  case optimizedExpr of
    Left err -> pure $ Left err
    Right optimizedExpr2 -> pure $ Right (Assign name optimizedExpr2)
    
applyStaticDatabaseOptimization (Insert targetName expr) = do
  context <- getStateContext
  let optimizedExpr = runReader (applyStaticRelationalOptimization expr) (RelationalExprStateElems context)
  case optimizedExpr of
    Left err -> pure $ Left err
    Right optimizedExpr2 -> if isEmptyRelationExpr optimizedExpr2 then -- if we are trying to insert an empty relation, do nothing
                              pure (Right NoOperation)
                              else 
                              case optimizedExpr2 of 
                                -- if the target relvar and the insert relvar are the same, there is nothing to do
                                -- insert s s -> NoOperation
                                RelationVariable insName () | insName == targetName -> pure (Right NoOperation)
                                _ -> pure $ Right (Insert targetName optimizedExpr2)
  
applyStaticDatabaseOptimization (Delete name predicate) = do  
  context <- getStateContext
  let optimizedPredicate = runReader (applyStaticPredicateOptimization predicate) (RelationalExprStateElems context)
  case optimizedPredicate of
      Left err -> pure $ Left err
      Right optimizedPredicate2 -> pure $ Right (Delete name optimizedPredicate2)

applyStaticDatabaseOptimization (Update name upmap predicate) = do 
  context <- getStateContext
  let optimizedPredicate = runReader (applyStaticPredicateOptimization predicate) (RelationalExprStateElems context)
  case optimizedPredicate of
      Left err -> pure $ Left err
      Right optimizedPredicate2 -> pure $ Right (Update name upmap optimizedPredicate2)
      
applyStaticDatabaseOptimization dep@(AddInclusionDependency _ _) = pure $ Right dep

applyStaticDatabaseOptimization (RemoveInclusionDependency name) = pure $ Right (RemoveInclusionDependency name)

applyStaticDatabaseOptimization (AddNotification name triggerExpr resultOldExpr resultNewExpr) = do
  context <- getStateContext
  let eTriggerExprOpt = runReader (applyStaticRelationalOptimization triggerExpr) (RelationalExprStateElems context)
  case eTriggerExprOpt of
         Left err -> pure $ Left err
         Right triggerExprOpt -> --it doesn't make sense to optimize queries when we don't have their proper contexts
           pure (Right (AddNotification name triggerExprOpt resultOldExpr resultNewExpr))

applyStaticDatabaseOptimization notif@(RemoveNotification _) = pure (Right notif)

applyStaticDatabaseOptimization c@(AddTypeConstructor _ _) = pure (Right c)
applyStaticDatabaseOptimization c@(RemoveTypeConstructor _) = pure (Right c)
applyStaticDatabaseOptimization c@(RemoveAtomFunction _) = pure (Right c)
applyStaticDatabaseOptimization c@(RemoveDatabaseContextFunction _) = pure (Right c)
applyStaticDatabaseOptimization c@(ExecuteDatabaseContextFunction _ _) = pure (Right c)

--optimization: from pgsql lists- check for join condition referencing foreign key- if join projection project away the referenced table, then it does not need to be scanned

--applyStaticDatabaseOptimization (MultipleExpr exprs) = pure $ Right $ MultipleExpr exprs
--for multiple expressions, we must evaluate
applyStaticDatabaseOptimization (MultipleExpr exprs) = do
  context <- getStateContext
  let optExprs = evalState substateRunner (contextWithEmptyTupleSets context, M.empty, False)
  let errors = lefts optExprs
  if not (null errors) then
    pure $ Left (head errors)
    else
      pure $ Right $ MultipleExpr (rights optExprs)
   where
     substateRunner = forM exprs $ \expr -> do
                                    --a previous expression could create a relvar, we don't want to miss it, so we clear the tuples and execute the expression to get an empty relation in the relvar                                
       _ <- evalDatabaseContextExpr expr    
       applyStaticDatabaseOptimization expr
  --this error handling could be improved with some lifting presumably
  --restore original context

applyStaticPredicateOptimization :: RestrictionPredicateExpr -> RelationalExprState (Either RelationalError RestrictionPredicateExpr)
applyStaticPredicateOptimization predi = do
  eOptPred <- case predi of 
-- where x and x => where x
    AndPredicate pred1 pred2 -> do
      eOptPred1 <- applyStaticPredicateOptimization pred1
      case eOptPred1 of
        Left err -> pure (Left err)
        Right optPred1 -> do
          eOptPred2 <- applyStaticPredicateOptimization pred2
          case eOptPred2 of
            Left err -> pure (Left err)
            Right optPred2 ->
              if optPred1 == optPred2 then
                pure (Right optPred1)
                else
                pure (Right (AndPredicate optPred1 optPred2))
-- where x or x => where x    
    OrPredicate pred1 pred2 -> do
      eOptPred1 <- applyStaticPredicateOptimization pred1
      case eOptPred1 of
        Left err -> pure (Left err)
        Right optPred1 -> do
          eOptPred2 <- applyStaticPredicateOptimization pred2
          case eOptPred2 of
            Left err -> pure (Left err)
            Right optPred2 | optPred1 == optPred2 -> pure (Right optPred1)
                           | isTrueExpr optPred1 -> pure (Right optPred1)  -- True or x -> True
                           | isTrueExpr optPred2 -> pure (Right optPred2)
                           | otherwise -> pure (Right (OrPredicate optPred1 optPred2))
    AttributeEqualityPredicate attrNameA (AttributeAtomExpr attrNameB) ->
      if attrNameA == attrNameB then
        pure (Right TruePredicate)
      else
        pure (Right predi)
    AttributeEqualityPredicate{} -> pure (Right predi)
    TruePredicate -> pure $ Right predi
    NotPredicate{} -> pure $ Right predi
    RelationalExprPredicate{} -> pure (Right predi)
    AtomExprPredicate{} -> pure (Right predi)
  case eOptPred of
    Left err -> pure (Left err)
    Right optPred -> 
      let attrMap = findStaticRestrictionPredicates optPred in
      pure (Right (replaceStaticAtomExprs optPred attrMap))

--determines if an atom expression is tautologically true
isTrueExpr :: RestrictionPredicateExpr -> Bool
isTrueExpr TruePredicate = True
isTrueExpr (AtomExprPredicate (NakedAtomExpr (BoolAtom True))) = True
isTrueExpr _ = False

--determines if an atom expression is tautologically false
isFalseExpr :: RestrictionPredicateExpr -> Bool
isFalseExpr (NotPredicate expr) = isTrueExpr expr
isFalseExpr (AtomExprPredicate (NakedAtomExpr (BoolAtom False))) = True
isFalseExpr _ = False

-- determine if the created relation can statically be determined to be empty
isEmptyRelationExpr :: RelationalExpr -> Bool    
isEmptyRelationExpr (MakeRelationFromExprs _ []) = True
isEmptyRelationExpr (MakeStaticRelation _ tupSet) = null (asList tupSet)
isEmptyRelationExpr (ExistingRelation rel) = rel == emptyRelationWithAttrs (attributes rel)
isEmptyRelationExpr _ = False
    
--transitive static variable optimization                        
replaceStaticAtomExprs :: RestrictionPredicateExpr -> M.Map AttributeName AtomExpr -> RestrictionPredicateExpr
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
findStaticRestrictionPredicates :: RestrictionPredicateExpr -> M.Map AttributeName AtomExpr
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
applyStaticJoinElimination :: RelationalExpr -> RelationalExprState (Either RelationalError RelationalExpr)
applyStaticJoinElimination expr@(Project attrNameSet (Join exprA exprB)) = do
  eProjType <- typeForRelationalExpr expr
  eTypeA <- typeForRelationalExpr exprA
  eTypeB <- typeForRelationalExpr exprB
  case eProjType of
    Left err -> pure (Left err)
    Right projType -> 
      case eTypeA of 
        Left err -> pure (Left err)
        Right typeA -> 
          case eTypeB of
            Left err -> pure (Left err)
            Right typeB -> do
              let matchesProjectionAttributes = if attrNames projType `S.isSubsetOf` attrNames typeA then
                                                  Just ((exprA, typeA), (exprB, typeB))
                                                else if attrNames projType `S.isSubsetOf` attrNames typeB then
                                                       Just ((exprB, typeB), (exprA, typeA))
                                                     else 
                                                       Nothing
                  attrNames = A.attributeNameSet . attributes
              case matchesProjectionAttributes of
                Nothing ->  -- this optimization does not apply
                  pure (Right expr)
                Just ((joinedExpr, joinedType), (unjoinedExpr, _)) -> do
                  --scan inclusion dependencies for a foreign key relationship
                  incDeps <- inclusionDependencies . stateElemsContext <$> ask
                  let fkConstraint = foldM isFkConstraint False incDeps
                      --search for matching fk constraint
                      isFkConstraint acc (InclusionDependency (Project subattrNames subrv) (Project _ superrv)) = 
                        case AS.projectionAttributesForAttributeNames (attributes projType) subattrNames of
                          Left _ -> pure acc
                          Right subAttrs -> 
                            pure (acc || (joinedExpr == subrv &&
                                          unjoinedExpr == superrv && 
                                          -- the fk attribute is one of the projection attributes
                                          A.attributesContained subAttrs (attributes joinedType)
                                ))
                      isFkConstraint acc _ = pure acc
                  case fkConstraint of
                    Right True -> --join elimination optimization applies
                      applyStaticRelationalOptimization (Project attrNameSet joinedExpr)
                    Right False -> --join elimination optimization does not apply
                      pure (Right expr)
                    Left err -> 
                      pure (Left err)
          
applyStaticJoinElimination expr = pure (Right expr)      
                                                                              
