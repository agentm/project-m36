module ProjectM36.StaticOptimizer where
import ProjectM36.Base
import ProjectM36.RelationalExpression
import ProjectM36.Relation
import ProjectM36.Error
import qualified ProjectM36.AttributeNames as AS
import ProjectM36.TupleSet
import Control.Monad.State hiding (join)
import Data.Either (rights, lefts)

-- the static optimizer performs optimizations which need not take any specific-relation statistics into account
-- apply optimizations which merely remove steps to become no-ops: example: projection of a relation across all of its attributes => original relation

--should optimizations offer the possibility to return errors? If they perform the up-front type-checking, maybe so
applyStaticRelationalOptimization :: RelationalExpr -> DatabaseState (Either RelationalError RelationalExpr)
applyStaticRelationalOptimization e@(MakeStaticRelation _ _) = return $ Right e

applyStaticRelationalOptimization e@(MakeRelationFromExprs _ _) = return $ Right e

applyStaticRelationalOptimization e@(ExistingRelation _) = return $ Right e

applyStaticRelationalOptimization e@(RelationVariable _ _) = return $ Right e

--remove project of attributes which removes no attributes
applyStaticRelationalOptimization (Project attrNameSet expr) = do
  relType <- typeForRelationalExpr expr
  case relType of
    Left err -> return $ Left err
    Right relType2 -> if attributeNames relType2 == AS.attributeNameSet attrNameSet then
                       applyStaticRelationalOptimization expr
                       else do
                         optimizedSubExpression <- applyStaticRelationalOptimization expr 
                         case optimizedSubExpression of
                           Left err -> return $ Left err
                           Right optSubExpr -> return $ Right $ Project attrNameSet optSubExpr
                           
applyStaticRelationalOptimization (Union exprA exprB) = do
  optExprA <- applyStaticRelationalOptimization exprA
  optExprB <- applyStaticRelationalOptimization exprB
  case optExprA of 
    Left err -> return $ Left err
    Right optExprAx -> case optExprB of
      Left err -> return $ Left err
      Right optExprBx -> if optExprAx == optExprBx then                          
                          return (Right optExprAx)
                          else
                            return $ Right $ Union optExprAx optExprBx
                            
applyStaticRelationalOptimization (Join exprA exprB) = do
  optExprA <- applyStaticRelationalOptimization exprA
  optExprB <- applyStaticRelationalOptimization exprB
  case optExprA of
    Left err -> return $ Left err
    Right optExprA2 -> case optExprB of
      Left err -> return $ Left err
      Right optExprB2 -> if optExprA == optExprB then --A join A == A
                           return optExprA
                         else
                           return $ Right (Join optExprA2 optExprB2)
                           
applyStaticRelationalOptimization (Difference exprA exprB) = do
  optExprA <- applyStaticRelationalOptimization exprA
  optExprB <- applyStaticRelationalOptimization exprB
  case optExprA of
    Left err -> return $ Left err
    Right optExprA2 -> case optExprB of
      Left err -> return $ Left err
      Right optExprB2 -> if optExprA == optExprB then do --A difference A == A where false
                           eEmptyRel <- typeForRelationalExpr optExprA2
                           case eEmptyRel of
                             Left err -> pure (Left err)
                             Right emptyRel -> pure (Right (ExistingRelation emptyRel))
                         else
                           return $ Right (Difference optExprA2 optExprB2)
                           
applyStaticRelationalOptimization e@(Rename _ _ _) = return $ Right e

applyStaticRelationalOptimization (Group oldAttrNames newAttrName expr) = do 
  return $ Right $ Group oldAttrNames newAttrName expr
  
applyStaticRelationalOptimization (Ungroup attrName expr) = do 
  return $ Right $ Ungroup attrName expr
  
--remove restriction of nothing
applyStaticRelationalOptimization (Restrict predicate expr) = do
  optimizedPredicate <- applyStaticPredicateOptimization predicate
  case optimizedPredicate of
    Left err -> return $ Left err
    Right optimizedPredicate2 -> if optimizedPredicate2 == TruePredicate then
                                  applyStaticRelationalOptimization expr
                                  else if optimizedPredicate2 == NotPredicate TruePredicate then do
                                    attributesRel <- typeForRelationalExpr expr
                                    case attributesRel of 
                                      Left err -> return $ Left err
                                      Right attributesRelA -> return $ Right $ MakeStaticRelation (attributes attributesRelA) emptyTupleSet
                                      else do
                                      optimizedSubExpression <- applyStaticRelationalOptimization expr
                                      case optimizedSubExpression of
                                        Left err -> return $ Left err
                                        Right optSubExpr -> return $ Right $ Restrict optimizedPredicate2 optSubExpr
  
applyStaticRelationalOptimization e@(Equals _ _) = return $ Right e 

applyStaticRelationalOptimization e@(NotEquals _ _) = return $ Right e 
  
applyStaticRelationalOptimization e@(Extend _ _) = return $ Right e  

applyStaticDatabaseOptimization :: DatabaseContextExpr -> DatabaseState (Either RelationalError DatabaseContextExpr)
applyStaticDatabaseOptimization x@NoOperation = pure $ Right x
applyStaticDatabaseOptimization x@(Define _ _) = pure $ Right x

applyStaticDatabaseOptimization x@(Undefine _) = pure $ Right x

applyStaticDatabaseOptimization (Assign name expr) = do
  optimizedExpr <- applyStaticRelationalOptimization expr
  case optimizedExpr of
    Left err -> return $ Left err
    Right optimizedExpr2 -> return $ Right (Assign name optimizedExpr2)
    
applyStaticDatabaseOptimization (Insert name expr) = do
  optimizedExpr <- applyStaticRelationalOptimization expr
  case optimizedExpr of
    Left err -> return $ Left err
    Right optimizedExpr2 -> return $ Right (Insert name optimizedExpr2)
  
applyStaticDatabaseOptimization (Delete name predicate) = do  
  optimizedPredicate <- applyStaticPredicateOptimization predicate
  case optimizedPredicate of
      Left err -> return $ Left err
      Right optimizedPredicate2 -> return $ Right (Delete name optimizedPredicate2)

applyStaticDatabaseOptimization (Update name upmap predicate) = do 
  optimizedPredicate <- applyStaticPredicateOptimization predicate
  case optimizedPredicate of
      Left err -> return $ Left err
      Right optimizedPredicate2 -> return $ Right (Update name upmap optimizedPredicate2)
      
applyStaticDatabaseOptimization dep@(AddInclusionDependency _ _) = return $ Right dep

applyStaticDatabaseOptimization (RemoveInclusionDependency name) = return $ Right (RemoveInclusionDependency name)

applyStaticDatabaseOptimization (AddNotification name triggerExpr resultExpr) = do
    eTriggerExprOpt <- applyStaticRelationalOptimization triggerExpr
    case eTriggerExprOpt of
         Left err -> pure $ Left err
         Right triggerExprOpt -> do
             eResultExprOpt <- applyStaticRelationalOptimization resultExpr
             case eResultExprOpt of
                  Left err -> pure $ Left err
                  Right resultExprOpt -> pure (Right (AddNotification name triggerExprOpt resultExprOpt))

applyStaticDatabaseOptimization notif@(RemoveNotification _) = pure (Right notif)

applyStaticDatabaseOptimization c@(AddTypeConstructor _ _) = pure (Right c)
applyStaticDatabaseOptimization c@(RemoveTypeConstructor _) = pure (Right c)
applyStaticDatabaseOptimization c@(RemoveAtomFunction _) = pure (Right c)

--optimization: from pgsql lists- check for join condition referencing foreign key- if join projection project away the referenced table, then it does not need to be scanned

--applyStaticDatabaseOptimization (MultipleExpr exprs) = return $ Right $ MultipleExpr exprs
--for multiple expressions, we must evaluate
applyStaticDatabaseOptimization (MultipleExpr exprs) = do
  context <- get
  let optExprs = evalState substateRunner (contextWithEmptyTupleSets context) 
  let errors = lefts optExprs
  if length errors > 0 then
    return $ Left (head errors)
    else
      return $ Right $ MultipleExpr (rights optExprs)
   where
     substateRunner = forM exprs $ \expr -> do
                                    --a previous expression could create a relvar, we don't want to miss it, so we clear the tuples and execute the expression to get an empty relation in the relvar                                
       _ <- evalContextExpr expr    
       applyStaticDatabaseOptimization expr
  --this error handling could be improved with some lifting presumably
  --restore original context

applyStaticPredicateOptimization :: RestrictionPredicateExpr -> DatabaseState (Either RelationalError RestrictionPredicateExpr)
applyStaticPredicateOptimization predicate = return $ Right predicate