module RelationStaticOptimizer where
import RelationType
import RelationExpr
import Relation
import RelationalError
import RelationTupleSet
import Control.Monad.State hiding (join)
import Data.Either (rights, lefts)

-- the static optimizer performs optimizations which need not take any specific-relation statistics into account
-- apply optimizations which merely remove steps to become no-ops: example: projection of a relation across all of its attributes => original relation

--should optimizations offer the possibility to return errors? If they perform the up-front type-checking, maybe so
applyStaticRelationalOptimization :: RelationalExpr -> DatabaseState (Either RelationalError RelationalExpr)
applyStaticRelationalOptimization e@(MakeStaticRelation _ _) = return $ Right e

applyStaticRelationalOptimization e@(RelationVariable _) = return $ Right e

--remove project of attributes which removes no attributes
applyStaticRelationalOptimization (Project attrNameSet expr) = do
  relType <- typeForRelationalExpr expr
  case relType of
    Left err -> return $ Left err
    Right relType -> if attributeNames relType == attrNameSet then
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
  typeA <- typeForRelationalExpr exprA
  typeB <- typeForRelationalExpr exprB
  context <- get
  case typeA of 
    Left err -> return $ Left err
    Right typeA -> case typeB of
      Left err -> return $ Left err
      Right typeB -> if typeA == typeB then --no new attributes to add
                       return $ Right exprA
                     else do
                       optExprA <- applyStaticRelationalOptimization exprA
                       optExprB <- applyStaticRelationalOptimization exprB
  
                       case optExprA of
                         Left err -> return $ Left err
                         Right optExprA -> case optExprB of
                           Left err -> return $ Left err
                           Right optExprB -> return $ Right (Join optExprA optExprB)
                           
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
    Right optimizedPredicate -> if optimizedPredicate == TruePredicate then
                                  applyStaticRelationalOptimization expr
                                  else if optimizedPredicate == NotPredicate TruePredicate then do
                                    attributesRel <- typeForRelationalExpr expr
                                    case attributesRel of 
                                      Left err -> return $ Left err
                                      Right attributesRelA -> return $ Right $ MakeStaticRelation (attributes attributesRelA) emptyTupleSet
                                      else do
                                      optimizedSubExpression <- applyStaticRelationalOptimization expr
                                      case optimizedSubExpression of
                                        Left err -> return $ Left err
                                        Right optSubExpr -> return $ Right $ Restrict optimizedPredicate optSubExpr
  
applyStaticRelationalOptimization (Equals exprA exprB) = do 
  return $ Right $ Equals exprA exprB

applyStaticDatabaseOptimization :: DatabaseExpr -> DatabaseState (Either RelationalError DatabaseExpr)
applyStaticDatabaseOptimization (Define name attrs) = return $ Right (Define name attrs)

applyStaticDatabaseOptimization (Undefine name) = return $ Right (Undefine name)

applyStaticDatabaseOptimization (Assign name expr) = do
  optimizedExpr <- applyStaticRelationalOptimization expr
  case optimizedExpr of
    Left err -> return $ Left err
    Right optimizedExpr -> return $ Right (Assign name optimizedExpr)
    
applyStaticDatabaseOptimization (Insert name expr) = do
  optimizedExpr <- applyStaticRelationalOptimization expr
  case optimizedExpr of
    Left err -> return $ Left err
    Right optimizedExpr -> return $ Right (Insert name optimizedExpr)
  
applyStaticDatabaseOptimization (Delete name predicate) = do  
  optimizedPredicate <- applyStaticPredicateOptimization predicate
  case optimizedPredicate of
      Left err -> return $ Left err
      Right optimizedPredicate -> return $ Right (Delete name optimizedPredicate)

applyStaticDatabaseOptimization (Update name map predicate) = do 
  optimizedPredicate <- applyStaticPredicateOptimization predicate
  case optimizedPredicate of
      Left err -> return $ Left err
      Right optimizedPredicate -> return $ Right (Update name map optimizedPredicate)
      
applyStaticDatabaseOptimization (AddInclusionDependency dep) = return $ Right (AddInclusionDependency dep)

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
                                    evalContextExpr expr    
                                    applyStaticDatabaseOptimization expr
  --this error handling could be improved with some lifting presumably
  --restore original context


applyStaticPredicateOptimization :: RestrictionPredicateExpr -> DatabaseState (Either RelationalError RestrictionPredicateExpr)
applyStaticPredicateOptimization predicate = return $ Right predicate