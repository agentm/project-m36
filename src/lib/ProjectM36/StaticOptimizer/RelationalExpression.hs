module ProjectM36.StaticOptimizer.RelationalExpression where
import ProjectM36.Base
import ProjectM36.RelationalExpressionState
import ProjectM36.Relation
import ProjectM36.RelationalExpression
import ProjectM36.Error
import qualified ProjectM36.AttributeNames as AS
import ProjectM36.TupleSet

-- the static optimizer performs optimizations which need not take any specific-relation statistics into account
-- apply optimizations which merely remove steps to become no-ops: example: projection of a relation across all of its attributes => original relation

--should optimizations offer the possibility to return errors? If they perform the up-front type-checking, maybe so
applyStaticRelationalOptimization :: RelationalExpr -> RelationalExprState (Either RelationalError RelationalExpr)
applyStaticRelationalOptimization e@(MakeStaticRelation _ _) = return $ Right e

applyStaticRelationalOptimization e@(MakeRelationFromExprs _ _) = return $ Right e

applyStaticRelationalOptimization e@(ExistingRelation _) = return $ Right e

applyStaticRelationalOptimization e@(RelationVariable _ _) = return $ Right e

--remove project of attributes which removes no attributes
applyStaticRelationalOptimization (Project attrNameSet expr) = do
  relType <- typeForRelationalExpr expr
  case relType of
    Left err -> return $ Left err
    Right relType2 -> if AS.all == attrNameSet then
                        applyStaticRelationalOptimization expr
                      else if AttributeNames (attributeNames relType2) == attrNameSet then
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

                           
applyStaticPredicateOptimization :: RestrictionPredicateExpr -> RelationalExprState (Either RelationalError RestrictionPredicateExpr)
applyStaticPredicateOptimization predicate = return $ Right predicate    