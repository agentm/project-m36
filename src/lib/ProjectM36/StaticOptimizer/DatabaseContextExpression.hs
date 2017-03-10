module ProjectM36.StaticOptimizer.DatabaseContextExpression where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.DatabaseContext
import ProjectM36.DatabaseContextState
import ProjectM36.DatabaseContextExpression
import ProjectM36.RelationalExpressionState
import ProjectM36.StaticOptimizer.RelationalExpression
import Control.Monad.State
import Data.Either

applyStaticDatabaseOptimization :: DatabaseContextExpr -> DatabaseState (Either RelationalError DatabaseContextExpr)
applyStaticDatabaseOptimization x@NoOperation = pure $ Right x
applyStaticDatabaseOptimization x@(Define _ _) = pure $ Right x

applyStaticDatabaseOptimization x@(Undefine _) = pure $ Right x

applyStaticDatabaseOptimization (Assign name expr) = do
  context <- getDatabaseContext
  let optimizedExpr = evalState (applyStaticRelationalOptimization expr) (RelationalExprStateElems context)
  case optimizedExpr of
    Left err -> return $ Left err
    Right optimizedExpr2 -> return $ Right (Assign name optimizedExpr2)
    
applyStaticDatabaseOptimization (Insert name expr) = do
  context <- getDatabaseContext
  let optimizedExpr = evalState (applyStaticRelationalOptimization expr) (RelationalExprStateElems context)
  case optimizedExpr of
    Left err -> return $ Left err
    Right optimizedExpr2 -> return $ Right (Insert name optimizedExpr2)
  
applyStaticDatabaseOptimization (Delete name predicate) = do  
  context <- getDatabaseContext
  let optimizedPredicate = evalState (applyStaticPredicateOptimization predicate) (RelationalExprStateElems context)
  case optimizedPredicate of
      Left err -> return $ Left err
      Right optimizedPredicate2 -> return $ Right (Delete name optimizedPredicate2)

applyStaticDatabaseOptimization (Update name upmap predicate) = do 
  context <- getDatabaseContext
  let optimizedPredicate = evalState (applyStaticPredicateOptimization predicate) (RelationalExprStateElems context)
  case optimizedPredicate of
      Left err -> return $ Left err
      Right optimizedPredicate2 -> return $ Right (Update name upmap optimizedPredicate2)
      
applyStaticDatabaseOptimization dep@(AddInclusionDependency _ _) = return $ Right dep

applyStaticDatabaseOptimization (RemoveInclusionDependency name) = return $ Right (RemoveInclusionDependency name)

applyStaticDatabaseOptimization (AddNotification name triggerExpr resultExpr) = do
  context <- getDatabaseContext
  let eTriggerExprOpt = evalState (applyStaticRelationalOptimization triggerExpr) (RelationalExprStateElems context)
  case eTriggerExprOpt of
         Left err -> pure $ Left err
         Right triggerExprOpt -> do
           let eResultExprOpt = evalState (applyStaticRelationalOptimization resultExpr) (RelationalExprStateElems context)
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
  dbstate <- get
  context <- getDatabaseContext
  let optExprs = evalState substateRunner (dbstate { dbcontext = contextWithEmptyTupleSets context }) 
  let errors = lefts optExprs
  if length errors > 0 then
    return $ Left (head errors)
    else
      return $ Right $ MultipleExpr (rights optExprs)
   where
     substateRunner = forM exprs $ \expr -> do
                                    --a previous expression could create a relvar, we don't want to miss it, so we clear the tuples and execute the expression to get an empty relation in the relvar                                
       _ <- evalDatabaseContextExpr expr    
       applyStaticDatabaseOptimization expr
  --this error handling could be improved with some lifting presumably
  --restore original context

