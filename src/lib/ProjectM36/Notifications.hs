module ProjectM36.Notifications where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph.Types
import ProjectM36.DatabaseContext.Types
import ProjectM36.StaticOptimizer
import qualified Data.Map as M
import Data.Either (isRight)

-- | Returns the notifications which should be triggered based on the transition from the first 'DatabaseContext' to the second 'DatabaseContext'.

-- TODO fix to use new optimizer (relies on IO)
notificationChanges :: Notifications -> TransactionGraph -> DatabaseContext -> DatabaseContext -> Notifications
notificationChanges nots graph context1 context2 = M.filter notificationFilter nots
  where
    notificationFilter (Notification chExpr _ _) = oldChangeEval /= newChangeEval && isRight oldChangeEval
      where
        oldChangeEval = evalChangeExpr chExpr (mkRelationalExprEnv context1 graph)
        newChangeEval = evalChangeExpr chExpr (mkRelationalExprEnv context2 graph)

    evalChangeExpr :: RelationalExpr -> RelationalExprEnv -> Either RelationalError Relation                                     
    evalChangeExpr chExpr env =
      optimizeAndEvalRelationalExpr env chExpr 

    
