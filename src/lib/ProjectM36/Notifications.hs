module ProjectM36.Notifications where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.StaticOptimizer
import Control.Monad.Trans.Reader
import qualified Data.Map as M
import Data.Either (isRight)

-- | Returns the notifications which should be triggered based on the transition from the first 'DatabaseContext' to the second 'DatabaseContext'.
notificationChanges :: Notifications -> TransactionId -> TransactionGraph -> DatabaseContext -> DatabaseContext -> Notifications
notificationChanges nots tid graph context1 context2 = M.filter notificationFilter nots
  where
    notificationFilter (Notification chExpr _ _) = oldChangeEval /= newChangeEval && isRight oldChangeEval
      where
        oldChangeEval = evalChangeExpr chExpr (mkRelationalExprEnv context1 tid graph)
        newChangeEval = evalChangeExpr chExpr (mkRelationalExprEnv context2 tid graph)

    evalChangeExpr :: RelationalExpr -> RelationalExprEnv -> Either RelationalError Relation                                     
    evalChangeExpr chExpr env =
      optimizeAndEvalRelationalExpr env chExpr 

    
