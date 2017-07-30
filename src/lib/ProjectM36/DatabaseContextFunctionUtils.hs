module ProjectM36.DatabaseContextFunctionUtils where
import ProjectM36.RelationalExpression
import ProjectM36.Base
import ProjectM36.DatabaseContextFunctionError
import ProjectM36.Error
import Control.Monad.State
import Control.Monad.Trans.Reader

executeDatabaseContextExpr :: DatabaseContextExpr -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext
executeDatabaseContextExpr expr context = case runState (evalDatabaseContextExpr expr) (freshDatabaseState context) of
  (Nothing, (context', _, _)) -> pure context'
  (Just err, _) -> error (show err)
  
executeRelationalExpr :: RelationalExpr -> DatabaseContext -> Either RelationalError Relation
executeRelationalExpr expr context = runReader (evalRelationalExpr expr) (mkRelationalExprState context)
