module ProjectM36.DatabaseContextFunctionUtils where
import ProjectM36.RelationalExpression
import ProjectM36.Base
import ProjectM36.DatabaseContextFunctionError
import ProjectM36.Error
import Control.Monad.State
import Control.Monad.Trans.Reader

executeDatabaseContextExpr :: DatabaseContextExpr -> TransactionId -> TransactionGraph -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext
executeDatabaseContextExpr expr tid graph context' = case runState (evalDatabaseContextExpr expr tid graph) (freshDatabaseState context') of
  (Right (), st) -> pure (context st)
  (Left err, _) -> error (show err)
  
executeRelationalExpr :: RelationalExpr -> DatabaseContext -> TransactionId -> TransactionGraph -> Either RelationalError Relation
executeRelationalExpr expr context tid graph = do
  gfExpr <- runReader (evalRelationalExpr expr) (mkRelationalExprState context tid graph)
  evalGraphRefRelationalExpr gfExpr graph
