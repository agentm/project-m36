module ProjectM36.DatabaseContextFunctionUtils where
import ProjectM36.RelationalExpression
import ProjectM36.Base
import ProjectM36.DatabaseContextFunctionError
import ProjectM36.Error
import ProjectM36.TransactionGraph.Types
import ProjectM36.DatabaseContext.Types
import ProjectM36.StaticOptimizer

executeDatabaseContextExpr :: DatabaseContextExpr' -> TransactionId -> TransactionGraph -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext
executeDatabaseContextExpr expr transId graph context' =
  case run of
    Right st -> pure (dbc_context st)
    Left err -> error (show err)
  where
    env = mkDatabaseContextEvalEnv transId graph
    run = runDatabaseContextEvalMonad context' env (optimizeAndEvalDatabaseContextExpr True expr)
  
executeRelationalExpr :: RelationalExpr -> DatabaseContext -> TransactionGraph -> Either RelationalError Relation
executeRelationalExpr expr context graph =
  run
  where
    env = mkRelationalExprEnv context graph
    run = optimizeAndEvalRelationalExpr env expr
