-- | Applies access control lists to the current role to determine whether should be granted
{-# LANGUAGE RankNTypes #-}
module ProjectM36.AccessControl where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AccessControlList
import ProjectM36.RelationalExpression
import ProjectM36.DatabaseContext.Types

applyACLDatabaseContextExpr :: [RoleId] -> DatabaseContextExpr -> DatabaseContextEvalMonad ()
applyACLDatabaseContextExpr roleIds expr = do
  dbcAcl <- resolveDBC acl
  let checkPerm :: forall p. Eq p => p -> AccessControlList RoleId p -> DatabaseContextEvalMonad ()
      checkPerm perm acl' =
        if hasAccess roleIds perm acl' then
          pure ()
        else
          dbErr AccessDeniedError
      rvAcl = relvarsACL dbcAcl
      dbcFuncAcl = dbcFunctionsACL dbcAcl
          
  case expr of
    NoOperation -> pure ()
    Define{} ->
      checkPerm AccessRelVars rvAcl
    Undefine{} ->
      checkPerm AccessRelVars rvAcl
    Assign{} ->
      checkPerm AccessRelVars rvAcl
    Insert{} ->
      checkPerm AccessRelVars rvAcl
    Delete{} ->
      checkPerm AccessRelVars rvAcl
    Update{} ->
      checkPerm AccessRelVars rvAcl
    AddInclusionDependency{} ->
      checkPerm AccessRelVars rvAcl
    RemoveInclusionDependency{} ->
      checkPerm AccessRelVars rvAcl
    AddNotification{} ->
      checkPerm AccessRelVars rvAcl
    RemoveNotification{} ->
      checkPerm AccessRelVars rvAcl
    AddTypeConstructor{} ->
      checkPerm AccessRelVars rvAcl
    RemoveTypeConstructor{} ->
      checkPerm AccessRelVars rvAcl
    RemoveAtomFunction{} ->
      checkPerm AccessRelVars rvAcl
    RemoveDatabaseContextFunction{} ->
      checkPerm AccessRelVars rvAcl
    ExecuteDatabaseContextFunction{} ->
      checkPerm ExecuteFunctionPermission dbcFuncAcl
    AddRegisteredQuery{} ->
      checkPerm AccessRelVars rvAcl
    RemoveRegisteredQuery{} ->
      checkPerm AccessRelVars rvAcl
    MultipleExpr exprs ->
      mapM_ (applyACLDatabaseContextExpr roleIds) exprs
