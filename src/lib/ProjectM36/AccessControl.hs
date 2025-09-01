-- | Applies access control lists to the current role to determine whether should be granted
{-# LANGUAGE RankNTypes #-}
module ProjectM36.AccessControl where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AccessControlList
import ProjectM36.TransGraphRelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.RelationalExpression
import ProjectM36.DatabaseContext.Types
import ProjectM36.RelationVariablesMentioned
import ProjectM36.IsomorphicSchema
import Control.Monad.Except

applyACLRelationalExpr :: [RoleId] -> RelVarAccessControlList -> RelationalExprBase a -> Either RelationalError ()
applyACLRelationalExpr roleIds acl' expr = do
  let checkPerm perm =
        if hasAccess roleIds perm acl' then
          pure ()
          else
          Left (AccessDeniedError (SomeRelVarPermission perm))
  if mentionsRelVar expr then checkPerm AccessRelVarsPermission else pure ()

applyACLAlterTransGraphExpr :: [RoleId] -> AlterTransGraphAccessControlList -> AlterTransactionGraphExpr -> Either RelationalError ()
applyACLAlterTransGraphExpr roleIds acl' _alterExpr =
  if hasAccess roleIds CommitTransactionPermission acl' then
    pure ()
    else
    Left (AccessDeniedError (SomeAlterTransGraphPermission CommitTransactionPermission))

-- we should probably have finer-grained permission here
applyACLDatabaseContextIOExpr :: [RoleId] -> DatabaseContextIOExpr -> DatabaseContextIOEvalMonad ()
applyACLDatabaseContextIOExpr roleIds _expr = do
   acl' <- resolveIODBC acl  
   if hasAccess roleIds LoadFunctionPermission (dbcFunctionsACL acl') then
     pure ()
     else
     throwError (AccessDeniedError (SomeFunctionPermission LoadFunctionPermission))

applyACLDatabaseContextExpr :: [RoleId] -> DatabaseContextExpr -> DatabaseContextEvalMonad ()
applyACLDatabaseContextExpr roleIds expr = do
  dbcAcl <- resolveDBC acl
  let checkRVPerm perm acl' =
        if hasAccess roleIds perm acl' then
          pure ()
        else
          dbErr (AccessDeniedError (SomeRelVarPermission perm))
      checkFuncPerm perm acl' =
        if hasAccess roleIds perm acl' then
          pure ()
        else
          dbErr (AccessDeniedError (SomeFunctionPermission perm))
      rvAcl = relvarsACL dbcAcl
      dbcFuncAcl = dbcFunctionsACL dbcAcl
  case expr of
    NoOperation -> pure ()
    Define{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    Undefine{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    Assign{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    Insert{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    Delete{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    Update{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    AddInclusionDependency{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    RemoveInclusionDependency{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    AddNotification{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    RemoveNotification{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    AddTypeConstructor{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    RemoveTypeConstructor{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    RemoveAtomFunction{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    RemoveDatabaseContextFunction{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    ExecuteDatabaseContextFunction{} ->
      checkFuncPerm ExecuteFunctionPermission dbcFuncAcl
    AddRegisteredQuery{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    RemoveRegisteredQuery{} ->
      checkRVPerm AccessRelVarsPermission rvAcl
    MultipleExpr exprs ->
      mapM_ (applyACLDatabaseContextExpr roleIds) exprs

applyACLTransGraphRelationalExpr :: [RoleId] -> RelVarAccessControlList -> TransGraphRelationalExpr -> Either RelationalError ()
applyACLTransGraphRelationalExpr roleIds acl' expr =
  if mentionsRelVar expr then
    if hasAccess roleIds AccessRelVarsPermission acl' then
      pure ()
      else
      Left (AccessDeniedError (SomeRelVarPermission AccessRelVarsPermission))
  else
    pure ()

applyACLSchemaExpr :: [RoleId] -> SchemaAccessControlList -> SchemaExpr -> Either RelationalError ()
applyACLSchemaExpr roleIds acl' _expr =
  if hasAccess roleIds AlterSchemaPermission acl' then
    pure ()
    else
    Left (AccessDeniedError (SomeAlterSchemaPermission AlterSchemaPermission))
