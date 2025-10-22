module ProjectM36.DatabaseContextExpr where
import ProjectM36.DatabaseContext.Types
import ProjectM36.DatabaseContext
import ProjectM36.Base
import ProjectM36.AccessControlList
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph.Types
import qualified Data.Map as M
import Data.Functor.Identity
import qualified Data.Functor.Foldable as Fold

resolvedDatabaseContextAsDatabaseContextExpr :: ResolvedDatabaseContext -> DatabaseContextExpr
resolvedDatabaseContextAsDatabaseContextExpr context = do
  let relVarsExprs = map (\(name, rel) -> Assign name (stripGraphRefRelationalExpr rel)) (M.toList relVars)
      relVars = runIdentity (relationVariables context)
      incDeps = runIdentity (inclusionDependencies context)
      incDepsExprs = map (uncurry AddInclusionDependency) (M.toList incDeps)
      funcsExprs = []
  MultipleExpr $ relVarsExprs ++ incDepsExprs ++ funcsExprs
  
-- | convert an existing database context into its constituent expression.   
databaseContextAsDatabaseContextExpr :: DatabaseContext -> TransactionGraph -> Either RelationalError DatabaseContextExpr
databaseContextAsDatabaseContextExpr context graph = do
  relVars <- resolveDBC' graph context relationVariables
  incDeps <- resolveDBC' graph context inclusionDependencies
  let relVarsExprs = map (\(name, rel) -> Assign name (stripGraphRefRelationalExpr rel)) (M.toList relVars)
      incDepsExprs = map (uncurry AddInclusionDependency) (M.toList incDeps)
      funcsExprs = []
  pure $ MultipleExpr $ relVarsExprs ++ incDepsExprs ++ funcsExprs

resolveRoleIds :: (RoleName -> Maybe RoleId) -> DatabaseContextExprBase a RoleName -> Either RelationalError (DatabaseContextExprBase a RoleId)
resolveRoleIds resolver expr = do
  case expr of
    NoOperation -> pure NoOperation
    Define rv exprs -> pure (Define rv exprs)
    Undefine rv -> pure (Undefine rv)
    Assign rv expr' -> pure (Assign rv expr')
    Insert rv expr' -> pure (Insert rv expr')
    Delete rv expr' -> pure (Delete rv expr')
    Update rv atoms expr' -> pure (Update rv atoms expr')
    AddInclusionDependency iname idep -> pure (AddInclusionDependency iname idep)
    RemoveInclusionDependency iname -> pure (RemoveInclusionDependency iname)
    AddNotification notName exprA exprB exprC -> pure (AddNotification notName exprA exprB exprC)
    RemoveNotification notName -> pure (RemoveNotification notName)
    AddTypeConstructor tDef dDefs -> pure (AddTypeConstructor tDef dDefs)
    RemoveTypeConstructor tName -> pure (RemoveTypeConstructor tName)
    RemoveAtomFunction fName -> pure (RemoveAtomFunction fName)
    RemoveDatabaseContextFunction fname -> pure (RemoveDatabaseContextFunction fname)
    ExecuteDatabaseContextFunction fname exprs -> pure (ExecuteDatabaseContextFunction fname exprs)
    AddRegisteredQuery qName expr' -> pure (AddRegisteredQuery qName expr')
    RemoveRegisteredQuery qName -> pure (RemoveRegisteredQuery qName)
    AlterACL expr' -> AlterACL <$> resolveRoleIdsDBCACLExpr resolver expr'
    MultipleExpr exprs -> MultipleExpr <$> mapM (resolveRoleIds resolver) exprs

resolveRoleNames :: (RoleId -> Maybe RoleName) -> DatabaseContextExprBase a RoleId -> Either RelationalError (DatabaseContextExprBase a RoleName)
resolveRoleNames resolver expr = do
  case expr of
    NoOperation -> pure NoOperation
    Define rv exprs -> pure (Define rv exprs)
    Undefine rv -> pure (Undefine rv)
    Assign rv expr' -> pure (Assign rv expr')
    Insert rv expr' -> pure (Insert rv expr')
    Delete rv expr' -> pure (Delete rv expr')
    Update rv atoms expr' -> pure (Update rv atoms expr')
    AddInclusionDependency iname idep -> pure (AddInclusionDependency iname idep)
    RemoveInclusionDependency iname -> pure (RemoveInclusionDependency iname)
    AddNotification notName exprA exprB exprC -> pure (AddNotification notName exprA exprB exprC)
    RemoveNotification notName -> pure (RemoveNotification notName)
    AddTypeConstructor tDef dDefs -> pure (AddTypeConstructor tDef dDefs)
    RemoveTypeConstructor tName -> pure (RemoveTypeConstructor tName)
    RemoveAtomFunction fName -> pure (RemoveAtomFunction fName)
    RemoveDatabaseContextFunction fname -> pure (RemoveDatabaseContextFunction fname)
    ExecuteDatabaseContextFunction fname exprs -> pure (ExecuteDatabaseContextFunction fname exprs)
    AddRegisteredQuery qName expr' -> pure (AddRegisteredQuery qName expr')
    RemoveRegisteredQuery qName -> pure (RemoveRegisteredQuery qName)
    AlterACL alterACLExpr -> AlterACL <$> resolveRoleNamesDBCACLExpr resolver alterACLExpr
    MultipleExpr exprs -> MultipleExpr <$> mapM (resolveRoleNames resolver) exprs

resolveRoleIdsDBCACLExpr :: (RoleName -> Maybe RoleId) -> AlterDBCACLExprBase RoleName -> Either RelationalError (AlterDBCACLExprBase RoleId)
resolveRoleIdsDBCACLExpr resolver =
  recurseRoles resolver'
  where
    resolver' roleName = maybe (Left (NoSuchRoleNameError roleName)) pure (resolver roleName)

-- | convert role ids into role names or vice versa
recurseRoles :: (a -> Either RelationalError b) -> AlterDBCACLExprBase a -> Either RelationalError (AlterDBCACLExprBase b)
recurseRoles resolver =
  Fold.cataA recurse
  where
--    recurse :: AlterDBCACLExprBaseF RoleId (Either RelationalError (AlterDBCACLExprBase RoleName)) -> Either RelationalError (AlterDBCACLExprBase RoleName)
    recurse (GrantAccessExprF roleInfo perm mgrant) = do
      roleOut <- resolver roleInfo
      pure (GrantAccessExpr roleOut perm mgrant)
    recurse (RevokeAccessExprF roleInfo perm) = do
      roleOut <- resolver roleInfo
      pure (RevokeAccessExpr roleOut perm)
    recurse (GrantDBCFunctionAccessExprF roleInfo funcName' perm mgrant) = do
      roleOut <- resolver roleInfo
      pure (GrantDBCFunctionAccessExpr roleOut funcName' perm mgrant)
    recurse (RevokeDBCFunctionAccessExprF roleInfo funcName' perm) = do
      roleOut <- resolver roleInfo
      pure (RevokeDBCFunctionAccessExpr roleOut funcName' perm)

resolveRoleNamesDBCACLExpr :: (RoleId -> Maybe RoleName) -> AlterDBCACLExprBase RoleId -> Either RelationalError (AlterDBCACLExprBase RoleName)
resolveRoleNamesDBCACLExpr resolver = recurseRoles resolver'
  where
    resolver' roleId = maybe (Left NoSuchRoleIdError) pure (resolver roleId)
  

