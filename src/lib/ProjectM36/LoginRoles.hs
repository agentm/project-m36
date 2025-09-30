-- | Provides a simple database mapping login names to role IDs (UUIDs). Only role names which map to a role ID are allowed to log in.
{-# LANGUAGE ScopedTypeVariables, TypeApplications, DeriveGeneric #-}
module ProjectM36.LoginRoles where
import qualified Database.SQLite.Simple as SQL
import Data.UUID (UUID, fromString, toText, fromText)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text)
import Data.Maybe
import GHC.Generics
import ProjectM36.Base
import qualified Data.Text as T
import Control.Monad (forM_)

type RoleId = UUID

type LoginRolesDB = SQL.Connection

type MayGrant = Bool

type MayLogin = Bool

-- | Used by console programs to change this database of roles which are allowed to login.
data AlterLoginRolesExpr = ShowRolesForRoleExpr RoleName | -- ^ show roles available to the given role
                           AddLoginRoleExpr RoleName MayLogin | -- ^ add a login role, requires admin role
                           RemoveLoginRoleExpr RoleName | -- ^ remove a login role, requires admin role
                           AddRoleToRoleExpr RoleName RoleName MayGrant | -- ^ add a role to an existing role
                           RemoveRoleFromRoleExpr RoleName RoleName | -- ^ remove a role from an existing role
                           AddPermissionToRoleExpr RoleName Permission MayGrant |
                           RemovePermissionFromRoleExpr RoleName Permission |
                           ShowAllRolesExpr -- ^ display all available roles
                           deriving (Eq, Generic, Show)

data LoginRoleError = PermissionDeniedError |
                      NoSuchRoleNameError RoleName |
                      InvalidRoleIdError Text |
                      NoSuchPermissionError Permission |
                      RoleNameAlreadyExistsError RoleName |
                      RoleNameConflictError RoleName
                      deriving (Eq, Generic, Show)

viewLoginRolesPerm :: Permission
viewLoginRolesPerm = "view_login_roles"

alterLoginRolesPerm :: Permission
alterLoginRolesPerm = "alter_login_roles"

openNoPersistence :: IO LoginRolesDB
openNoPersistence = open ":memory:"

open :: String -> IO LoginRolesDB
open openText = do
  db <- SQL.open openText
  setupDatabaseIfNecessary db
  pure db

adminRoleName :: RoleName  
adminRoleName = "admin"

withTransaction :: LoginRolesDB -> IO a -> IO a
withTransaction = SQL.withTransaction

setupDatabaseIfNecessary :: LoginRolesDB -> IO ()
setupDatabaseIfNecessary db = do
  adminRoleId <- nextRandom
  SQL.execute_ db "PRAGMA foreign_keys = ON;"
  SQL.withTransaction db $ do
    res <- SQL.query_ @(SQL.Only Int) db "SELECT 1 FROM sqlite_master WHERE type='table' AND name='login_role_name'"
    case res of
      [SQL.Only _x] -> pure ()
      _ -> do
        let dbsetup =
              ["CREATE TABLE login_role_name(rolename TEXT PRIMARY KEY, roleid TEXT NOT NULL UNIQUE, maylogin BOOLEAN NOT NULL, UNIQUE(rolename), UNIQUE(roleid))",
                "CREATE TABLE permission(name TEXT PRIMARY KEY)",
                "CREATE TABLE login_role_permission(roleid TEXT NOT NULL REFERENCES login_role_name(roleid), permission TEXT NOT NULL REFERENCES permission(name), may_grant BOOLEAN NOT NULL, UNIQUE(roleid, permission, may_grant))",
                "CREATE TABLE login_role_member (roleid TEXT NOT NULL REFERENCES login_role_name(roleid), memberof TEXT NOT NULL, maygrant BOOLEAN NOT NULL, UNIQUE(roleid, memberof))",
              -- create login permissions
              "INSERT INTO permission(name) VALUES ('login'),('alter_login_roles'),('view_login_roles')"]
        forM_ dbsetup $ \sql ->
          SQL.execute_ db sql
        -- add admin role
        SQL.execute db "INSERT INTO login_role_name(rolename, roleid, maylogin) VALUES ('admin',?, true)" (SQL.Only (toText adminRoleId))
        -- grant full permission to admin role
        SQL.execute db "INSERT INTO login_role_permission(roleid, permission,may_grant) VALUES (?,'login',true), (?, 'alter_login_roles',true), (?, 'view_login_roles',true)" (toText adminRoleId, toText adminRoleId, toText adminRoleId)

-- | Determine if the role name has the privilege to login to the database. Some roles are prohibited from logging in.
roleNameMayLogin :: RoleName -> LoginRolesDB -> IO (Either LoginRoleError Bool)
roleNameMayLogin roleName db = do
  res <- SQL.query db "SELECT maylogin FROM login_role_name WHERE rolename = ?" (SQL.Only roleName)
  case res of
    [val] -> pure (Right (SQL.fromOnly val))
    _ -> pure (Left (NoSuchRoleNameError roleName))
  
-- | Test for a role name to role ID mapping. If a role id is found, the role is allowed to login to the database.
roleIdsForRoleName :: RoleName -> LoginRolesDB -> IO (Either LoginRoleError [RoleId])
roleIdsForRoleName roleName db = do
  --with recursive query to extract all roles
  let q = "with recursive cte as (select NULL as roleid,roleid as memberof FROM login_role_name WHERE rolename=? union all select lrm.roleid,lrm.memberof FROM login_role_member as lrm JOIN cte AS c ON c.memberof = lrm.roleid) select memberof from cte;"
  res <- SQL.query db q (SQL.Only roleName)
  pure (Right (map (\x -> case fromString (SQL.fromOnly x) of
                     Nothing -> error "invalid uuid"
                     Just val -> val) res))

roleNamesForRoleName :: RoleName -> LoginRolesDB -> IO (Either LoginRoleError [(RoleName, MayGrant)])
roleNamesForRoleName roleName db = do
  let q = "with recursive cte as (select lrm.roleid,lrm.memberof,lrm.maygrant FROM login_role_member as lrm WHERE roleid=(SELECT roleid FROM login_role_name WHERE rolename=?) union all select lrm.roleid,lrm.memberof,lrm.maygrant FROM login_role_member as lrm JOIN cte AS c ON c.memberof = lrm.roleid) select lrn.rolename,cte.maygrant from cte join login_role_name AS lrn ON lrn.roleid = cte.memberof union all select rolename,false as maygrant FROM login_role_name WHERE rolename=?;"
  eTargetId <- roleIdForRoleName roleName db
  case eTargetId of
    Left err -> pure (Left err)
    Right _ -> do
      roleNamesAndGrants <- SQL.query db q (roleName, roleName)
      pure (Right roleNamesAndGrants)

permissionsForRoleName :: RoleName -> LoginRolesDB -> IO (Either LoginRoleError [Permission])
permissionsForRoleName roleName db = do
  -- collect all roles for the role name
  -- then collect all the permissions for those roles  
  let q = "with recursive cte as (select NULL as roleid,roleid as memberof FROM login_role_name WHERE rolename=? union all select lrm.roleid,lrm.memberof FROM login_role_member as lrm JOIN cte AS c ON c.memberof = lrm.roleid) select permission from login_role_permission WHERE roleid IN (SELECT memberof FROM cte);"
  perms <- SQL.query db q (SQL.Only roleName)
  pure (Right (map SQL.fromOnly perms))
  
roleHasPermission :: RoleName -> Permission -> LoginRolesDB -> IO (Either LoginRoleError Bool)
roleHasPermission roleName perm db = do
  eTargetId <- roleIdForRoleName roleName db
  case eTargetId of
    Left err -> pure (Left err)
    Right _ -> do
      let q = "with recursive cte as (select NULL as roleid,roleid as memberof FROM login_role_name WHERE rolename=? union all select lrm.roleid,lrm.memberof FROM login_role_member as lrm JOIN cte AS c ON c.memberof = lrm.roleid),perms AS (select permission from login_role_permission WHERE roleid IN (SELECT memberof FROM cte)) SELECT 1 FROM perms WHERE permission = ?;"
      perms <- SQL.query @_ @(SQL.Only Int) db q (roleName, perm)
      pure $ case perms of
        [SQL.Only 1] -> Right True
        _ -> Right False

-- | Add new role name to the database with optional login privilege.
addRoleName :: RoleName -> RoleId -> MayLogin -> LoginRolesDB -> IO (Either LoginRoleError ())
addRoleName roleName newRoleId maylogin db = do
  SQL.execute db "INSERT INTO login_role_name(rolename, roleid, maylogin) VALUES (?,?,?) ON CONFLICT(rolename) DO NOTHING" (roleName, show newRoleId, maylogin)
  c <- SQL.changes db
  if c > 0 then
    pure (Right ())
    else
    pure (Left (RoleNameAlreadyExistsError roleName))

-- | Grant role access to other role.
addRoleToRoleName :: RoleName -> RoleName -> MayGrant -> LoginRolesDB -> IO (Either LoginRoleError ())
addRoleToRoleName roleNameTarget addToRoleName mayGrant db = do
  -- TODO: prevent infinite loops in roles
    eTargetId <- roleIdForRoleName roleNameTarget db
    eAddId <- roleIdForRoleName addToRoleName db
    case eTargetId of
      Left err -> pure (Left err)
      Right targetId ->
        case eAddId of
          Left err -> pure (Left err)
          Right addId -> do
            let q = "INSERT INTO login_role_member(roleid,memberof,maygrant) VALUES (?,?,?)-- ON CONFLICT DO NOTHING"
            SQL.execute db q (toText targetId, toText addId, mayGrant)
            c <- SQL.changes db
            if c == 0 then
              pure (Left (RoleNameConflictError addToRoleName))
              else
              pure (Right ())

removeRoleFromRoleName :: RoleName -> RoleName -> LoginRolesDB -> IO (Either LoginRoleError ())
removeRoleFromRoleName roleNameTarget fromRole db = do
  eTargetId <- roleIdForRoleName roleNameTarget db
  eDelId <- roleIdForRoleName fromRole db
  case eTargetId of
    Left err -> pure (Left err)
    Right targetId ->
      case eDelId of
        Left err -> pure (Left err)
        Right delId -> do
          let q = "DELETE FROM login_role_member WHERE roleid=? AND memberof=?"
          SQL.execute db q (toText targetId, toText delId)
          c <- SQL.changes db 
          if c > 0 then
            pure (Right ())
            else
            pure (Left (NoSuchRoleNameError roleNameTarget))

addPermissionToRoleName :: RoleName -> Permission -> Bool -> LoginRolesDB -> IO (Either LoginRoleError ())
addPermissionToRoleName roleName perm mayGrant db = do
  eTargetId <- roleIdForRoleName roleName db
  case eTargetId of
    Left err -> pure (Left err)
    Right targetId -> do
      let q = "INSERT INTO login_role_permission(roleid,permission,may_grant) VALUES (?,?,?)"
      SQL.execute db q (toText targetId, perm, mayGrant)
      pure (Right ())

-- | Whether or not the granting role has permission to grant the role. To whom is irrelevant.
roleMayGrantRole :: RoleName -> RoleName -> LoginRolesDB -> IO (Either LoginRoleError Bool)
roleMayGrantRole grantingRole roleToGrant db = do
  eroles <- roleNamesForRoleName grantingRole db
  case eroles of
    Left err -> pure (Left err)
    Right roles -> do
      let matches = filter (\(role', maygrant) -> role' == roleToGrant && maygrant) roles
      pure (Right (not (null matches)))

removePermissionFromRoleName :: RoleName -> Permission -> LoginRolesDB -> IO (Either LoginRoleError ())
removePermissionFromRoleName roleName perm db = do
  eTargetId <- roleIdForRoleName roleName db  
  case eTargetId of
    Left err -> pure (Left err)
    Right targetId -> do
      let q = "DELETE FROM login_role_permission WHERE roleid=? AND permission=?"
      SQL.execute db q (toText targetId, perm)
      pure (Right ())
      
-- | Return the primary key for the given role name or Nothing.
roleIdForRoleName :: RoleName -> LoginRolesDB -> IO (Either LoginRoleError RoleId)
roleIdForRoleName rolename db = do
  let q = "SELECT roleid FROM login_role_name where rolename=?"
  res <- SQL.query db q (SQL.Only rolename)
  case res of
   [SQL.Only roleId] ->
     case fromText roleId of
       Nothing -> pure (Left (InvalidRoleIdError roleId))
       Just roleuuid -> pure (Right roleuuid)
   _ -> pure (Left (NoSuchRoleNameError rolename))

-- | Remove a role. Returns True if the role was present and was removed.
removeRole :: RoleName -> LoginRolesDB -> IO (Either LoginRoleError ())
removeRole rolename db = do
  SQL.execute db "DELETE FROM login_role_name WHERE rolename = ?" (SQL.Only rolename)
  c <- SQL.changes db
  if c > 0 then
    pure (Right ())
    else
    pure (Left (NoSuchRoleNameError rolename))

allRoles :: LoginRolesDB -> IO [(RoleName, RoleId)]
allRoles db = do
  res <- SQL.query_ db "SELECT rolename,roleid from login_role_name"
  pure (map (\(rn, rid) -> (rn, fromMaybe (error "invalid uuid in login_role_name") (fromString rid))) res)

close :: LoginRolesDB -> IO ()
close db = SQL.close db

executeAlterLoginRolesExpr :: RoleName -> LoginRolesDB -> AlterLoginRolesExpr -> IO (Either LoginRoleError Text)
executeAlterLoginRolesExpr currentRole db expr = do
  let ok = pure (Right "ok")
      checkPerm expr' =
        case expr' of
          ShowRolesForRoleExpr roleName ->
            if currentRole == roleName then
              pure (Right True)
            else
              hasPerm viewLoginRolesPerm
          AddLoginRoleExpr{} -> hasPerm alterLoginRolesPerm
          RemoveLoginRoleExpr{} -> hasPerm alterLoginRolesPerm
          AddRoleToRoleExpr _target other _mayGrant -> do
            eperm <- hasPerm alterLoginRolesPerm
            case eperm of
              Left err -> pure (Left err)
              Right True -> pure (Right True)
              Right False -> do
                res <- roleMayGrantRole currentRole other db
                pure res
          RemoveRoleFromRoleExpr _target other ->
            roleMayGrantRole currentRole other db
          AddPermissionToRoleExpr{} -> hasPerm alterLoginRolesPerm
          RemovePermissionFromRoleExpr{} -> hasPerm alterLoginRolesPerm
          ShowAllRolesExpr{} -> hasPerm viewLoginRolesPerm
      hasPerm perm = do
        roleHasPermission currentRole perm db

  SQL.withTransaction db $ do
    check <- checkPerm expr
    case check of
      Left err -> pure (Left err)
      Right False -> pure (Left PermissionDeniedError)
      Right True -> do
        case expr of
          ShowRolesForRoleExpr roleName -> do
            eRoles <- roleNamesForRoleName roleName db
            case eRoles of
              Left err ->
                pure (Left err)
              Right roles ->
                pure (Right (T.intercalate "\n" (map (\(r,g) -> r <> if g then ":maygrant" else "") roles)))
          AddLoginRoleExpr roleName maylogin -> do
            newRoleId <- nextRandom
            result <- addRoleName roleName newRoleId maylogin db
            case result of
              Left err -> pure (Left err)
              Right () -> ok
          ShowAllRolesExpr -> do
            roleInfos <- allRoles db
            pure (Right (T.intercalate "\n" (map (\(roleName, roleId) -> T.pack (show roleId) <> ":" <> roleName) roleInfos)))
          RemoveLoginRoleExpr roleName -> do
            result <- removeRole roleName db
            case result of
              Left err -> pure (Left err)
              Right () -> ok
          AddRoleToRoleExpr targetRole otherRole mayGrant -> do
            -- who can do this? the current user granting his permission or alter_login_roles perm
            res <- addRoleToRoleName targetRole otherRole mayGrant db
            case res of
              Left err -> pure (Left err)
              Right () -> ok
          RemoveRoleFromRoleExpr targetRole otherRole -> do
            res <- removeRoleFromRoleName targetRole otherRole db
            case res of
              Left err -> pure (Left err)
              Right () -> ok
          AddPermissionToRoleExpr targetRole perm mayGrant -> do
            res <- addPermissionToRoleName targetRole perm mayGrant db
            case res of
              Left err -> pure (Left err)
              Right () -> ok
          RemovePermissionFromRoleExpr targetRole perm -> do
            res <- removePermissionFromRoleName targetRole perm db
            case res of
              Left err -> pure (Left err)
              Right () -> ok

     
     
