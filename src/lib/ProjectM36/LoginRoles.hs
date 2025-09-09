-- | Provides a simple database mapping login names to role IDs (UUIDs). Only role names which map to a role ID are allowed to log in.
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module ProjectM36.LoginRoles where
import qualified Database.SQLite.Simple as SQL
import Data.UUID (UUID, fromString)
import Data.Text (Text)
import Data.Maybe

type RoleId = UUID
type RoleName = Text

type LoginRolesDB = SQL.Connection

openNoPersistence :: IO SQL.Connection
openNoPersistence = open ":memory:"

open :: String -> IO SQL.Connection
open openText = do
  db <- SQL.open openText
  setupDatabaseIfNecessary db
  pure db

setupDatabaseIfNecessary :: SQL.Connection -> IO ()
setupDatabaseIfNecessary db = do
  SQL.withTransaction db $ do
    res <- SQL.query_ @(SQL.Only Int) db "SELECT 1 FROM sqlite_master WHERE type='table' AND name='login_role_names'"
    case res of
      [SQL.Only _x] -> pure ()
      _ -> do
        SQL.execute_ db "CREATE TABLE login_role_name(rolename TEXT PRIMARY KEY, roleid TEXT NOT NULL)"        
        SQL.execute_ db "INSERT INTO login_role_name(rolename, roleid) VALUES ('admin','00000000-0000-0000-0000-000000000000')"
        SQL.execute_ db "CREATE TABLE login_role_member (roleid TEXT NOT NULL, memberof TEXT NOT NULL)"
        
      
  

-- | Test for a role name to role ID mapping. If a role id is found, the role is allowed to login to the database.
roleIdsForRoleName :: RoleName -> SQL.Connection -> IO [RoleId]
roleIdsForRoleName roleName db = do
  --with recursive query to extract all roles
  let q = "with cte as (select NULL as roleid,roleid as memberof FROM login_role_name WHERE rolename=? " <>
          "union all " <>
          "select lrm.roleid,lrm.memberof FROM login_role_member as lrm JOIN cte AS c ON c.memberof = lrm.roleid) " <>
        "select memberof from cte;"
  res <- SQL.query db q (SQL.Only roleName)
  pure (map (\x -> case fromString (SQL.fromOnly x) of
                     Nothing -> error ("invalid uuid: " <> show (SQL.fromOnly x))
                     Just val -> val) res)

    

-- | Add or update a role name mapped to a role ID.
setRole :: RoleName -> RoleId -> SQL.Connection -> IO ()
setRole roleName roleId db = do
  SQL.withTransaction db $ do
    SQL.execute db "DELETE FROM login_roles WHERE rolename=?" (SQL.Only roleName)
    SQL.execute db "INSERT INTO login_roles(rolename, roleid) VALUES (?,?)" (roleName, show roleId)

-- | Remove a role. Returns True if the role was present and was removed.
removeRole :: RoleName -> SQL.Connection -> IO Bool
removeRole rolename db = do
  SQL.execute db "DELETE FROM login_roles WHERE rolename = ?" (SQL.Only rolename)
  c <- SQL.changes db
  pure (c > 0)

allRoles :: SQL.Connection -> IO [(RoleName, RoleId)]
allRoles db = do
  res <- SQL.query_ db "SELECT rolename,roleid from login_roles"
  pure (map (\(rn, rid) -> (rn, fromMaybe (error "invalid uuid in login_roles") (fromString rid))) res)

close :: SQL.Connection -> IO ()
close db = SQL.close db
