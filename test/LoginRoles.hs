-- | Test correctness of the role-based authorization from LoginRoles.import Test.HUnit
import ProjectM36.LoginRoles as LR
import Test.HUnit
import System.Exit
import Data.UUID as UUID

main :: IO ()
main = do
  let tests = [testRoleMayGrant,
              testAlterLoginRolePrivilege,
              testGrantPermission,
              testAdminRole]
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess  

-- test that role with maygrant can share his role and that a role may not granted a role when unprivileged
testRoleMayGrant :: Test
testRoleMayGrant = TestCase $ do
  db <- LR.openNoPersistence
  let run roleName expr = do
        res <- executeAlterLoginRolesExpr roleName db expr
        case res of
          Left err -> assertFailure (show (expr, err))
          Right _ -> pure ()
      runAdmin = run adminRoleName
  --create some roles

  runAdmin (AddLoginRoleExpr "programmer1" True)
  runAdmin (AddLoginRoleExpr "programmer2" True)
  runAdmin (AddLoginRoleExpr "programmer" False) -- make programmer a non-login role

  --grant programmer to programmer1 and may grant privilege
  runAdmin (AddRoleToRoleExpr "programmer1" "programmer" True)

  -- programmer1 can grant programmer to programmer2
  run "programmer1" (AddRoleToRoleExpr "programmer2" "programmer" False)

  -- however, programmer2 may not grant the role
  res <- executeAlterLoginRolesExpr "programmer2" db (AddRoleToRoleExpr "admin" "programmer" False)
  assertEqual "reject grant" (Left PermissionDeniedError) res

    
--test that role without alter_login_roles cannot make changes
testAlterLoginRolePrivilege :: Test
testAlterLoginRolePrivilege = TestCase $ do
  db <- LR.openNoPersistence
  let run roleName expr = do
        res <- executeAlterLoginRolesExpr roleName db expr
        case res of
          Left err -> assertFailure (show (expr, err))
          Right _ -> pure ()
      runAdmin = run adminRoleName

  runAdmin (AddLoginRoleExpr "programmer1" True)
  --programmer1 can see his own roles
  run "programmer1" (ShowRolesForRoleExpr "programmer1")
  
  -- programmer1 cannot view all the roles
  res <- executeAlterLoginRolesExpr "programmer1" db ShowAllRolesExpr
  assertEqual "reject show all roles" (Left PermissionDeniedError) res  

  --programmer1 cannot alter the roles
  res' <- executeAlterLoginRolesExpr "programmer1" db (AddLoginRoleExpr "evilprogrammer" True)
  assertEqual "reject role add" (Left PermissionDeniedError) res'
  
--test permission addition and removal
testGrantPermission :: Test
testGrantPermission = TestCase $ do
    db <- LR.openNoPersistence
    let run roleName expr = do
          res <- executeAlterLoginRolesExpr roleName db expr
          case res of
            Left err -> assertFailure (show (expr, err))
            Right _ -> pure ()
        runAdmin = run adminRoleName
    runAdmin (AddLoginRoleExpr "programmer1" True)
    runAdmin (AddLoginRoleExpr "programmer2" True)
    runAdmin (AddPermissionToRoleExpr "programmer1" viewLoginRolesPerm False)

    res' <- executeAlterLoginRolesExpr "programmer1" db (AddPermissionToRoleExpr "programmer2" viewLoginRolesPerm False)
    assertEqual "reject perm grant" (Left PermissionDeniedError) res'

-- admin role has a special nil UUID
testAdminRole :: Test
testAdminRole = TestCase $ do
  -- db <- LR.openNoPersistence
  db <- LR.open "loginroles.db"
  LR.setupDatabaseIfNecessary db
  eRes <- roleIdsForRoleName LR.adminRoleName db
  assertEqual "roleIds for admin" (Right [UUID.nil]) eRes
  

