import TutorialD.Interpreter.TestBase
import ProjectM36.Client
import ProjectM36.Relation
import Test.HUnit
import System.Exit
import Data.Either (isRight)

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testDBCFunctionACL,
                                 testRelVarAccess,
                                 testFunctionAccess])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testDBCFunctionACL :: Test
testDBCFunctionACL = TestCase $ do
  (sessionId, conn) <- dateExamplesConnection emptyNotificationCallback
  -- add a less-privileged role
  let user1 = "user1"
  res <- executeAlterLoginRolesExpr sessionId conn (AddLoginRoleExpr user1 False)
  assertEqual "add role" (Right QuietSuccessResult) res
  let user1conn = setRoleName user1 conn
  -- test that the role does *not* have access to a dbc function
  res' <- executeDatabaseContextExpr sessionId user1conn (ExecuteDatabaseContextFunction "deleteAll" [])
  assertEqual "failure to run dbc function" (Left (AccessDeniedError (SomeFunctionPermission ExecuteFunctionPermission))) res'
  -- test that the role cannot create a dbc function
  res'' <- executeDatabaseContextIOExpr sessionId user1conn (AddDatabaseContextFunction "failure" [] "")
  assertEqual "failure to add dbc function" (Left (AccessDeniedError (SomeFunctionPermission AlterFunctionPermission))) res''
  -- grant permission to the role to execute the function
  res''' <- executeDatabaseContextExpr sessionId conn (AlterACL (GrantDBCFunctionAccessExpr user1 "deleteAll" ExecuteDBCFunctionPermission False))
  assertEqual "success adding function permission" (Right ()) res'''
  
  -- fail again because of two-tiered ACLs necessary to execute the dbc function
  res'''' <- executeDatabaseContextExpr sessionId user1conn (ExecuteDatabaseContextFunction "deleteAll" [])
  assertEqual "failure of calling deleteAll" (Left (AccessDeniedError (SomeFunctionPermission ExecuteFunctionPermission))) res''''

  -- grant the remaining, necessary permission
  res'''''' <- executeDatabaseContextExpr sessionId conn (AlterACL (GrantAccessExpr user1 (SomeFunctionPermission ExecuteFunctionPermission) False))
  assertEqual "success adding function permission" (Right ()) res''''''

  -- successfully call the dbc function
  res''''''' <- executeDatabaseContextExpr sessionId user1conn (ExecuteDatabaseContextFunction "deleteAll" [])
  assertEqual "success calling deleteAll" (Right ()) res'''''''


testRelVarAccess :: Test
testRelVarAccess = TestCase $ do
  (sessionId, conn) <- dateExamplesConnection emptyNotificationCallback
  -- add a less-privileged role
  let user1 = "user1"
  res <- executeAlterLoginRolesExpr sessionId conn (AddLoginRoleExpr user1 False)
  assertEqual "add role" (Right QuietSuccessResult) res
  let user1conn = setRoleName user1 conn

  -- check that the user cannot view the relvars
  res' <- executeRelationalExpr sessionId user1conn (RelationVariable "x" ())

  assertEqual "reject relvar access" (Left (AccessDeniedError (SomeRelVarPermission AccessRelVarsPermission))) res'

  -- grant relvars access
  res'' <- executeDatabaseContextExpr sessionId conn (AlterACL (GrantAccessExpr user1 (SomeRelVarPermission AccessRelVarsPermission) False))

  assertEqual "grant rv access" (Right ()) res''

  -- check that the user can view the relvars
  res''' <- executeRelationalExpr sessionId user1conn (RelationVariable "true" ())

  assertEqual "accept relvar access" (Right relationTrue) res'''
  
testFunctionAccess :: Test
testFunctionAccess = TestCase $ do
  (sessionId, conn) <- dateExamplesConnection emptyNotificationCallback
  -- add a less-privileged role
  let user1 = "user1"
  res <- executeAlterLoginRolesExpr sessionId conn (AddLoginRoleExpr user1 False)
  assertEqual "add role" (Right QuietSuccessResult) res
  let user1conn = setRoleName user1 conn

  -- check that function view access is denied
  res' <- databaseContextFunctionsAsRelation sessionId user1conn 
  assertEqual "rejected dbc function view" (Left (AccessDeniedError (SomeRelVarPermission AccessRelVarsPermission))) res'
  
  -- grant function view
  res'' <- executeDatabaseContextExpr sessionId conn (AlterACL (GrantAccessExpr user1 (SomeRelVarPermission AccessRelVarsPermission) False))
  assertEqual "grant relvars access" (Right ()) res''

  -- check that function view works
  res''' <- databaseContextFunctionsAsRelation sessionId user1conn 
  assertBool "rejected dbc function view" (isRight res''')

  -- check that function execute is denied
  res'''' <- executeDatabaseContextExpr sessionId user1conn (ExecuteDatabaseContextFunction "deleteAll" [])
  assertEqual "failure to run dbc function" (Left (AccessDeniedError (SomeFunctionPermission ExecuteFunctionPermission))) res''''

  -- grant permission to the role to execute the function
  res''''' <- executeDatabaseContextExpr sessionId conn (AlterACL (GrantDBCFunctionAccessExpr user1 "deleteAll" ExecuteDBCFunctionPermission False))
  assertEqual "success adding dbcfunction permission" (Right ()) res'''''

  res'''''' <- executeDatabaseContextExpr sessionId conn (AlterACL (GrantAccessExpr user1 (SomeFunctionPermission ExecuteFunctionPermission) False))
  assertEqual "success adding function permission" (Right ()) res''''''

  -- check that function execute works
  res''''''' <- executeDatabaseContextExpr sessionId user1conn (ExecuteDatabaseContextFunction "deleteAll" [])
  assertEqual "success calling deleteAll" (Right ()) res'''''''
