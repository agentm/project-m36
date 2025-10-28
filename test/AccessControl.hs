import TutorialD.Interpreter.TestBase
import ProjectM36.Client
import Test.HUnit
import System.Exit

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testDBCFunctionACL])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testDBCFunctionACL :: Test
testDBCFunctionACL = TestCase $ do
  (sessionId, conn) <- dateExamplesConnection emptyNotificationCallback
  -- add a less-privileged role
  let user1 = "user1"
  res <- executeAlterLoginRolesExpr sessionId conn (AddLoginRoleExpr user1 False)
  assertEqual "add role" (Right "ok") res
  let user1conn = setRoleName user1 conn
  -- test that the role does *not* have access to a dbc function
  res' <- executeDatabaseContextExpr sessionId user1conn (ExecuteDatabaseContextFunction "deleteAll" [])
  assertEqual "failure to run dbc function" (Left (AccessDeniedError (SomeDBCFunctionPermission ExecuteDBCFunctionPermission))) res'
  -- test that the role cannot create a dbc function
  res'' <- executeDatabaseContextIOExpr sessionId user1conn (AddDatabaseContextFunction "failure" [] "")
  assertEqual "failure to add dbc function" (Left (AccessDeniedError (SomeFunctionPermission LoadFunctionPermission))) res''
  -- grant permission to the role to execute the function
  res''' <- executeDatabaseContextExpr sessionId conn (AlterACL (GrantDBCFunctionAccessExpr user1 "deleteAll" ExecuteDBCFunctionPermission False))
  assertEqual "success adding function permission" (Right ()) res'''
  
  -- successfully execute the dbc function
  res'''' <- executeDatabaseContextExpr sessionId user1conn (ExecuteDatabaseContextFunction "deleteAll" [])
  assertEqual "success calling deleteAll" (Right ()) res''''
