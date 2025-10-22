import Test.HUnit
import ProjectM36.AccessControlList
import qualified Data.UUID as U
import qualified Data.ByteString.Lazy as BS
import Data.Word
import qualified Data.Map as M
import Data.Maybe
import System.Exit

main :: IO ()
main = do
  tcounts <- runTestTT (TestList [testBasic])
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess  

fakeUUID :: Word8 -> U.UUID
fakeUUID x = fromMaybe (error "impossible uuid") (U.fromByteString (BS.concat (replicate 4 w32)))
  where w32 = BS.pack (replicate 4 x)

testBasic :: Test
testBasic = TestCase $ do
  let func_acl = AccessControlList (M.singleton role1
                                                 (grantable ViewFunctionPermission))
      role1 = fakeUUID 1
      role2 = fakeUUID 2

  assertBool "simple hasAccess" (hasAccess [role1] ViewFunctionPermission func_acl)

  let func_acl1 = addAccess role1 ExecuteFunctionPermission True func_acl
  assertBool "addAccess to existing role" (hasAccess [role1] ExecuteFunctionPermission func_acl1)

  let func_acl2 = addAccess role2 ViewFunctionPermission True func_acl
  assertBool "addAccess with new role" (hasAccess [role2] ViewFunctionPermission func_acl2)
  let func_acl3 = removeAccess role1 ViewFunctionPermission func_acl
  assertBool "remove access" (not (hasAccess [role1] ViewFunctionPermission func_acl3))

  let func_acl4 = addAccess role2 ExecuteFunctionPermission True func_acl
  assertBool "mixed perms" (hasAccess [role2] ExecuteFunctionPermission func_acl4)

  
