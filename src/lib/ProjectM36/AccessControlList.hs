module ProjectM36.AccessControlList where
import qualified Data.Set as S
import Data.UUID

data AccessControlList role' permission =
  AccessControlList [RoleAccess role' permission]
  deriving (Show)

data RoleAccess role' permission = RoleAccess
  {
    role :: role',
    perms :: S.Set permission
  }
  deriving (Eq, Show)

type RoleId = UUID

type RelVarAccessControlList = AccessControlList RoleId RelVarPermission

type FunctionControlList = AccessControlList RoleId FunctionPermission

data RelVarPermission = AccessRelVars
  deriving (Eq, Show, Ord)

data FunctionPermission =
  ExecuteFunctionPermission -- ^ allow the role to execute the function
  | ViewFunctionPermission -- ^ allow the role to see that the function exists
  deriving (Eq, Show, Ord)

hasAccess :: (Eq role', Eq perm) => [role'] -> perm -> AccessControlList role' perm -> Bool
hasAccess hasRoles checkPerm (AccessControlList roleAccesses) =
  or (map (\ra -> role ra `elem` hasRoles && checkPerm `elem` perms ra) roleAccesses)

addAccess :: (Eq role', Eq perm, Ord perm) => role' -> perm -> AccessControlList role' perm -> AccessControlList role' perm
addAccess targetRole newPerm (AccessControlList ras) =
  AccessControlList $ if targetRole `elem` map role ras then
                        modifyExistingRoleAccess
                      else
                        addRoleAccess

  where
    addRoleAccess = ras <> [RoleAccess { role = targetRole,
                                        perms = S.singleton newPerm
                                      }]
    modifyExistingRoleAccess = foldr folder [] ras
    folder ra acc =
      if role ra == targetRole then
        if newPerm `elem` perms ra then
          ra : acc
        else
          ra { perms = S.insert newPerm (perms ra) } : acc
      else
        RoleAccess { role = targetRole, perms = S.singleton newPerm } : acc

removeAccess :: (Eq role', Eq perm, Ord perm) => role' -> perm -> AccessControlList role' perm -> AccessControlList role' perm
removeAccess targetRole targetPerm (AccessControlList ras) =
  AccessControlList $ foldr folder [] ras
  where
    folder ra acc = 
      if role ra == targetRole then
          ra { perms = S.delete targetPerm (perms ra) } : acc
      else
        ra : acc

