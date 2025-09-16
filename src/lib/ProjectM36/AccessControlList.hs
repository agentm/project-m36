{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.AccessControlList where
import qualified Data.Set as S
import Data.UUID
import Control.DeepSeq (NFData)
import GHC.Generics

data AccessControlList role' permission =
  AccessControlList [RoleAccess role' permission]
  deriving (Show, NFData, Generic)

instance (Ord p, Eq r) => Semigroup (AccessControlList r p) where
  a <> b = merge a b
  
instance (Ord p, Eq r) => Monoid (AccessControlList r p) where
  mempty = empty

data RoleAccess role' permission = RoleAccess
  {
    role :: role',
    perms :: S.Set permission
  }
  deriving (Eq, Show, NFData, Generic)

type RoleId = UUID

-- | Determine if the role can view or alter underlying relation variables.
type RelVarAccessControlList = AccessControlList RoleId RelVarPermission
type RelVarRoleAccess = RoleAccess RoleId RelVarPermission

type FunctionAccessControlList = AccessControlList RoleId FunctionPermission
type FunctionRoleAccess = RoleAccess RoleId FunctionPermission

type AlterTransGraphAccessControlList = AccessControlList RoleId AlterTransGraphPermission
type AlterTransGraphRoleAccess = RoleAccess RoleId AlterTransGraphPermission

type SchemaAccessControlList = AccessControlList RoleId AlterSchemaPermission
type SchemaRoleAccess = RoleAccess RoleId AlterSchemaPermission

class AllPermissions a where
  allPermissions :: S.Set a

data RelVarPermission = AccessRelVarsPermission {-| AccessTypeConstructorsPermission -}
  deriving (Eq, Show, Ord, Generic, NFData)

instance AllPermissions RelVarPermission where
  allPermissions = S.fromList [AccessRelVarsPermission]

data FunctionPermission =
  ExecuteFunctionPermission -- ^ allow the role to execute the function
  | ViewFunctionPermission -- ^ allow the role to see that the function exists
  | LoadFunctionPermission -- ^ allow the role to load functions from outside the database
  deriving (Eq, Show, Ord, Generic, NFData)

instance AllPermissions FunctionPermission where
  allPermissions = S.fromList [ExecuteFunctionPermission, ViewFunctionPermission, LoadFunctionPermission]

data AlterTransGraphPermission =
  CommitTransactionPermission -- ^ allow the role to commit a transaction to the graph
  deriving (Eq, Show, Ord, Generic, NFData)  

instance AllPermissions AlterTransGraphPermission where
  allPermissions = S.fromList [CommitTransactionPermission]

data AlterSchemaPermission =
  AlterSchemaPermission -- ^ allow the role to add or remove a schema
  deriving (Eq, Show, Ord, Generic, NFData)

instance AllPermissions AlterSchemaPermission where
  allPermissions = S.fromList [AlterSchemaPermission]
    
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

empty :: AccessControlList r p
empty = AccessControlList []

merge :: (Ord p, Eq r) => AccessControlList r p -> AccessControlList r p -> AccessControlList r p
merge (AccessControlList acl1) (AccessControlList acl2) =
  AccessControlList $ foldr folder acl1 acl2
  where
    folder ra acc =
      foldr (\perm acc2 ->
               case addAccess (role ra) perm (AccessControlList acc2) of
                 AccessControlList aclout -> aclout
               ) acc (perms ra)

data DatabaseContextACL =
  DatabaseContextACL {
  relvarsACL :: RelVarAccessControlList,
  dbcFunctionsACL :: FunctionAccessControlList,
  transGraphACL :: AlterTransGraphAccessControlList,
  schemaACL :: SchemaAccessControlList
      }
  deriving (Show, NFData, Generic)

instance Semigroup DatabaseContextACL where
  a <> b = DatabaseContextACL {
    relvarsACL = relvarsACL a <> relvarsACL b,
    dbcFunctionsACL = dbcFunctionsACL a <> dbcFunctionsACL b,
    transGraphACL = transGraphACL a <> transGraphACL b,
    schemaACL = schemaACL a <> schemaACL b
    }

instance Monoid DatabaseContextACL where
  mempty = DatabaseContextACL {
    relvarsACL = mempty,
    dbcFunctionsACL = mempty,
    transGraphACL = mempty,
    schemaACL = mempty
    }

basic :: DatabaseContextACL
basic = DatabaseContextACL {
  relvarsACL = AccessControlList [RoleAccess {
                                     role = superAdminRole,
                                     perms = allPermissions
                                     }],
  dbcFunctionsACL = AccessControlList [RoleAccess {
                                          role = superAdminRole,
                                          perms = allPermissions
                                          }],
  transGraphACL = AccessControlList [RoleAccess {
                                        role = superAdminRole,
                                        perms = allPermissions
                                        }],
  schemaACL = AccessControlList [RoleAccess {
                                        role = superAdminRole,
                                        perms = allPermissions
                                        }]
  }

superAdminRole :: RoleId
superAdminRole = nil

-- used in access control error reporting
data SomePermission = SomeRelVarPermission RelVarPermission |
                      SomeFunctionPermission FunctionPermission |
                      SomeAlterSchemaPermission AlterSchemaPermission |
                      SomeAlterTransGraphPermission AlterTransGraphPermission
                    deriving (Show, NFData, Generic, Eq)
                     
