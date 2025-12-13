{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes, TupleSections #-}
module ProjectM36.AccessControlList where
import qualified Data.Set as S
import Data.UUID
import Control.DeepSeq (NFData)
import GHC.Generics
import Data.Hashable
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Default

newtype AccessControlList role' permission =
    AccessControlList (M.Map role' (RoleAccess permission))
   deriving (Show, NFData, Generic, Read, Eq)

instance Ord r => Default (AccessControlList r p) where
  def = AccessControlList mempty

instance (Ord p, Eq r, Ord r) => Semigroup (AccessControlList r p) where
  a <> b = merge a b
  
instance (Ord p, Eq r, Ord r) => Monoid (AccessControlList r p) where
  mempty = empty

type MayGrant = Bool

type RoleAccess permission = M.Map permission MayGrant

type RoleId = UUID

-- | Determine if the role can view or alter underlying relation variables.
type RelVarAccessControlList = AccessControlList RoleId RelVarPermission

type FunctionAccessControlList = AccessControlList RoleId FunctionPermission

type AlterTransGraphAccessControlList = AccessControlList RoleId AlterTransGraphPermission

type SchemaAccessControlList = AccessControlList RoleId AlterSchemaPermission

type ACLAccessControlList = AccessControlList RoleId ACLPermission

type DBCFunctionAccessControlList = AccessControlList RoleId DBCFunctionPermission

grantable :: perm -> M.Map perm MayGrant
grantable perm = M.singleton perm True

notGrantable :: perm -> M.Map perm MayGrant
notGrantable perm = M.singleton perm False

class AllPermissions a where
  allPermissions :: S.Set a

data RelVarPermission = AccessRelVarsPermission {-| AccessTypeConstructorsPermission -}
  deriving (Eq, Show, Ord, Generic, NFData, Read, Hashable)

instance AllPermissions RelVarPermission where
  allPermissions = S.fromList [AccessRelVarsPermission]

data FunctionPermission =
  ExecuteFunctionPermission -- ^ allow the role to execute the function
  | ViewFunctionPermission -- ^ allow the role to see that the function exists
  | AlterFunctionPermission -- ^ allow the role to load functions from outside the database or replace existing functions
  deriving (Eq, Show, Ord, Generic, NFData, Hashable)

instance AllPermissions FunctionPermission where
  allPermissions = S.fromList [ExecuteFunctionPermission, ViewFunctionPermission, AlterFunctionPermission]

data AlterTransGraphPermission =
  CommitTransactionPermission -- ^ allow the role to commit a transaction to the graph
  deriving (Eq, Show, Ord, Generic, NFData, Hashable)

instance AllPermissions AlterTransGraphPermission where
  allPermissions = S.fromList [CommitTransactionPermission]

data AlterSchemaPermission =
  AlterSchemaPermission -- ^ allow the role to add or remove a schema
  deriving (Eq, Show, Ord, Generic, NFData, Hashable)

instance AllPermissions AlterSchemaPermission where
  allPermissions = S.fromList [AlterSchemaPermission]

data ACLPermission =
  ViewACLPermission |
  AlterACLPermission
  deriving (Eq, Show, Ord, Generic, NFData, Hashable)

instance AllPermissions ACLPermission where
  allPermissions = S.fromList [ViewACLPermission, AlterACLPermission]

data DBCFunctionPermission =
  ViewDBCFunctionPermission |
  ExecuteDBCFunctionPermission |
  AlterDBCFunctionPermission -- ^ change or delete this function
  deriving (Eq, Show, Ord, Generic, NFData, Hashable)

instance AllPermissions DBCFunctionPermission where
  allPermissions = S.fromList [ViewDBCFunctionPermission,
                               ExecuteDBCFunctionPermission,
                               AlterDBCFunctionPermission]

hasAccess :: (Ord perm, Ord role', Eq role', Eq perm) => [role'] -> perm -> AccessControlList role' perm -> Bool
hasAccess hasRoles checkPerm (AccessControlList acl) =
  any (\role -> M.member checkPerm (fromMaybe mempty (M.lookup role acl))) hasRoles

addAccess :: (Eq role', Ord role', Eq perm, Ord perm) => role' -> perm -> MayGrant -> AccessControlList role' perm -> AccessControlList role' perm
addAccess targetRole newPerm mayGrant (AccessControlList ras) =
  AccessControlList $ M.insertWith M.union targetRole (M.singleton newPerm mayGrant) ras

removeAccess :: (Eq role', Ord role', Eq perm, Ord perm) => role' -> perm -> AccessControlList role' perm -> AccessControlList role' perm
removeAccess targetRole targetPerm (AccessControlList acl) =
  normalize $ AccessControlList $ M.alter alterFunc targetRole acl
  where
    alterFunc (Just ras) = Just $ M.delete targetPerm ras
    alterFunc Nothing = Nothing

-- | Remove empty dictionaries (roles without permissions).
normalize :: AccessControlList r p -> AccessControlList r p
normalize (AccessControlList acl) = AccessControlList $ M.filter (not . M.null) acl

empty :: Ord r => AccessControlList r p
empty = AccessControlList mempty

allPermissionsForRoleId :: (AllPermissions p, Ord p) => r -> AccessControlList r p
allPermissionsForRoleId roleid = AccessControlList (M.singleton roleid (M.fromList (map (,True) (S.toList allPermissions))))

merge :: (Ord p, Eq r, Ord r) => AccessControlList r p -> AccessControlList r p -> AccessControlList r p
merge (AccessControlList acl1) (AccessControlList acl2) =
  normalize $ AccessControlList $ M.unionWith mergeFunc acl1 acl2
  where
    mergeFunc = M.unionWith (||)

data DatabaseContextACL =
  DatabaseContextACL {
  relvarsACL :: RelVarAccessControlList,
  dbcFunctionsACL :: FunctionAccessControlList,
  transGraphACL :: AlterTransGraphAccessControlList,
  schemaACL :: SchemaAccessControlList,
  aclACL :: ACLAccessControlList
      }
  deriving (Show, NFData, Generic)

instance Semigroup DatabaseContextACL where
  a <> b = DatabaseContextACL {
    relvarsACL = relvarsACL a <> relvarsACL b,
    dbcFunctionsACL = dbcFunctionsACL a <> dbcFunctionsACL b,
    transGraphACL = transGraphACL a <> transGraphACL b,
    schemaACL = schemaACL a <> schemaACL b,
    aclACL = aclACL a <> aclACL b
    }

instance Monoid DatabaseContextACL where
  mempty = DatabaseContextACL {
    relvarsACL = mempty,
    dbcFunctionsACL = mempty,
    transGraphACL = mempty,
    schemaACL = mempty,
    aclACL = mempty
    }

basic :: DatabaseContextACL
basic = DatabaseContextACL {
  relvarsACL = AccessControlList (M.singleton adminRoleId
                                   (grantableL allPermissions)
                                 ),
  dbcFunctionsACL = AccessControlList (M.singleton adminRoleId
                                        (grantableL allPermissions)
                                      ),
  transGraphACL = AccessControlList (M.singleton adminRoleId
                                      (grantableL allPermissions)
                                    ),
  schemaACL = AccessControlList (M.singleton adminRoleId
                                  (grantableL allPermissions)
                                ),
  aclACL = AccessControlList (M.singleton adminRoleId
                               (grantableL allPermissions)
                             )
  }
  where
    grantableL :: forall perm. Ord perm => S.Set perm -> RoleAccess perm
    grantableL perms = M.fromList (map (, True) (S.toList perms))

adminRoleId :: RoleId
adminRoleId = nil

-- used in access control error reporting
data SomePermission = SomeRelVarPermission RelVarPermission |
                      SomeFunctionPermission FunctionPermission |
                      SomeAlterSchemaPermission AlterSchemaPermission |
                      SomeAlterTransGraphPermission AlterTransGraphPermission |
                      SomeACLPermission ACLPermission |
                      SomeDBCFunctionPermission DBCFunctionPermission
                    deriving (Show, NFData, Generic, Eq, Hashable)


