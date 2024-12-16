{-# LANGUAGE DeriveGeneric #-}
module ProjectM36.Transaction.Types where
import ProjectM36.Base
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.DatabaseContext
import ProjectM36.MerkleHash
import GHC.Generics
import Data.Time.Clock
import qualified Data.List.NonEmpty as NE
import qualified Data.UUID as U
import qualified Data.Set as S

data Transaction = Transaction TransactionId TransactionInfo (Schemas DatabaseContext)
  deriving Generic

--- | Every transaction has context-specific information attached to it.
--- The `TransactionDiff`s represent child/edge relationships to previous transactions (branches or continuations of the same branch).
data TransactionInfo = TransactionInfo {
  parents :: TransactionParents,
  stamp :: UTCTime,
  merkleHash :: MerkleHash
  } deriving (Show, Generic)

type TransactionParents = NE.NonEmpty TransactionId

rootParent :: TransactionParents
rootParent = singleParent U.nil

singleParent :: TransactionId -> TransactionParents
singleParent tid = tid NE.:| []

uid :: Transaction -> TransactionId
uid (Transaction uid' _ _) = uid'

info :: Transaction -> TransactionInfo
info (Transaction _ info' _) = info'


transactionId :: Transaction -> TransactionId
transactionId (Transaction tid _ _) = tid

transactionInfo :: Transaction -> TransactionInfo
transactionInfo (Transaction _ info' _) = info'

instance Eq Transaction where                            
  (Transaction uuidA _ _) == (Transaction uuidB _ _) = uuidA == uuidB
                   
instance Ord Transaction where                            
  compare (Transaction uuidA _ _) (Transaction uuidB _ _) = compare uuidA uuidB

-- | Return the singular context which is not virtual.
concreteDatabaseContext :: Transaction -> DatabaseContext
concreteDatabaseContext (Transaction _ _ (Schemas context _)) = context

-- | Returns all schemas including the concrete schema.
schemas :: Transaction -> Schemas DatabaseContext
schemas (Transaction _ _ schemas') = schemas'
    
-- | Returns all subschemas which are isomorphic or sub-isomorphic to the concrete schema.
subschemas :: Transaction -> Subschemas
subschemas (Transaction _ _ (Schemas _ sschemas)) = sschemas

parentIds :: Transaction -> S.Set TransactionId
parentIds (Transaction _ tinfo _) = S.fromList (NE.toList (parents tinfo))
