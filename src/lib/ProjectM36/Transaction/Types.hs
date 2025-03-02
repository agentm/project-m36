{-# LANGUAGE DeriveGeneric #-}
module ProjectM36.Transaction.Types where
import ProjectM36.Base
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.DatabaseContext
import ProjectM36.ChangeTrackingDatabaseContext
import ProjectM36.MerkleHash
import GHC.Generics
import Data.Time.Clock
import qualified Data.List.NonEmpty as NE
import qualified Data.UUID as U
import qualified Data.Set as S

data TransactionBase ctx = Transaction TransactionId TransactionInfo (Schemas ctx)
  deriving Generic

type Transaction = TransactionBase DatabaseContext

-- | Data needed to write a transaction to permanent storage. This is different from a DisconnectedTransaction in that it's not clear if the DisconnectedTransaction will be written to disk (or rolled back). This transaction will be written.
type UnwrittenTransaction = TransactionBase ChangeTrackingDatabaseContext

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

info :: TransactionBase a -> TransactionInfo
info (Transaction _ info' _) = info'

transactionId :: TransactionBase a -> TransactionId
transactionId (Transaction tid _ _) = tid

transactionInfo :: TransactionBase a -> TransactionInfo
transactionInfo (Transaction _ info' _) = info'

instance Eq (TransactionBase a) where                            
  (Transaction uuidA _ _) == (Transaction uuidB _ _) = uuidA == uuidB
                   
instance Ord (TransactionBase a) where                            
  compare (Transaction uuidA _ _) (Transaction uuidB _ _) = compare uuidA uuidB

-- | Return the singular context which is not virtual.
concreteDatabaseContext :: TransactionBase a -> a
concreteDatabaseContext (Transaction _ _ (Schemas context _)) = context

-- | Returns all schemas including the concrete schema.
schemas :: Transaction -> Schemas DatabaseContext
schemas (Transaction _ _ schemas') = schemas'
    
-- | Returns all subschemas which are isomorphic or sub-isomorphic to the concrete schema.
subschemas :: TransactionBase a -> Subschemas
subschemas (Transaction _ _ (Schemas _ sschemas)) = sschemas

parentIds :: TransactionBase a -> S.Set TransactionId
parentIds (Transaction _ tinfo _) = S.fromList (NE.toList (parents tinfo))
