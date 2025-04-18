module ProjectM36.Transaction where
import ProjectM36.Base
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.DatabaseContext.Types
import ProjectM36.Transaction.Types
import qualified Data.Set as S
import qualified Data.UUID as U
import Data.Time.Clock
import qualified Data.List.NonEmpty as NE

-- | Return the same transaction but referencing only the specific child transactions. This is useful when traversing a graph and returning a subgraph. This doesn't filter parent transactions because it assumes a head-to-root traversal.
filterTransactionInfoTransactions :: S.Set TransactionId -> TransactionInfo -> TransactionInfo
filterTransactionInfoTransactions filterIds tinfo =
  tinfo { parents = case
                      NE.filter (`S.member`  filterIds) (parents tinfo) of
                      [] -> rootParent
                      xs -> NE.fromList xs}

filterParent :: TransactionId -> S.Set TransactionId -> TransactionId
filterParent parentId validIds = if S.member parentId validIds then parentId else U.nil

-- | Remove any child or parent transaction references not in the valud UUID set.
filterTransaction :: S.Set TransactionId -> Transaction -> Transaction
filterTransaction filterIds (Transaction selfId tInfo context) = Transaction selfId (filterTransactionInfoTransactions filterIds tInfo) context


fresh :: TransactionId -> UTCTime -> Schemas DatabaseContext -> Transaction
fresh freshId stamp' = Transaction freshId tinfo
  where
    tinfo = TransactionInfo {parents = rootParent,
                             stamp = stamp',
                             merkleHash = mempty
                            }

timestamp :: Transaction -> UTCTime
timestamp (Transaction _ tinfo _) = stamp tinfo

