module ProjectM36.Transaction where
import ProjectM36.Base
import qualified Data.Set as S
import qualified Data.UUID as U
import Data.Time.Clock
import qualified Data.List.NonEmpty as NE
import ProjectM36.TransactionDiffs

parentIds :: Transaction -> S.Set TransactionId
parentIds (Transaction _ tinfo _) = S.fromList (NE.toList (NE.map fst (parents tinfo)))

-- | Return the same transaction but referencing only the specific child transactions. This is useful when traversing a graph and returning a subgraph. This doesn't filter parent transactions because it assumes a head-to-root traversal.
filterTransactionInfoTransactions :: S.Set TransactionId -> TransactionInfo -> TransactionInfo
filterTransactionInfoTransactions filterIds tinfo =
  TransactionInfo { parents = case
                      NE.filter (\diff -> S.member (fst diff) filterIds) (parents tinfo) of
                      [] -> root
                      xs -> NE.fromList xs,
                    stamp = stamp tinfo }

filterParent :: TransactionId -> S.Set TransactionId -> TransactionId
filterParent parentId validIds = if S.member parentId validIds then parentId else U.nil

-- | Remove any child or parent transaction references not in the valud UUID set.
filterTransaction :: S.Set TransactionId -> Transaction -> Transaction
filterTransaction filterIds (Transaction selfId tInfo context) = Transaction selfId (filterTransactionInfoTransactions filterIds tInfo) context

-- | Return the singular context which is not virtual.
concreteDatabaseContext :: Transaction -> DatabaseContext
concreteDatabaseContext (Transaction _ _ (Schemas context _)) = context

-- | Returns all schemas including the concrete schema.
schemas :: Transaction -> Schemas
schemas (Transaction _ _ schemas') = schemas'
    
-- | Returns all subschemas which are isomorphic or sub-isomorphic to the concrete schema.
subschemas :: Transaction -> Subschemas
subschemas (Transaction _ _ (Schemas _ sschemas)) = sschemas

diffs :: Transaction -> TransactionDiffs
diffs (Transaction _ info _) = parents info

fresh :: TransactionId -> UTCTime -> Schemas -> Transaction
fresh freshId stamp' newSchemas = Transaction freshId (TransactionInfo root stamp') newSchemas

timestamp :: Transaction -> UTCTime
timestamp (Transaction _ tinfo _) = stamp tinfo
