module ProjectM36.TransactionInfo where
import ProjectM36.Base
import qualified Data.Set as S
import Data.Time.Clock.POSIX

parentTids :: TransactionInfo -> S.Set TransactionId
parentTids (TransactionInfo tid _ _) = S.singleton tid
parentTids (MergeTransactionInfo tid1 tid2 _ _) = S.fromList [tid1, tid2]

diffs :: TransactionInfo -> TransactionDiffs
diffs (TransactionInfo _ diffs _) = diffs
diffs (MergeTransactionInfo _ _ diffs _) = diffs

children :: TransactionInfo -> S.Set TransactionId
children ti = S.fromList (map snd (diffs ti))

stamp :: TransactionInfo -> UTCTime
stamp (TransactionInfo _ _ t) = t
stamp (MergeTransactionInfo _ _ _ t) = t
