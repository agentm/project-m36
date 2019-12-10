module ProjectM36.TransactionInfo where
import ProjectM36.Base
import qualified Data.Set as S
import Data.Time.Clock
import Data.List.NonEmpty

-- | Create a TransactionInfo with just one parent transaction ID.
singleParent :: TransactionId -> UTCTime -> TransactionInfo
singleParent tid stamp = TransactionInfo {
  parents = tid :| [],
  stamp = stamp }
