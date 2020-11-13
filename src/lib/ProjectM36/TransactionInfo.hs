module ProjectM36.TransactionInfo where
import ProjectM36.Base
import Data.Time.Clock
import qualified Data.List.NonEmpty as NE

-- | Create a TransactionInfo with just one parent transaction ID.
singleParent :: TransactionId -> UTCTime -> TransactionInfo
singleParent tid stamp' = TransactionInfo {
  parents = tid NE.:| [],
  stamp = stamp',
  merkleHash = mempty }

