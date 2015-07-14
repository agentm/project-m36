module ProjectM36.HashSetBinary where
import Data.Hashable
import qualified Data.HashSet as HS
import Data.Binary

instance (Binary a, Hashable a, Eq a) => Binary (HS.HashSet a) where
  put = put . HS.toList
  get = do
        list <- get :: (Binary a, Hashable a, Eq a) => Get [a]
        return (HS.fromList list)

