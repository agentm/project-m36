-- | Mark fields as not updated since a specific transaction. Useful for not writing redundant data to disk- the data was written by an earlier transaction.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.ValueMarker where
import ProjectM36.Base
import GHC.Generics
import Control.DeepSeq (NFData)
  
data ValueMarker a = ValueMarker a | NotChangedSinceMarker TransactionId
  deriving (NFData, Generic, Show)

valueIsUpdated :: ValueMarker a -> Bool
valueIsUpdated ValueMarker{} = True
valueIsUpdated NotChangedSinceMarker{} = False

emptyValue :: Monoid a => ValueMarker a
emptyValue = ValueMarker mempty
