{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module ProjectM36.MerkleHash where
import Data.ByteString (ByteString)
import GHC.Generics
import Data.Binary (Binary)
import Control.DeepSeq (NFData)

newtype MerkleHash = MerkleHash { _unMerkleHash :: ByteString }
  deriving (Show, Eq, Binary, Generic, Monoid, Semigroup, NFData)
                                
