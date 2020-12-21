{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, DerivingVia #-}
module ProjectM36.MerkleHash where
import Data.ByteString (ByteString)
import GHC.Generics
import Control.DeepSeq (NFData)

newtype MerkleHash = MerkleHash { _unMerkleHash :: ByteString }
  deriving (Show, Eq, Generic, Monoid, Semigroup, NFData)
                                
