{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, DerivingVia #-}
module ProjectM36.MerkleHash where
import Data.ByteString (ByteString)
import GHC.Generics
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

newtype MerkleHash = MerkleHash { _unMerkleHash :: ByteString }
  deriving (Eq, Generic, Monoid, Semigroup, NFData)

instance Show MerkleHash where
  show h = T.unpack (TE.decodeUtf8 (B64.encode (_unMerkleHash h)))
                                
