{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.DatabaseContextFunctionError where
import GHC.Generics
import Data.Binary
import Control.DeepSeq

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
data DatabaseContextFunctionError = DatabaseContextFunctionUserError String
                                  deriving (Generic, Eq, Show, Binary, NFData)