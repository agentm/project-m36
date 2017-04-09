{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.DatabaseContextFunctionError where
import GHC.Generics
import Data.Binary
import Control.DeepSeq

data DatabaseContextFunctionError = DatabaseContextFunctionUserError String
                                  deriving (Generic, Eq, Show, Binary, NFData)