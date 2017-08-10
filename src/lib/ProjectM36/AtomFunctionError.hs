{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.AtomFunctionError where
import Data.Binary
import GHC.Generics
import Control.DeepSeq
import Data.Text

data AtomFunctionError = AtomFunctionUserError String |
                         AtomFunctionTypeMismatchError |
                         InvalidIntervalOrdering |
                         InvalidIntervalBoundaries |
                         AtomTypeDoesNotSupportOrdering Text |
                         AtomTypeDoesNotSupportInterval Text |
                         AtomFunctionBytesDecodingError String
                       deriving(Generic, Eq, Show, Binary, NFData)

