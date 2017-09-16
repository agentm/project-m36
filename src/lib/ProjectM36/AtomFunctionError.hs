{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.AtomFunctionError where
import Data.Binary
import GHC.Generics
import Control.DeepSeq
import Data.Text

data AtomFunctionError = AtomFunctionUserError String |
                         AtomFunctionTypeMismatchError |
                         InvalidIntervalOrderingError |
                         InvalidIntervalBoundariesError |
                         AtomFunctionEmptyRelationError |
                         AtomTypeDoesNotSupportOrderingError Text |
                         AtomTypeDoesNotSupportIntervalError Text |
                         AtomFunctionBytesDecodingError String
                       deriving(Generic, Eq, Show, Binary, NFData)

