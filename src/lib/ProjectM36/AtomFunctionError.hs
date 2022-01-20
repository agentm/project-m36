{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.AtomFunctionError where
import GHC.Generics
import Control.DeepSeq
import Data.Text

data AtomFunctionError = AtomFunctionUserError String |
                         AtomFunctionTypeMismatchError |
                         AtomFunctionParseError String |
                         InvalidIntervalOrderingError |
                         InvalidIntervalBoundariesError |
                         InvalidIntBoundError |
                         InvalidUUIDString Text |
                         AtomFunctionEmptyRelationError |
                         AtomTypeDoesNotSupportOrderingError Text |
                         AtomTypeDoesNotSupportIntervalError Text |
                         AtomFunctionBytesDecodingError String
                       deriving(Generic, Eq, Show, NFData)

