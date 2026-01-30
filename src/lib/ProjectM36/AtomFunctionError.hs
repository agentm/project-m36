{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.AtomFunctionError where
import GHC.Generics
import Control.DeepSeq
import Data.Text

data AtomFunctionError = AtomFunctionUserError String |
                         AtomFunctionTypeMismatchError |
                         AtomFunctionParseError String |
                         AtomFunctionMissingReturnTypeError |
                         InvalidIntervalOrderingError |
                         InvalidIntervalBoundariesError |
                         AtomFunctionAttributeNameNotFoundError Text |
                         InvalidIntBoundError |
                         InvalidUUIDString Text |
                         RelationAtomExpectedError Text |
                         AtomFunctionEmptyRelationError |
                         AtomTypeDoesNotSupportOrderingError Text |
                         AtomTypeDoesNotSupportIntervalError Text |
                         AtomFunctionBytesDecodingError String
                       deriving (Generic, Eq, Show, NFData)

