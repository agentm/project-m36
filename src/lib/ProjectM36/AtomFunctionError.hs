{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.AtomFunctionError where
import Data.Binary
import GHC.Generics
import Control.DeepSeq

data AtomFunctionError = AtomFunctionUserError String |
                         AtomFunctionTypeMismatchError |
                         AtomFunctionBytesDecodingError String
                       deriving(Generic, Eq, Show, Binary, NFData)

