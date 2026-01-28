-- | Utility module for importing scripted atom and database context functions.
module ProjectM36.Module where
import ProjectM36.Base
import Control.Monad.Trans.Writer

data ProcessFunction = RegisterAtomFunction String

declareAtomFunction :: FunctionName -> EntryPoints ()
declareAtomFunction nam = tell [DeclareAtomFunction nam]

declareDatabaseContextFunction :: FunctionName -> EntryPoints ()
declareDatabaseContextFunction nam = tell [DeclareDatabaseContextFunction nam]

type EntryPoints = Writer [DeclareFunction]

runEntryPoints :: EntryPoints () -> [DeclareFunction]
runEntryPoints e = execWriter e

data DeclareFunctionBase a = DeclareAtomFunction a |
                             DeclareDatabaseContextFunction a
  deriving (Show)

type DeclareFunction = DeclareFunctionBase FunctionName

