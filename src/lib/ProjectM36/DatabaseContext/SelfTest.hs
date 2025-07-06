-- | Include some test utility functions on top of the basic database context.
module ProjectM36.DatabaseContext.SelfTest where
import ProjectM36.DatabaseContext.Types
import ProjectM36.DatabaseContext.Basic
import ProjectM36.AtomFunctions.Basic
import ProjectM36.AtomFunctions.SelfTest
import Data.Functor.Identity

selfTestDatabaseContext :: ResolvedDatabaseContext
selfTestDatabaseContext = basicDatabaseContext {
  atomFunctions = Identity (basicAtomFunctions <> selfTestAtomFunctions)
  }
