{-# LANGUAGE RankNTypes, DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, ExistentialQuantification, StandaloneDeriving, FlexibleInstances #-}
module ProjectM36.DatabaseContext.Types where
import GHC.Generics
import Control.DeepSeq (NFData)
import ProjectM36.Base
import ProjectM36.DatabaseContextFunctionError
import qualified Data.HashSet as HS
import Data.Functor.Identity

data ValueMarker a = ValueMarker a | NotChangedSinceMarker TransactionId
  deriving (NFData, Generic, Show)

valueIsUpdated :: ValueMarker a -> Bool
valueIsUpdated ValueMarker{} = True
valueIsUpdated NotChangedSinceMarker{} = False

emptyValue :: Monoid a => ValueMarker a
emptyValue = ValueMarker mempty

data DatabaseContextBase a = DatabaseContext {
  inclusionDependencies :: a InclusionDependencies,
  relationVariables :: a RelationVariables,
  atomFunctions :: a AtomFunctions,
  dbcFunctions :: a DatabaseContextFunctions,
  notifications :: a Notifications,
  typeConstructorMapping :: a TypeConstructorMapping,
  registeredQueries :: a RegisteredQueries
  } 

-- | The type of the database context when stored in the graph. It can reference data from other transactions in the graph.
type DatabaseContext = DatabaseContextBase ValueMarker

deriving instance NFData DatabaseContext
deriving instance Generic DatabaseContext

-- | The type of the database context when it is fully resolved (of transaction markers such as in the TransactionRefDatabaseContext); standalone, ready-to-use.
type ResolvedDatabaseContext = DatabaseContextBase Identity

data TransactionIdMarker a = TransactionIdMarker TransactionId a
  deriving (NFData, Generic)

type DatabaseContextFunctionBodyType = [Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext
type DatabaseContextFunctions = HS.HashSet DatabaseContextFunction

type DatabaseContextFunction = Function DatabaseContextFunctionBodyType
type DatabaseContextFunctionBody = FunctionBody DatabaseContextFunctionBodyType

