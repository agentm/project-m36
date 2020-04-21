{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.Error where
import ProjectM36.Base
import ProjectM36.DatabaseContextFunctionError
import ProjectM36.AtomFunctionError
import qualified Data.Set as S
import Control.DeepSeq (NFData, rnf)
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Binary
import Data.Typeable
import Control.Exception

data RelationalError = NoSuchAttributeNamesError (S.Set AttributeName)
                     | TupleAttributeCountMismatchError Int --attribute name
                     | EmptyAttributesError
                     | DuplicateAttributeNamesError (S.Set AttributeName)
                     | TupleAttributeTypeMismatchError Attributes
                     | AttributeCountMismatchError Int
                     | AttributeNamesMismatchError (S.Set AttributeName)
                     | AttributeNameInUseError AttributeName
                     | AttributeIsNotRelationValuedError AttributeName
                     | CouldNotInferAttributes
                     | RelVarNotDefinedError RelVarName
                     | RelVarAlreadyDefinedError RelVarName
                     | RelationTypeMismatchError Attributes Attributes --expected, found
                     | InclusionDependencyCheckError IncDepName
                     | InclusionDependencyNameInUseError IncDepName
                     | InclusionDependencyNameNotInUseError IncDepName
                     | ParseError T.Text
                     | PredicateExpressionError T.Text
                     | NoCommonTransactionAncestorError TransactionId TransactionId
                     | NoSuchTransactionError TransactionId
                     | RootTransactionTraversalError 
                     | HeadNameSwitchingHeadProhibitedError HeadName
                     | NoSuchHeadNameError HeadName
                     | UnknownHeadError
                     | NewTransactionMayNotHaveChildrenError TransactionId
                     | ParentCountTraversalError Int Int --maximum, requested
                     | NewTransactionMissingParentError TransactionId
                     | TransactionIsNotAHeadError TransactionId
                     | TransactionGraphCycleError TransactionId
                     | SessionIdInUseError TransactionId
                     | NoSuchSessionError TransactionId
                     | FailedToFindTransactionError TransactionId
                     | TransactionIdInUseError TransactionId
                     | NoSuchFunctionError AtomFunctionName
                     | NoSuchTypeConstructorName TypeConstructorName
                     | TypeConstructorAtomTypeMismatch TypeConstructorName AtomType
                     | AtomTypeMismatchError AtomType AtomType
                     | TypeConstructorNameMismatch TypeConstructorName TypeConstructorName
                     | AtomTypeTypeConstructorReconciliationError AtomType TypeConstructorName
                     | DataConstructorNameInUseError DataConstructorName
                     | DataConstructorUsesUndeclaredTypeVariable TypeVarName
                     | TypeConstructorTypeVarsMismatch (S.Set TypeVarName) (S.Set TypeVarName)
                     | TypeConstructorTypeVarMissing TypeVarName
                     | TypeConstructorTypeVarsTypesMismatch TypeConstructorName TypeVarMap TypeVarMap
                     | DataConstructorTypeVarsMismatch DataConstructorName TypeVarMap TypeVarMap
                     | AtomFunctionTypeVariableResolutionError AtomFunctionName TypeVarName
                     | AtomFunctionTypeVariableMismatch TypeVarName AtomType AtomType
                     | AtomTypeNameInUseError AtomTypeName
                     | IncompletelyDefinedAtomTypeWithConstructorError
                     | AtomTypeNameNotInUseError AtomTypeName
                     | AttributeNotSortableError Attribute
                     | FunctionNameInUseError AtomFunctionName
                     | FunctionNameNotInUseError AtomFunctionName
                     | EmptyCommitError
                     | FunctionArgumentCountMismatchError Int Int
                     | ConstructedAtomArgumentCountMismatchError Int Int
                     | NoSuchDataConstructorError DataConstructorName
                     | NoSuchTypeConstructorError TypeConstructorName
                     | InvalidAtomTypeName AtomTypeName
                     | AtomTypeNotSupported AttributeName --used by persistent driver
                     | AtomOperatorNotSupported T.Text --used by persistent driver
                     | EmptyTuplesError -- used by persistent driver
                     | AtomTypeCountError [AtomType] [AtomType]
                     | AtomFunctionTypeError AtomFunctionName Int AtomType AtomType --arg number
                     | AtomFunctionUserError AtomFunctionError
                     | PrecompiledFunctionRemoveError AtomFunctionName -- pre-compiled atom functions cannot be serialized, so they cannot change over time- they are referred to in perpetuity
                     | RelationValuedAttributesNotSupportedError [AttributeName]
                     | NotificationNameInUseError NotificationName
                     | NotificationNameNotInUseError NotificationName
                     | ImportError T.Text -- really? This should be broken out into some other error type- this has nothing to do with relational algebra
                     | ExportError T.Text
                     | UnhandledExceptionError String
                     | MergeTransactionError MergeError
                     | ScriptError ScriptCompilationError
                     | LoadFunctionError
                     | DatabaseContextFunctionUserError DatabaseContextFunctionError
                     | DatabaseLoadError PersistenceError
                       
                     | SubschemaNameInUseError SchemaName
                     | SubschemaNameNotInUseError SchemaName
                       
                     | SchemaCreationError SchemaError 
                       
                     | ImproperDatabaseStateError

                     | NonConcreteSchemaPlanError

                     | NoUncommittedContextInEvalError
                     | TupleExprsReferenceMultipleMarkersError
                       
                     | MultipleErrors [RelationalError]
                       deriving (Show,Eq,Generic,Binary,Typeable, NFData) 

data PersistenceError = InvalidDirectoryError FilePath | 
                        MissingTransactionError TransactionId |
                        WrongDatabaseFormatVersionError String String
                      deriving (Show, Eq, Generic, Binary, NFData)

--collapse list of errors into normal error- if there is just one, just return one
someErrors :: [RelationalError] -> RelationalError                                      
someErrors [] = error "no errors in error list: function misuse" 
someErrors errList  = if length errList == 1 then
                        head errList
                      else
                        MultipleErrors errList
                        
data MergeError = SelectedHeadMismatchMergeError |
                  PreferredHeadMissingMergeError HeadName |
                  StrategyViolatesConstraintMergeError |
                  InvalidMergeStrategyError MergeStrategy | -- this is an internal coding error
                  DisconnectedTransactionNotAMergeHeadError TransactionId |
                  StrategyViolatesComponentMergeError | --failed merge in inc deps, relvars, etc.
                  StrategyViolatesRelationVariableMergeError |
                  StrategyViolatesTypeConstructorMergeError
                  deriving (Show, Eq, Generic, Binary, Typeable)
                           
instance NFData MergeError where rnf = genericRnf                           
                                 
data ScriptCompilationError = TypeCheckCompilationError String String | --expected, got
                              SyntaxErrorCompilationError String |
                              ScriptCompilationDisabledError |
                              OtherScriptCompilationError String
                            deriving (Show, Eq, Generic, Binary, Typeable, NFData)
                                     
instance Exception ScriptCompilationError                                     
                                               
data SchemaError = RelVarReferencesMissing (S.Set RelVarName) |
                   RelVarInReferencedMoreThanOnce RelVarName |
                   RelVarOutReferencedMoreThanOnce RelVarName
                   deriving (Show, Eq, Generic, Binary, Typeable, NFData)
                           
                                               
-- errors returned from the distributed-process call handlers
data ServerError = RequestTimeoutError |
                   ProcessDiedError String
                   deriving (Generic, Binary, Eq)
