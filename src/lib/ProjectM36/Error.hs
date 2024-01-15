{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.Error where
import ProjectM36.Base
import ProjectM36.MerkleHash
import ProjectM36.DatabaseContextFunctionError
import ProjectM36.AtomFunctionError
import qualified Data.Set as S
import Control.DeepSeq (NFData, rnf)
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics (Generic)
import qualified Data.Text as T
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
                     | InclusionDependencyCheckError IncDepName (Maybe RelationalError)
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
                     | NoSuchFunctionError FunctionName
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
                     | AtomFunctionTypeVariableResolutionError FunctionName TypeVarName
                     | AtomFunctionTypeVariableMismatch TypeVarName AtomType AtomType
                     | AtomTypeNameInUseError AtomTypeName
                     | IncompletelyDefinedAtomTypeWithConstructorError
                     | AtomTypeNameNotInUseError AtomTypeName
                     | AttributeNotSortableError Attribute
                     | FunctionNameInUseError FunctionName
                     | FunctionNameNotInUseError FunctionName
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
                     | AtomFunctionTypeError FunctionName Int AtomType AtomType --arg number
                     | AtomFunctionUserError AtomFunctionError
                     | PrecompiledFunctionRemoveError FunctionName -- pre-compiled atom functions cannot be serialized, so they cannot change over time- they are referred to in perpetuity
                     | RelationValuedAttributesNotSupportedError [AttributeName]
                     | NotificationNameInUseError NotificationName
                     | NotificationNameNotInUseError NotificationName
                     | ImportError ImportError'
                     | ExportError T.Text
                     | UnhandledExceptionError String
                     | MergeTransactionError MergeError
                     | ScriptError ScriptCompilationError
                     | LoadFunctionError
                     | SecurityLoadFunctionError
                     | DatabaseContextFunctionUserError DatabaseContextFunctionError
                     | DatabaseLoadError PersistenceError
                       
                     | SubschemaNameInUseError SchemaName
                     | SubschemaNameNotInUseError SchemaName
                       
                     | SchemaCreationError SchemaError 
                       
                     | ImproperDatabaseStateError

                     | NonConcreteSchemaPlanError

                     | NoUncommittedContextInEvalError
                     | TupleExprsReferenceMultipleMarkersError

                     | MerkleHashValidationError TransactionId MerkleHash MerkleHash

                     | RegisteredQueryValidationError RegisteredQueryName RelationalError
                     | RegisteredQueryNameInUseError RegisteredQueryName
                     | RegisteredQueryNameNotInUseError RegisteredQueryName

                     | MultipleErrors [RelationalError]
                       deriving (Show,Eq,Generic,Typeable, NFData) 

data PersistenceError = InvalidDirectoryError FilePath | 
                        MissingTransactionError TransactionId |
                        WrongDatabaseFormatVersionError String String
                      deriving (Show, Eq, Generic, NFData)

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
                  StrategyViolatesRelationVariableMergeError RelationalError |
                  StrategyWithoutPreferredBranchResolutionMergeError |
                  StrategyViolatesTypeConstructorMergeError |
                  StrategyViolatesRegisteredQueryMergeError [RegisteredQueryName]
                  deriving (Show, Eq, Generic, Typeable)
                           
instance NFData MergeError where rnf = genericRnf                           
                                 
data ScriptCompilationError = TypeCheckCompilationError String String | --expected, got
                              SyntaxErrorCompilationError String |
                              ScriptCompilationDisabledError |
                              OtherScriptCompilationError String
                            deriving (Show, Eq, Generic, Typeable, NFData)
                                     
instance Exception ScriptCompilationError                                     
                                               
data SchemaError = RelVarReferencesMissing (S.Set RelVarName) |
                   RelVarInReferencedMoreThanOnce RelVarName |
                   RelVarOutReferencedMoreThanOnce RelVarName
                   deriving (Show, Eq, Generic, Typeable, NFData)


data ImportError' = InvalidSHA256Error T.Text
                  | SHA256MismatchError T.Text T.Text
                  | InvalidFileURIError T.Text
                  | ImportFileDecodeError T.Text
                  | ImportFileError T.Text
                  | ImportDownloadError T.Text
                  deriving (Show, Eq, Generic, Typeable, NFData)
