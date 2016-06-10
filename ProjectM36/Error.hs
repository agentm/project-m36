{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.Error where
import ProjectM36.Base
import qualified Data.Set as S
import qualified Data.UUID as U
import Control.DeepSeq (NFData, rnf)
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Binary
import Data.Typeable

data RelationalError = NoSuchAttributeNamesError (S.Set AttributeName)
                     | TupleAttributeCountMismatchError Int --attribute name
                     | TupleAttributeTypeMismatchError Attributes
                     | AttributeCountMismatchError Int
                     | AttributeNamesMismatchError (S.Set AttributeName)
                     | AttributeNameInUseError AttributeName
                     | AttributeIsNotRelationValuedError AttributeName
                     | CouldNotInferAttributes
                     | RelVarNotDefinedError RelVarName
                     | RelVarAlreadyDefinedError RelVarName
                     | RelVarAssignmentTypeMismatchError Attributes Attributes --expected, found
                     | InclusionDependencyCheckError IncDepName
                     | InclusionDependencyNameInUseError IncDepName
                     | InclusionDependencyNameNotInUseError IncDepName
                     | ParseError T.Text
                     | PredicateExpressionError T.Text
                     | NoCommonTransactionAncestorError U.UUID U.UUID
                     | NoSuchTransactionError U.UUID
                     | NoSuchHeadNameError HeadName
                     | TransactionIsNotAHeadError U.UUID
                     | TransactionGraphCycleError U.UUID
                     | SessionIdInUseError U.UUID
                     | NoSuchSessionError U.UUID
                     | NoSuchTupleExprFunctionError AtomFunctionName
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
                     | AtomTypeNameInUseError AtomTypeName
                     | IncompletelyDefinedAtomTypeWithConstructorError
                     | AtomTypeNameNotInUseError AtomTypeName
                     | NoSuchDataConstructorError DataConstructorName
                     | NoSuchTypeConstructorError TypeConstructorName
                     | InvalidAtomTypeName AtomTypeName
                     | AtomTypeNotSupported AttributeName --used by persistent driver
                     | AtomOperatorNotSupported T.Text --used by persistent driver
                     | EmptyTuplesError -- used by persistent driver
                     | AtomTypeCountError [AtomType] [AtomType]
                     | AtomFunctionTypeError AtomFunctionName Int AtomType AtomType --arg number
                     | RelationValuedAttributesNotSupportedError [AttributeName]
                     | NotificationNameInUseError NotificationName
                     | NotificationNameNotInUseError NotificationName
                     | ImportError T.Text -- really? This should be broken out into some other error type- this has nothing to do with relational algebra
                     | ExportError T.Text
                     | MergeTransactionError MergeError
                     | MultipleErrors [RelationalError]
                       deriving (Show,Eq,Generic,Binary,Typeable) 

instance NFData RelationalError where rnf = genericRnf
                                      
data PersistenceError = InvalidDirectoryError FilePath | 
                        MissingTransactionError U.UUID
                      deriving (Show, Eq)

--collapse list of errors into normal error- if there is just one, just return one
someErrors :: [RelationalError] -> RelationalError                                      
someErrors [] = error "no errors in error list: function misuse" 
someErrors errList  = if length errList == 1 then
                        head errList
                      else
                        MultipleErrors errList
                        
data MergeError = SelectedHeadMismatchMergeError |
                  StrategyViolatesConstraintMergeError |
                  DisconnectedTransactionNotAMergeHeadError U.UUID
                  deriving (Show, Eq, Generic, Binary, Typeable)
                           
instance NFData MergeError where rnf = genericRnf                           