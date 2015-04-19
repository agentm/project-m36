module RelationalError where
import RelationType
import qualified Data.UUID as U
import qualified Data.Set as S


data RelationalError = NoSuchAttributeNameError String
                     | TupleAttributeCountMismatchError Int --attribute name
                     | TupleAttributeTypeMismatchError Attributes
                     | AttributeCountMismatchError Int
                     | AttributeNameMismatchError String
                     | AttributeNameInUseError AttributeName
                     | AttributeIsNotRelationValuedError AttributeName
                     | RelVarNotDefinedError String
                     | RelVarAlreadyDefinedError String
                     | RelVarAssignmentTypeMismatchError
                     | InclusionDependencyCheckError String
                     | ParseError String
                     | PredicateExpressionError String
                     | NoSuchTransactionError U.UUID
                     | NoSuchHeadNameError HeadName
                     | TransactionIsNotAHeadError U.UUID
                     | TransactionGraphCycleError U.UUID
                       deriving (Show,Eq) 


