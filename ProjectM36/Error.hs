module ProjectM36.Error where
import ProjectM36.Base
import qualified Data.UUID as U

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


