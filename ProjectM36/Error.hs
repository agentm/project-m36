{-# LANGUAGE DeriveGeneric #-}
module ProjectM36.Error where
import ProjectM36.Base
import qualified Data.Set as S
import qualified Data.UUID as U
import Control.DeepSeq (NFData, rnf)
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics (Generic)
import qualified Data.Text as T


data RelationalError = NoSuchAttributeNamesError (S.Set AttributeName)
                     | TupleAttributeCountMismatchError Int --attribute name
                     | TupleAttributeTypeMismatchError Attributes
                     | AttributeCountMismatchError Int
                     | AttributeNamesMismatchError (S.Set AttributeName)
                     | AttributeNameInUseError AttributeName
                     | AttributeIsNotRelationValuedError AttributeName
                     | RelVarNotDefinedError RelVarName
                     | RelVarAlreadyDefinedError RelVarName
                     | RelVarAssignmentTypeMismatchError Attributes Attributes --expected, found
                     | InclusionDependencyCheckError IncDepName
                     | InclusionDepedencyNameInUseError IncDepName
                     | ParseError T.Text
                     | PredicateExpressionError T.Text
                     | NoSuchTransactionError U.UUID
                     | NoSuchHeadNameError HeadName
                     | TransactionIsNotAHeadError U.UUID
                     | TransactionGraphCycleError U.UUID
                     | NoSuchTupleExprFunctionError AtomFunctionName
                     | AtomTypeMismatchError AtomType AtomType
                     | AtomTypeCountError [AtomType] [AtomType]
                     | AtomFunctionTypeError AtomFunctionName Int AtomType AtomType --arg number
                       deriving (Show,Eq,Generic) 


instance NFData RelationalError where rnf = genericRnf