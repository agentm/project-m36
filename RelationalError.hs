module RelationalError where
import qualified Control.Monad.Error as Err
import RelationType

data RelationalError = NoSuchAttributeNameError String
                     | TupleAttributeCountMismatchError Int
                     | TupleAttributeTypeMismatchError Int
                     | AttributeCountMismatchError Int
                     | AttributeNameMismatchError String
                     | AttributeNameInUseError AttributeName
                     | AttributeIsNotRelationValuedError AttributeName
                     | RelvarNotDefinedError String
                     | ParseError String
                       deriving (Show,Eq) 

instance Err.Error RelationalError where

