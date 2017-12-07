module ProjectM36.AttributeNames where
import ProjectM36.Base
import qualified Data.Set as S
--AttributeNames is a data structure which can represent inverted projection attributes and attribute names derived from relational expressions

empty :: AttributeNames
empty = AttributeNames S.empty

all :: AttributeNames
all = InvertedAttributeNames S.empty
