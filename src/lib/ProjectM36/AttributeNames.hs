{-# LANGUAGE CPP #-}
module ProjectM36.AttributeNames where
import ProjectM36.Base
import qualified Data.Set as S
#if MIN_VERSION_base(4,20,0)
#else
import Data.Foldable (foldl')
#endif
--AttributeNames is a data structure which can represent inverted projection attributes and attribute names derived from relational expressions

empty :: AttributeNamesBase a
empty = AttributeNames S.empty

all :: AttributeNamesBase a
all = InvertedAttributeNames S.empty

-- | Coalesce a bunch of AttributeNames into a single AttributeNames.
some :: Eq a => [AttributeNamesBase a] -> AttributeNamesBase a
some [] = ProjectM36.AttributeNames.all
some [an] = an
some (a:as) = foldl' folder a as
  where
    folder :: Eq a => AttributeNamesBase a -> AttributeNamesBase a -> AttributeNamesBase a
    folder acc names =
      case acc of
        AttributeNames an | S.null an -> names
        acc' -> if names == empty then
                  acc
                else
                  UnionAttributeNames acc' names
      
