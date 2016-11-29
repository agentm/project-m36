module ProjectM36.AttributeNames where
import qualified ProjectM36.Attribute as A
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Set as S
--AttributeNames is a data structure which can represent inverted projection attributes

empty :: AttributeNames
empty = AttributeNames S.empty

all :: AttributeNames
all = InvertedAttributeNames S.empty

--check that the attribute names are actually in the attributes
projectionAttributesForAttributeNames :: Attributes -> AttributeNames -> Either RelationalError Attributes
projectionAttributesForAttributeNames attrs (AttributeNames attrNameSet) = do
  let nonExistentAttributeNames = A.attributeNamesNotContained attrNameSet (A.attributeNameSet attrs)
  if not $ S.null nonExistentAttributeNames then
    Left $ AttributeNamesMismatchError nonExistentAttributeNames
    else
      return $ A.attributesForNames attrNameSet attrs
projectionAttributesForAttributeNames attrs (InvertedAttributeNames unselectedAttrNameSet) = do
  let nonExistentAttributeNames = A.attributeNamesNotContained unselectedAttrNameSet (A.attributeNameSet attrs)
  if not $ S.null nonExistentAttributeNames then
    Left $ AttributeNamesMismatchError nonExistentAttributeNames
    else
      return $ A.attributesForNames (A.nonMatchingAttributeNameSet unselectedAttrNameSet (A.attributeNameSet attrs)) attrs      
projectionAttributesForAttributeNames attrs (UnionAttributeNames namesA namesB) = do
  attrsA <- projectionAttributesForAttributeNames attrs namesA
  attrsB <- projectionAttributesForAttributeNames attrs namesB
  pure (A.union attrsA attrsB)
      
invertAttributeNames :: AttributeNames -> AttributeNames
invertAttributeNames (AttributeNames names) = InvertedAttributeNames names
invertAttributeNames (InvertedAttributeNames names) = AttributeNames names
invertAttributeNames (UnionAttributeNames namesA namesB) = UnionAttributeNames (invertAttributeNames namesA) (invertAttributeNames namesB)
