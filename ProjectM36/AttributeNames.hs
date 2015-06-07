module ProjectM36.AttributeNames where
import qualified ProjectM36.Attribute as A
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Set as S
--AttributeNames is a data structure which can represent inverted projection attributes

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

attributeNameSet :: AttributeNames -> S.Set AttributeName
attributeNameSet (AttributeNames nameSet) = nameSet
attributeNameSet (InvertedAttributeNames nameSet) = nameSet

invertAttributeNames :: AttributeNames -> AttributeNames
invertAttributeNames (AttributeNames names) = InvertedAttributeNames names
invertAttributeNames (InvertedAttributeNames names) = AttributeNames names