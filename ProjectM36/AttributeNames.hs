module ProjectM36.AttributeNames where
import qualified ProjectM36.Attribute as A
import ProjectM36.Base
import qualified Data.Set as S
--AttributeNames is a data structure which can represent inverted projection attributes


projectionAttributesForAttributeNames :: Attributes -> AttributeNames -> Attributes
projectionAttributesForAttributeNames attrs (AttributeNames attrNameSet) = A.attributesForNames attrNameSet attrs
projectionAttributesForAttributeNames attrs (InvertedAttributeNames unselectedAttrNameSet) = A.attributesForNames (A.nonMatchingAttributeNameSet unselectedAttrNameSet (A.attributeNameSet attrs)) attrs

attributeNameSet :: AttributeNames -> S.Set AttributeName
attributeNameSet (AttributeNames nameSet) = nameSet
attributeNameSet (InvertedAttributeNames nameSet) = nameSet

invertAttributeNames :: AttributeNames -> AttributeNames
invertAttributeNames (AttributeNames names) = InvertedAttributeNames names
invertAttributeNames (InvertedAttributeNames names) = AttributeNames names