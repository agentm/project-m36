module ProjectM36.Attribute where
import ProjectM36.Base
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

renameAttribute :: AttributeName -> Attribute -> Attribute                
renameAttribute newAttrName (Attribute _ typeo) = Attribute newAttrName typeo

remapWithAttributes :: S.Set AttributeName -> M.Map AttributeName Atom -> M.Map AttributeName Atom
remapWithAttributes attrs m = M.intersection m hollowMap
  where
    hollowMap = M.fromList $ zip (S.toList attrs) (repeat "")

attributeNameSet :: Attributes -> S.Set AttributeName
attributeNameSet = M.keysSet

--checks if set s1 is wholly contained in the set s2
attributesContained :: Attributes -> Attributes -> Bool
attributesContained attrs1 attrs2 = attributeNamesContained (attributeNameSet attrs1) (attributeNameSet attrs2)

attributeNamesContained :: S.Set AttributeName -> S.Set AttributeName -> Bool
attributeNamesContained attrs1 attrs2 = S.isSubsetOf attrs1 attrs2

--returns the disjunction of the AttributeNameSets
nonMatchingAttributeNameSet :: S.Set AttributeName -> S.Set AttributeName -> S.Set AttributeName
nonMatchingAttributeNameSet a1 a2 = S.difference (S.union a1 a2) (S.intersection a1 a2)

matchingAttributeNameSet :: S.Set AttributeName -> S.Set AttributeName -> S.Set AttributeName
matchingAttributeNameSet = S.intersection


-- this is sorted so the tuples know in which order to output- the ordering is arbitrary
sortedAttributeNameList :: S.Set AttributeName -> [AttributeName]
sortedAttributeNameList attrNameSet= L.sort $ S.toList attrNameSet
    
-- take two attribute sets and return an attribute set with the attributes which do not match
-- bug: missing attributes don't show up (?)
attributesDifference :: Attributes -> Attributes -> Attributes
attributesDifference attrA attrB = M.union mismatchA missingB
  where 
    mismatchA = M.foldrWithKey folder M.empty attrA
    missingB = M.difference attrA attrB
    folder :: AttributeName -> Attribute -> Attributes -> Attributes
    folder attrName attrinA acc = if M.notMember attrName attrB || M.lookup attrName attrB /= Just attrinA then
                           M.insert attrName attrinA acc 
                         else
                           acc