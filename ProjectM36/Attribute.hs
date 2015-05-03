module ProjectM36.Attribute where
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Hashable as Hash
import qualified Data.HashSet as HS
import Data.Maybe

arity :: Attributes -> Int
arity = V.length

emptyAttributes :: Attributes
emptyAttributes = V.empty

null :: Attributes -> Bool
null = V.null

--merges duplicates
attributesFromList :: [Attribute] -> Attributes
attributesFromList attrList = mergedAttributes
  where
    mergedAttributes = V.reverse $ V.foldr (\attr acc -> if containsAttributeName (attributeName attr) acc then
                                               acc
                                             else
                                               acc `V.snoc` attr
                               ) V.empty $ V.fromList attrList
    containsAttributeName name attrs = isJust $ V.find (\attr -> attributeName attr == name) attrs 

attributeName :: Attribute -> AttributeName
attributeName (Attribute name _) = name

isRelationAtomType :: AtomType -> Bool
isRelationAtomType (RelationAtomType _) = True
isRelationAtomType _ = False

--hm- no error-checking here
addAttribute :: Attribute -> Attributes -> Attributes
addAttribute attr attrs = attrs `V.snoc` attr

--if some attribute names overlap but the types do not, then spit back an error
joinAttributes :: Attributes -> Attributes -> Either RelationalError Attributes
joinAttributes attrs1 attrs2 = if V.length (vectorUniqueify overlappingAttributes) /= V.length overlappingAttributes then
                                 Left $ TupleAttributeTypeMismatchError overlappingAttributes
                               else
                                 Right $ attrs1 V.++ otherAttributes
  where
    (overlappingAttributes, otherAttributes) = V.partition (\attr -> V.elem attr attrs2) attrs1

addAttributes :: Attributes -> Attributes -> Attributes
addAttributes = (V.++)

deleteAttributeName :: AttributeName -> Attributes -> Attributes
deleteAttributeName attrName = V.filter (\attr -> attributeName attr /= attrName)

renameAttribute :: AttributeName -> Attribute -> Attribute                
renameAttribute newAttrName (Attribute _ typeo) = Attribute newAttrName typeo

renameAttributes :: AttributeName -> AttributeName -> Attributes -> Attributes
renameAttributes oldAttrName newAttrName attrs = V.map renamer attrs
  where
    renamer attr = if attributeName attr == oldAttrName then 
                     renameAttribute newAttrName attr
                   else  
                     attr
                     
atomTypeForAttributeName :: AttributeName -> Attributes -> Maybe AtomType
atomTypeForAttributeName attrName attrs = case attr of
  (Just (Attribute _ atomType)) -> Just atomType
  _ -> Nothing
  where 
    attr = attributeForName attrName attrs
                     
attributeForName :: AttributeName -> Attributes -> Maybe Attribute
attributeForName attrName attrs = V.find ((attrName ==) . attributeName) attrs

attributesForNames :: S.Set AttributeName -> Attributes -> Attributes
attributesForNames attrNameSet attrs = V.foldr folder V.empty attrs
  where
    folder attr acc = if S.member (attributeName attr) attrNameSet then
                        acc `V.snoc` attr
                      else
                        acc
                        
attributeNameSet :: Attributes -> S.Set AttributeName
attributeNameSet attrVec = S.fromList $ V.toList $ V.map (\(Attribute name _) -> name) attrVec

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
attributesDifference :: Attributes -> Attributes -> Attributes                           
attributesDifference attrsA attrsB = V.fromList $ diff (V.toList attrsA) (V.toList attrsB)
  where
    diff a b = (a L.\\ b)  ++ (b L.\\ a)
    
vectorUniqueify :: (Hash.Hashable a, Eq a) => V.Vector a -> V.Vector a
vectorUniqueify vecIn = V.fromList $ HS.toList $ HS.fromList $ V.toList vecIn

--check that each attribute only appears once
verifyAttributes :: Attributes -> Either RelationalError Attributes
verifyAttributes attrs = if collapsedAttrs /= attrs then
                           Left $ TupleAttributeTypeMismatchError (attributesDifference collapsedAttrs attrs)
                         else
                           Right attrs
  where
    collapsedAttrs = vectorUniqueify attrs
  
