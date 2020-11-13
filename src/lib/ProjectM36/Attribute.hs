module ProjectM36.Attribute where
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Hashable as Hash
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Data.Either

arity :: Attributes -> Int
arity = V.length

emptyAttributes :: Attributes
emptyAttributes = V.empty

null :: Attributes -> Bool
null = V.null

attributesFromList :: [Attribute] -> Attributes
attributesFromList = V.fromList -- . L.nub --too expensive

attributeName :: Attribute -> AttributeName
attributeName (Attribute name _) = name

atomType :: Attribute -> AtomType
atomType (Attribute _ atype) = atype

atomTypes :: Attributes -> V.Vector AtomType
atomTypes = V.map atomType

atomTypesList :: Attributes -> [AtomType]
atomTypesList = V.toList . atomTypes 

--hm- no error-checking here
addAttribute :: Attribute -> Attributes -> Attributes
addAttribute attr attrs = attrs `V.snoc` attr

--if some attribute names overlap but the types do not, then spit back an error
joinAttributes :: Attributes -> Attributes -> Either RelationalError Attributes
joinAttributes attrs1 attrs2 | V.length uniqueOverlappingAttributes /= V.length overlappingAttributes = Left (TupleAttributeTypeMismatchError overlappingAttributes)
                             | V.length overlappingAttrsDifferentTypes > 0 = Left (TupleAttributeTypeMismatchError overlappingAttrsDifferentTypes)
                             | otherwise = Right $ vectorUniqueify (attrs1 V.++ attrs2)
  where
    overlappingAttrsDifferentTypes = V.filter (\attr -> V.elem (attributeName attr) attrNames2 && V.notElem attr attrs2) attrs1
    attrNames2 = V.map attributeName attrs2
    uniqueOverlappingAttributes = vectorUniqueify overlappingAttributes
    overlappingAttributes = V.filter (`V.elem` attrs2) attrs1

addAttributes :: Attributes -> Attributes -> Attributes
addAttributes = (V.++)

deleteAttributeName :: AttributeName -> Attributes -> Attributes
deleteAttributeName attrName = V.filter (\attr -> attributeName attr /= attrName)

renameAttribute :: AttributeName -> Attribute -> Attribute
renameAttribute newAttrName (Attribute _ typeo) = Attribute newAttrName typeo

renameAttributes :: AttributeName -> AttributeName -> Attributes -> Attributes
renameAttributes oldAttrName newAttrName = V.map renamer
  where
    renamer attr = if attributeName attr == oldAttrName then
                     renameAttribute newAttrName attr
                   else
                     attr

atomTypeForAttributeName :: AttributeName -> Attributes -> Either RelationalError AtomType
atomTypeForAttributeName attrName attrs = do
  (Attribute _ atype) <- attributeForName attrName attrs
  return atype

attributeForName :: AttributeName -> Attributes -> Either RelationalError Attribute
attributeForName attrName attrs = case V.find (\attr -> attributeName attr == attrName) attrs of
  Nothing -> Left (NoSuchAttributeNamesError (S.singleton attrName))
  Just attr -> Right attr

isAttributeNameContained :: AttributeName -> Attributes -> Bool
isAttributeNameContained nam attrs = isRight (attributeForName nam attrs)

--similar to attributesForNames, but returns error if some names are missing  
projectionAttributesForNames :: S.Set AttributeName -> Attributes -> Either RelationalError Attributes
projectionAttributesForNames names attrsIn = 
  if not (S.null missingNames) then
    Left (NoSuchAttributeNamesError missingNames)
  else
    Right (attributesForNames names attrsIn)
  where
    missingNames = attributeNamesNotContained names (S.fromList (V.toList (attributeNames attrsIn)))

attributesForNames :: S.Set AttributeName -> Attributes -> Attributes
attributesForNames attrNameSet = V.filter filt
  where
    filt attr = S.member (attributeName attr) attrNameSet

attributeNameSet :: Attributes -> S.Set AttributeName
attributeNameSet attrVec = S.fromList $ V.toList $ V.map (\(Attribute name _) -> name) attrVec

attributeNames :: Attributes -> V.Vector AttributeName
attributeNames = V.map attributeName

--checks if set s1 is wholly contained in the set s2
attributesContained :: Attributes -> Attributes -> Bool
attributesContained attrs1 attrs2 = attributeNamesContained (attributeNameSet attrs1) (attributeNameSet attrs2)

attributeNamesContained :: S.Set AttributeName -> S.Set AttributeName -> Bool
attributeNamesContained = S.isSubsetOf

--returns the disjunction of the AttributeNameSets
nonMatchingAttributeNameSet :: S.Set AttributeName -> S.Set AttributeName -> S.Set AttributeName
nonMatchingAttributeNameSet a1 a2 = S.difference (S.union a1 a2) (S.intersection a1 a2)

matchingAttributeNameSet :: S.Set AttributeName -> S.Set AttributeName -> S.Set AttributeName
matchingAttributeNameSet = S.intersection

attributeNamesNotContained :: S.Set AttributeName -> S.Set AttributeName -> S.Set AttributeName
attributeNamesNotContained subset superset = S.filter (`S.notMember` superset) subset

-- useful for display
orderedAttributes :: Attributes -> [Attribute]
orderedAttributes attrs = L.sortBy (\a b -> attributeName a `compare` attributeName b) (V.toList attrs)

orderedAttributeNames :: Attributes -> [AttributeName]
orderedAttributeNames attrs = map attributeName (orderedAttributes attrs)

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
                           Left (TupleAttributeTypeMismatchError (attributesDifference collapsedAttrs attrs))
                         else
                           Right attrs
  where
    collapsedAttrs = vectorUniqueify attrs

attributesEqual :: Attributes -> Attributes -> Bool
attributesEqual attrs1 attrs2 =  V.null (attributesDifference attrs1 attrs2)

attributesAsMap :: Attributes -> M.Map AttributeName Attribute
attributesAsMap attrs = (M.fromList . V.toList) (V.map (\attr -> (attributeName attr, attr)) attrs)

-- | Left-biased union of attributes.
union :: Attributes -> Attributes -> Attributes
union attrsA attrsB = V.fromList (M.elems unioned)
  where
    unioned = M.union (attributesAsMap attrsA) (attributesAsMap attrsB)
                      
intersection :: Attributes -> Attributes -> Attributes
intersection attrsA attrsB = V.fromList (M.elems intersected)
  where
    intersected = M.intersection (attributesAsMap attrsA) (attributesAsMap attrsB)
