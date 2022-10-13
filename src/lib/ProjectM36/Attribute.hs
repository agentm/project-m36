{-# OPTIONS_GHC -fno-warn-orphans #-}
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
arity a = V.length (attributesVec a)

instance Semigroup Attributes where
  attrsA <> attrsB =
    case joinAttributes attrsA attrsB of
      Left err -> error (show err)
      Right attrs' -> attrs'
    
instance Monoid Attributes where
  mempty = Attributes {
    attributesVec = mempty
    --,attributeSet = mempty
    }

emptyAttributes :: Attributes
emptyAttributes = mempty

null :: Attributes -> Bool
null a = V.null (attributesVec a)

singleton :: Attribute -> Attributes
singleton attr = Attributes {
  attributesVec = V.singleton attr
  --,attributesSet = HS.singleton attr
  }

toList :: Attributes -> [Attribute]
toList attrs = V.toList (attributesVec attrs)

attributesFromList :: [Attribute] -> Attributes
attributesFromList attrsL = Attributes {
  attributesVec = vec
  --,attributesSet = hset
  }
  where
    vec = if length attrsL == HS.size hset then
      --fast path- no duplicates
      V.fromList attrsL
      else
      --duplicate detected, uniqueify while maintaining original ordering
      V.fromList uniquedL
    hset = HS.fromList attrsL
    uniquedL = fst $ foldr (\attr acc@(l,s) ->
                              if HS.member attr s then
                                acc
                                else
                                (l ++ [attr], HS.insert attr s))
                              ([],mempty) attrsL

attributeName :: Attribute -> AttributeName
attributeName (Attribute name _) = name

atomType :: Attribute -> AtomType
atomType (Attribute _ atype) = atype

atomTypes :: Attributes -> V.Vector AtomType
atomTypes attrs = V.map atomType (attributesVec attrs)

atomTypesList :: Attributes -> [AtomType]
atomTypesList = V.toList . atomTypes 

addAttribute :: Attribute -> Attributes -> Attributes
addAttribute attr attrs = attrs <> singleton attr

--if some attribute names overlap but the types do not, then spit back an error
{-joinAttributes :: Attributes -> Attributes -> Either RelationalError Attributes
joinAttributes attrs1 attrs2 | V.length uniqueOverlappingAttributes /= V.length overlappingAttributes = Left (TupleAttributeTypeMismatchError overlappingAttributes)
                             | V.length overlappingAttrsDifferentTypes > 0 = Left (TupleAttributeTypeMismatchError overlappingAttrsDifferentTypes)
                             | otherwise = Right $ vectorUniqueify (attrs1 V.++ attrs2)
  where
    overlappingAttrsDifferentTypes = V.filter (\attr -> V.elem (attributeName attr) attrNames2 && V.notElem attr attrs2) attrs1
    attrNames2 = V.map attributeName attrs2
    uniqueOverlappingAttributes = vectorUniqueify overlappingAttributes
    overlappingAttributes = V.filter (`V.elem` attrs2) attrs1
-}
joinAttributes :: Attributes -> Attributes -> Either RelationalError Attributes
joinAttributes attrs1 attrs2 
  | S.size overlappingNames == 0 = -- fast path, no overlapping names
    pure (concated id)
  | attributesForNames overlappingNames attrs1 == attributesForNames overlappingNames attrs2 = -- that atomtypes match
    pure (concated vectorUniqueify)
  | otherwise =
    --special handling to validate that overlapping names have the same atom types
    Left (TupleAttributeTypeMismatchError (attributesForNames overlappingNames attrs1))
  where
    nameSet1 = attributeNameSet attrs1
    nameSet2 = attributeNameSet attrs2
    overlappingNames = S.intersection nameSet1 nameSet2
    concated f = Attributes {
      attributesVec = f (attributesVec attrs1 <> attributesVec attrs2)
      --,attributesSet = attributesSet attrs1 <> attributesSet attrs2
      }

{-
-- | Return the intersection of attributes. If the attributes share the same name, but not the same types, return an error.
intersection :: Attributes -> Attributes -> Either Relation Attributes
intersection attrsA attrsB =
  if overlappingAttrs attrsA  == overlappingAttrs attrsB then
    pure $ overlappingAttrs attrA
  else
    Left (TupleAttributeTypeMismatchError (attributesForNames overlappingNames attrsA))
  where
    nameSet1 = attributeNameSet attrsA
    nameSet2 = attributeNameSet attrsB
    overlappingNames = S.intersection nameSet1 nameSet2
    overlappingAttrs = attributesForNames overlappingNames
-}    

addAttributes :: Attributes -> Attributes -> Attributes
addAttributes = (<>)

member :: Attribute -> Attributes -> Bool
member attr attrs = HS.member attr (attributesSet attrs)

deleteAttributeName :: AttributeName -> Attributes -> Attributes
deleteAttributeName attrName = deleteAttributeNames (S.singleton attrName)

deleteAttributeNames :: S.Set AttributeName -> Attributes -> Attributes
deleteAttributeNames attrNames attrs = Attributes {
  attributesVec = vec
  }
  where
    vec = V.filter attrFilter (attributesVec attrs)
    attrFilter attr = S.notMember (attributeName attr) attrNames

renameAttribute :: AttributeName -> Attribute -> Attribute
renameAttribute newAttrName (Attribute _ typeo) = Attribute newAttrName typeo

renameAttributes :: AttributeName -> AttributeName -> Attributes -> Attributes
renameAttributes oldAttrName newAttrName attrs = Attributes {
  attributesVec = vec
  }
  where
  vec = V.map renamer (attributesVec attrs)
  renamer attr = if attributeName attr == oldAttrName then
                     renameAttribute newAttrName attr
                   else
                     attr

atomTypeForAttributeName :: AttributeName -> Attributes -> Either RelationalError AtomType
atomTypeForAttributeName attrName attrs = do
  (Attribute _ atype) <- attributeForName attrName attrs
  return atype

attributeForName :: AttributeName -> Attributes -> Either RelationalError Attribute
attributeForName attrName attrs = case V.find (\attr -> attributeName attr == attrName) (attributesVec attrs) of
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
attributesForNames attrNameSet attrs = Attributes {
  attributesVec = vec
  }
  where
    vec = V.filter filt (attributesVec attrs)
    filt attr = S.member (attributeName attr) attrNameSet

attributeNameSet :: Attributes -> S.Set AttributeName
attributeNameSet attrs = S.fromList $ V.toList $ V.map (\(Attribute name _) -> name) (attributesVec attrs)

attributeNames :: Attributes -> V.Vector AttributeName
attributeNames attrs = V.map attributeName (attributesVec attrs)

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
orderedAttributes attrs = L.sortBy (\a b -> attributeName a `compare` attributeName b) (V.toList (attributesVec attrs))

orderedAttributeNames :: Attributes -> [AttributeName]
orderedAttributeNames attrs = map attributeName (orderedAttributes attrs)

-- take two attribute sets and return an attribute set with the attributes which do not match
--this is the function which benefits the most from the HashSet representation- this turned up in the insert performance test
attributesDifference :: Attributes -> Attributes -> Attributes
{-attributesDifference attrsA attrsB = V.fromList $ diff (V.toList attrsA) (V.toList attrsB)
  where
    diff a b = (a L.\\ b)  ++ (b L.\\ a)
-}
attributesDifference attrsA attrsB =
  if attributesSet attrsA == attributesSet attrsB then
    mempty
    else
    Attributes {
    attributesVec = vec
    --,attributesSet = hset
    }
  where
    hset = HS.difference setA setB <> HS.difference setB setA
    setA = attributesSet attrsA
    setB = attributesSet attrsB
    vec = V.filter (`HS.member` hset) (attributesVec attrsA <> attributesVec attrsB)

vectorUniqueify :: (Hash.Hashable a, Eq a) => V.Vector a -> V.Vector a
vectorUniqueify vecIn = V.fromList $ HS.toList $ HS.fromList $ V.toList vecIn

--check that each attribute only appears once
verifyAttributes :: Attributes -> Either RelationalError Attributes
verifyAttributes attrs =
  if vecSet == attributesSet attrs then
    pure attrs
  else
    Left (TupleAttributeTypeMismatchError diffAttrs)
  where
    vecSet = V.foldr' HS.insert HS.empty (attributesVec attrs)
    diffSet = HS.difference vecSet (attributesSet attrs) <> HS.difference (attributesSet attrs) vecSet
    diffAttrs = Attributes {
      attributesVec = V.fromList (HS.toList diffSet)
      --,attributesSet = diffSet
      }

--used in Generics derivation for ADTs without named attributes- not to be used elsewhere
--drop first n attributes from vector representation
drop :: Int -> Attributes -> Attributes
drop c attrs = Attributes { attributesVec = vec
                          }
  where
    vec = V.drop c (attributesVec attrs)

    
-- use this in preference to attributesEqual when the attribute ordering matters such as during tuple unions
attributesAndOrderEqual :: Attributes -> Attributes -> Bool
attributesAndOrderEqual a b = attributesVec a == attributesVec b

-- use to determine if the same attributes are contained (but ordering is irrelevant)
attributesEqual :: Attributes -> Attributes -> Bool
attributesEqual attrsA attrsB =
  attributesVec attrsA == attributesVec attrsB || 
  attributesSet attrsA == attributesSet attrsB

attributesAsMap :: Attributes -> M.Map AttributeName Attribute
attributesAsMap attrs = V.foldr' (\attr acc -> M.insert (attributeName attr) attr acc) mempty (attributesVec attrs)


-- | Left-biased union of attributes.
union :: Attributes -> Attributes -> Attributes
union attrsA attrsB = Attributes {
  attributesVec = vec
  --,attributesSet = hset
  }
  where
    hset = HS.union (attributesSet attrsA) (attributesSet attrsB)
    vec = HS.foldr (flip V.snoc) mempty hset
                      
intersection :: Attributes -> Attributes -> Attributes
intersection attrsA attrsB = Attributes {
  attributesVec = vec
  --,attributesSet = hset
  }
  where
    hset = HS.intersection (attributesSet attrsA) (attributesSet attrsB)
    vec = HS.foldr (flip V.snoc) mempty hset
