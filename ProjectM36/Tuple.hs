module ProjectM36.Tuple where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Attribute
import ProjectM36.Atom

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Data.Either (rights)

emptyTuple :: RelationTuple
emptyTuple = RelationTuple V.empty V.empty

tupleSize :: RelationTuple -> Int
tupleSize (RelationTuple tupAttrs _) = V.length tupAttrs

tupleAttributeNameSet :: RelationTuple -> S.Set AttributeName
tupleAttributeNameSet (RelationTuple tupAttrs _) = S.fromList $ V.toList $ V.map attributeName tupAttrs

tupleAttributes :: RelationTuple -> Attributes
tupleAttributes (RelationTuple tupAttrs _) = tupAttrs
    
-- return atoms in some arbitrary but consistent key order    
tupleAtoms :: RelationTuple -> V.Vector Atom
tupleAtoms (RelationTuple _ tupVec) = tupVec

atomForAttributeName :: AttributeName -> RelationTuple -> Either RelationalError Atom
atomForAttributeName attrName (RelationTuple tupAttrs tupVec) = case V.findIndex (\attr -> attributeName attr == attrName) tupAttrs of
  Nothing -> Left $ NoSuchAttributeNameError attrName
  Just index -> case tupVec V.!? index of 
    Nothing -> Left $ NoSuchAttributeNameError attrName
    Just atom -> Right atom

atomsForAttributeNames :: V.Vector AttributeName -> RelationTuple -> V.Vector Atom
atomsForAttributeNames attrNames tuple = V.foldr folder V.empty attrNames
  where
    folder attrName acc = case atomForAttributeName attrName tuple of
      Left _ -> acc
      Right atom -> V.snoc acc atom
      
relationForAttributeName :: AttributeName -> RelationTuple -> Either RelationalError Relation
relationForAttributeName attrName tuple = if maybe False isRelationAtomType (atomTypeForAttributeName attrName (tupleAttributes tuple)) then
                                        Left $ AttributeIsNotRelationValuedError attrName
                                      else do
                                        atomVal <- atomForAttributeName attrName tuple
                                        relationForAtom atomVal



--in case the oldattr does not exist in the tuple, then return the old tuple
tupleRenameAttribute :: AttributeName -> AttributeName -> RelationTuple -> RelationTuple
tupleRenameAttribute oldattr newattr (RelationTuple tupAttrs tupVec) = RelationTuple newAttrs tupVec
  where
    newAttrs = renameAttributes oldattr newattr tupAttrs

mkRelationTuple :: Attributes -> V.Vector Atom -> RelationTuple
mkRelationTuple attrs atoms = RelationTuple attrs atoms

mkRelationTuples :: Attributes -> [V.Vector Atom] -> [RelationTuple]
mkRelationTuples attrs atomsVec = map mapper atomsVec
  where
    mapper = mkRelationTuple attrs

singleTupleSetJoin :: RelationTuple -> RelationTupleSet -> RelationTupleSet
singleTupleSetJoin tup tupSet = HS.foldr tupleJoiner HS.empty tupSet
  where
    tupleJoiner tuple2 accumulator = case joinedTuple of
      Nothing -> accumulator
      Just relTuple -> HS.insert relTuple accumulator
      where joinedTuple = singleTupleJoin tup tuple2

-- the keys/values don't need to be resorted
-- if the keys share some keys and values, then merge the tuples
singleTupleJoin :: RelationTuple -> RelationTuple -> Maybe RelationTuple
singleTupleJoin tup1@(RelationTuple tupAttrs1 _) tup2@(RelationTuple tupAttrs2 _) = if 
  V.null keysIntersection || atomsForAttributeNames keysIntersection tup1 /= atomsForAttributeNames keysIntersection tup2
  then
    Nothing
  else 
    Just $ RelationTuple newAttrs newVec
  where
    keysIntersection = V.map attributeName attrsIntersection
    attrsIntersection = V.filter (flip V.notElem $ tupAttrs1) tupAttrs2
    newAttrs = vectorUnion tupAttrs1 tupAttrs2
    newVec = V.map (findAtomForAttributeName . attributeName) newAttrs
    --search both tuples for the 
    findAtomForAttributeName attrName = head $ rights $ fmap (atomForAttributeName attrName) [tup1, tup2]
    
--same consideration as Data.List.union- duplicates in v1 are not de-duped    
vectorUnion :: (Eq a) => V.Vector a -> V.Vector a -> V.Vector a
vectorUnion v1 v2 = V.foldr folder v1 v2
  where
    folder e acc = if V.elem e v1 then
                     acc
                   else
                     V.snoc acc e

--precondition- no overlap in attributes
tupleExtend :: RelationTuple -> RelationTuple -> RelationTuple
tupleExtend (RelationTuple tupAttrs1 tupVec1) (RelationTuple tupAttrs2 tupVec2) = RelationTuple newAttrs newVec
  where
    newAttrs = tupAttrs1 V.++ tupAttrs2
    newVec = tupVec1 V.++ tupVec2

{- sort the associative list to match the header sorting -}    
tupleSortedAssocs :: RelationTuple -> [(AttributeName, Atom)]
tupleSortedAssocs (RelationTuple tupAttrs tupVec) = V.toList $ V.map (\(index,attr) -> (attributeName attr, tupVec V.! index)) $ V.indexed tupAttrs
  
--this could be cheaper- it may not be wortwhile to update all the tuples for projection, but then the attribute management must be slightly different- perhaps the attributes should be a vector of association tuples [(name, index)]
tupleProject :: S.Set AttributeName -> RelationTuple -> RelationTuple    
tupleProject projectAttrs (RelationTuple attrs tupVec) = RelationTuple newAttrs newTupVec
  where
    deleteIndices = V.findIndices (\attr -> S.notMember (attributeName attr) projectAttrs) attrs
    indexDeleter = V.ifilter (\index _ -> V.notElem index deleteIndices)
    newAttrs = indexDeleter attrs
    newTupVec = indexDeleter tupVec

--return the attributes and atoms which are equal in both vectors
--semi-join
tupleIntersection :: RelationTuple -> RelationTuple -> RelationTuple
tupleIntersection tuple1 tuple2 = RelationTuple newAttrs newTupVec
  where
    attrs1 = tupleAttributes tuple1
    attrs2 = tupleAttributes tuple2
    matchingIndices = V.findIndices (\attr -> V.elem attr attrs2 && 
                                              atomForAttributeName (attributeName attr) tuple1 ==
                                              atomForAttributeName (attributeName attr) tuple2
                                    ) attrs1
    indexFilter = V.ifilter (\index _ -> V.elem index matchingIndices)
    newAttrs = indexFilter attrs1
    newTupVec = indexFilter (tupleAtoms tuple1)
    
updateTuple :: (M.Map AttributeName Atom) -> RelationTuple -> RelationTuple
updateTuple updateMap (RelationTuple attrs tupVec) = RelationTuple attrs newVec
  where
    updateKeysSet = M.keysSet updateMap
    updateKeysIVec = V.filter (\(_,attr) -> S.member (attributeName attr) updateKeysSet) (V.indexed attrs)
    newVec = V.update tupVec updateVec
    updateVec = V.map (\(index, attr) -> (index, updateMap M.! (attributeName attr))) updateKeysIVec

tupleToMap :: RelationTuple -> (M.Map AttributeName Atom)
tupleToMap (RelationTuple attrs tupVec) = M.fromList assocList
  where
    assocList = V.toList $ V.map (\(index, attr) -> (attributeName attr, tupVec V.! index)) (V.indexed attrs)
    
verifyTuple :: Attributes -> RelationTuple -> Either RelationalError RelationTuple
verifyTuple attrs tuple = let attrsTypes = V.map atomType attrs 
                              tupleTypes = V.map atomTypeForAtom (tupleAtoms tuple) in
  if V.length attrs /= V.length tupleTypes then
    Left $ TupleAttributeCountMismatchError 0
  else if attrsTypes /= tupleTypes then
         Left $ TupleAttributeTypeMismatchError (attributesDifference attrs (tupleAttributes tuple))
       else
         Right $ tuple


    