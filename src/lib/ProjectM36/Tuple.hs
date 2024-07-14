module ProjectM36.Tuple where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Attribute as A
import ProjectM36.Atom
import ProjectM36.AtomType
import ProjectM36.DataTypes.Primitive

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Either (rights)
import Control.Monad
import Control.Arrow
import Data.Maybe

emptyTuple :: RelationTuple
emptyTuple = RelationTuple mempty mempty

tupleSize :: RelationTuple -> Int
tupleSize (RelationTuple tupAttrs _) = arity tupAttrs

tupleAttributeNameSet :: RelationTuple -> S.Set AttributeName
tupleAttributeNameSet (RelationTuple tupAttrs _) = attributeNameSet tupAttrs

tupleAttributes :: RelationTuple -> Attributes
tupleAttributes (RelationTuple tupAttrs _) = tupAttrs

tupleAssocs :: RelationTuple -> [(AttributeName, Atom)]
tupleAssocs (RelationTuple attrs tupVec) = V.toList $ V.map (first attributeName) (V.zip (attributesVec attrs) tupVec)

orderedTupleAssocs :: RelationTuple -> [(AttributeName, Atom)]
orderedTupleAssocs tup@(RelationTuple attrVec _) = map (\attr -> (attributeName attr, atomForAttr (attributeName attr))) oAttrs
  where
    oAttrs = orderedAttributes attrVec
    atomForAttr nam = case atomForAttributeName nam tup of
      Left _ -> TextAtom "<?>"
      Right val -> val

-- return atoms in some arbitrary but consistent key order
tupleAtoms :: RelationTuple -> V.Vector Atom
tupleAtoms (RelationTuple _ tupVec) = tupVec

atomForAttributeName :: AttributeName -> RelationTuple -> Either RelationalError Atom
atomForAttributeName attrName (RelationTuple tupAttrs tupVec) = case V.findIndex (\attr -> attributeName attr == attrName) (attributesVec tupAttrs) of
  Nothing -> Left (NoSuchAttributeNamesError (S.singleton attrName))
  Just index -> case tupVec V.!? index of
    Nothing -> Left (NoSuchAttributeNamesError (S.singleton attrName))
    Just atom -> Right atom

atomsForAttributeNames :: V.Vector AttributeName -> RelationTuple -> Either RelationalError (V.Vector Atom)
atomsForAttributeNames attrNames tuple = 
  V.map (\index -> tupleAtoms tuple V.! index) <$> vectorIndicesForAttributeNames attrNames (tupleAttributes tuple)
      
vectorIndicesForAttributeNames :: V.Vector AttributeName -> Attributes -> Either RelationalError (V.Vector Int)
vectorIndicesForAttributeNames attrNameVec attrs = if not $ V.null unknownAttrNames then
                                                     Left $ NoSuchAttributeNamesError (S.fromList (V.toList unknownAttrNames))
                                                   else
                                                     Right $ V.map mapper attrNameVec
  where
    unknownAttrNames = V.filter (`V.notElem` attributeNames attrs) attrNameVec
    mapper attrName = fromMaybe (error "logic failure in vectorIndicesForAttributeNames") (V.elemIndex attrName (attributeNames attrs))

relationForAttributeName :: AttributeName -> RelationTuple -> Either RelationalError Relation
relationForAttributeName attrName tuple = do
  aType <- atomTypeForAttributeName attrName (tupleAttributes tuple)
  if not (isRelationAtomType aType) then
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
mkRelationTuple = RelationTuple 

mkRelationTuples :: Attributes -> [V.Vector Atom] -> [RelationTuple]
mkRelationTuples attrs = map mapper
  where
    mapper = mkRelationTuple attrs
    
mkRelationTupleFromMap :: M.Map AttributeName Atom -> RelationTuple
mkRelationTupleFromMap attrMap = RelationTuple (attributesFromList attrs) (V.map (attrMap M.!) (V.fromList attrNames))
  where
    attrNames = M.keys attrMap
    attrs = map (\attrName -> Attribute attrName (atomTypeForAtom (attrMap M.! attrName))) attrNames

--return error if attribute names match but their types do not
singleTupleSetJoin :: Attributes -> RelationTuple -> RelationTupleSet -> Either RelationalError [RelationTuple]
singleTupleSetJoin joinAttrs tup tupSet = 
    foldM tupleJoiner [] (asList tupSet) 
  where
    tupleJoiner :: [RelationTuple] -> RelationTuple -> Either RelationalError [RelationTuple]
    tupleJoiner accumulator tuple2 = case singleTupleJoin joinAttrs tup tuple2 of
        Right Nothing -> Right accumulator
        Right (Just relTuple) -> Right $ relTuple : accumulator
        Left err -> Left err
            
{-            
singleTupleSetJoin :: RelationTuple -> RelationTupleSet -> RelationTupleSet
singleTupleSetJoin tup1 tupSet = HS.union 
  where
    mapper tup2 = singleTupleJoin tup1 tup2
-}
            
-- if the keys share some keys and values, then merge the tuples
-- if there are shared attributes, if they match, create a new tuple from the atoms of both tuples based on the attribute ordering argument
singleTupleJoin :: Attributes -> RelationTuple -> RelationTuple -> Either RelationalError (Maybe RelationTuple)
singleTupleJoin joinedAttrs tup1@(RelationTuple tupAttrs1 _) tup2@(RelationTuple tupAttrs2 _) = if
  atomsForAttributeNames keysIntersection tup1 /= atomsForAttributeNames keysIntersection tup2
  then
    return Nothing
  else
    return $ Just $ RelationTuple joinedAttrs newVec
  where
    keysIntersection = V.map attributeName attrsIntersection
    attrsIntersection = V.filter (`V.elem` attributesVec tupAttrs1) (attributesVec tupAttrs2)
    newVec = V.map (findAtomForAttributeName . attributeName) (attributesVec joinedAttrs)
    --search both tuples for the attribute
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
    newAttrs = tupAttrs1 <> tupAttrs2
    newVec = tupVec1 <> tupVec2

tupleAtomExtend :: AttributeName -> Atom -> RelationTuple -> RelationTuple
tupleAtomExtend newAttrName atom tupIn = tupleExtend tupIn newTup
  where
    newTup = RelationTuple (A.singleton $ Attribute newAttrName (atomTypeForAtom atom)) (V.singleton atom)

{-tupleProject :: S.Set AttributeName -> RelationTuple -> RelationTuple
tupleProject projectAttrs (RelationTuple attrs tupVec) = RelationTuple newAttrs newTupVec
  where
    deleteIndices = V.findIndices (\attr -> S.notMember (attributeName attr) projectAttrs) (attributesVec attrs)
    indexDeleter = V.ifilter (\index _ -> V.notElem index deleteIndices)
    newAttrs = case A.projectionAttributesForNames projectAttrs attrs of
                 Left err -> error (show (err, projectAttrs, attrs))
                 Right attrs' ->  attrs'
    newTupVec = indexDeleter tupVec
-}
-- remember that the attributes order matters
tupleProject :: Attributes -> RelationTuple -> Either RelationalError RelationTuple
tupleProject projectAttrs tup = do
  newTupVec <- foldM (\acc attr ->
                        V.snoc acc <$> atomForAttributeName (attributeName attr) tup
                        ) V.empty (attributesVec projectAttrs)
  pure $ reorderTuple projectAttrs (RelationTuple projectAttrs newTupVec)
--return the attributes and atoms which are equal in both vectors
--semi-join
tupleIntersection :: RelationTuple -> RelationTuple -> RelationTuple
tupleIntersection tuple1 tuple2 = RelationTuple newAttrs newTupVec
  where
    attrs1 = tupleAttributes tuple1
    attrs2 = tupleAttributes tuple2
    intersectAttrs = A.intersection attrs1 attrs2
    matchingIndices = V.findIndices (\attr -> A.member attr intersectAttrs &&
                                    atomForAttributeName (attributeName attr) tuple1 ==
                                    atomForAttributeName (attributeName attr) tuple2) (attributesVec attrs1)
    indexFilter = V.ifilter (\index _ -> V.elem index matchingIndices)
    newAttrs = case A.projectionAttributesForNames (A.attributeNameSet intersectAttrs) attrs1 of
      Left _ -> mempty
      Right attrs' -> attrs'
    newTupVec = indexFilter (tupleAtoms tuple1)

-- | An optimized form of tuple update which updates vectors efficiently.
updateTupleWithAtoms :: M.Map AttributeName Atom -> RelationTuple -> RelationTuple
updateTupleWithAtoms updateMap (RelationTuple attrs tupVec) = RelationTuple attrs newVec
  where
    updateKeysSet = M.keysSet updateMap
    updateKeysIVec = V.filter (\(_,attr) -> S.member (attributeName attr) updateKeysSet) (V.indexed (attributesVec attrs))
    newVec = V.update tupVec updateVec
    updateVec = V.map (\(index, attr) -> (index, updateMap M.! attributeName attr)) updateKeysIVec

tupleToMap :: RelationTuple -> M.Map AttributeName Atom
tupleToMap (RelationTuple attrs tupVec) = M.fromList assocList
  where
    assocList = V.toList $ V.map (\(index, attr) -> (attributeName attr, tupVec V.! index)) (V.indexed (attributesVec attrs))

-- | Validate that the tuple has the correct attributes in the correct order
verifyTuple :: Attributes -> RelationTuple -> Either RelationalError RelationTuple
verifyTuple attrs tuple = let attrsTypes = A.atomTypes attrs
                              tupleTypes = V.map atomTypeForAtom (tupleAtoms tuple) in
  if A.arity attrs /= V.length tupleTypes then
    Left $ TupleAttributeCountMismatchError 0
  else do
    mapM_ (uncurry atomTypeVerify) (V.zip attrsTypes tupleTypes)
    Right tuple

--two tuples can be equal but the vectors of attributes could be out-of-order
--reorder if necessary- this is useful during relMogrify so that all the relation's tuples have identical atom/attribute ordering
reorderTuple :: Attributes -> RelationTuple -> RelationTuple
reorderTuple attrs tupIn = if attributesAndOrderEqual (tupleAttributes tupIn) attrs then
                             tupIn
                           else
                             RelationTuple attrs (V.map mapper (attributesVec attrs))
  where
    mapper attr = case atomForAttributeName (attributeName attr) tupIn of
      Left err -> error ("logic bug in reorderTuple: " ++ show err <> show tupIn)
      Right atom -> atom

--used in Generics derivation for ADTs without named attributes
trimTuple :: Int -> RelationTuple -> RelationTuple
trimTuple index (RelationTuple attrs vals) =
  RelationTuple newAttrs (V.drop index vals)
  where
    newAttrs = A.drop index attrs
  
  
  
