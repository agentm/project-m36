module RelationTuple where
import RelationType
import RelationAttribute
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.HashSet as HS

tupleSize :: RelationTuple -> Int
tupleSize (RelationTuple m) = M.size m

tupleAttributeNameSet :: RelationTuple -> S.Set AttributeName
tupleAttributeNameSet (RelationTuple tupMap) = M.keysSet tupMap

tupleAtomForAttribute :: RelationTuple -> AttributeName -> Atom
tupleAtomForAttribute (RelationTuple tupMap) attrName = tupMap M.! attrName

{-instance Show RelationTuple where
  show (RelationTuple t) = show $ M.toAscList t-}
  
--in case the oldattr does not exist in the tuple, then return the old tuple
tupleRenameAttribute :: AttributeName -> AttributeName -> RelationTuple -> RelationTuple
tupleRenameAttribute oldattr newattr old@(RelationTuple tmap) = 
  if M.notMember oldattr tmap
     then old
  else
    RelationTuple newmap
  where
    newmap = M.insert newattr oldvalue deletedKeyMap
    oldvalue = tmap M.! oldattr
    deletedKeyMap = M.delete oldattr tmap

--in case the map does not contain the attributes, filter them out
mkRelationTuple :: S.Set AttributeName-> M.Map AttributeName Atom -> RelationTuple
mkRelationTuple attrs map = RelationTuple newMap
  where
    --make sure only attributes in the name set are in the tuple
    --check that Atoms are all of the same type- change to Either return value
    newMap = remapWithAttributes attrs map

mkRelationTuples :: Attributes -> [M.Map AttributeName Atom] -> [RelationTuple]
mkRelationTuples attrs maplist = map mapper maplist
  where
    mapper m = RelationTuple $ remapWithAttributes (attributeNameSet attrs) m

singleTupleSetJoin :: RelationTuple -> RelationTupleSet -> RelationTupleSet
singleTupleSetJoin tup tupSet = HS.foldr tupleJoiner HS.empty tupSet
  where
    tupleJoiner tuple2 accumulator = case joinedTuple of
      Nothing -> accumulator
      Just relTuple -> HS.insert relTuple accumulator
      where joinedTuple = singleTupleJoin tup tuple2

singleTupleJoin :: RelationTuple -> RelationTuple -> Maybe RelationTuple
singleTupleJoin (RelationTuple tup1) (RelationTuple tup2) = newTuple
  where
    newTuple = if M.isSubmapOfBy (==) matchingMap tup2 then
                     Just $ RelationTuple (M.union tup1 tup2)
                   else
                     Nothing
    matchingMap = M.intersection tup1 tup2 


tupleExtend :: RelationTuple -> RelationTuple -> RelationTuple
tupleExtend (RelationTuple map1) (RelationTuple map2) = RelationTuple $ M.union map1 map2

{- sort the associative list to match the header sorting -}    
tupleSortedAssocs :: RelationTuple -> [(AttributeName, Atom)]
tupleSortedAssocs (RelationTuple tupMap) = L.sortBy (\x y -> compare (fst x) (fst y)) (M.assocs tupMap)
                          
