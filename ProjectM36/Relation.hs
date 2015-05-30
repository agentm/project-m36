-- this is an implementation of the Relation typeclass which creates a GADT DSL
{-# LANGUAGE GADTs,ExistentialQuantification,NoImplicitPrelude,OverloadedStrings #-}
module ProjectM36.Relation where
import qualified Data.Set as S
import qualified Data.HashSet as HS
import Control.Monad
import qualified Data.Vector as V

import ProjectM36.Base
import ProjectM36.Tuple
import qualified ProjectM36.Attribute as A
import ProjectM36.TupleSet
import ProjectM36.Error
import Prelude hiding (join)
import qualified Data.Text as T
import qualified Control.Parallel.Strategies as P

attributes :: Relation -> Attributes
attributes (Relation attrs _ ) = attrs

attributeNames :: Relation -> S.Set AttributeName
attributeNames (Relation attrs _) = A.attributeNameSet attrs

attributeForName :: AttributeName -> Relation -> Either RelationalError Attribute
attributeForName attrName (Relation attrs _) = A.attributeForName attrName attrs

attributesForNames :: S.Set AttributeName -> Relation -> Attributes
attributesForNames attrNameSet (Relation attrs _) = A.attributesForNames attrNameSet attrs
    
atomTypeForName :: AttributeName -> Relation -> Either RelationalError AtomType
atomTypeForName attrName (Relation attrs _) = A.atomTypeForAttributeName attrName attrs
    
atomValueType :: Atom -> AtomType
atomValueType (StringAtom _) = StringAtomType
atomValueType (IntAtom _) = IntAtomType
atomValueType (RelationAtom rel) = RelationAtomType (attributes rel)

mkRelationFromList :: Attributes -> [[Atom]] -> Either RelationalError Relation
mkRelationFromList attrs atomMatrix = do
  tupSet <- mkTupleSetFromList attrs atomMatrix
  return $ Relation attrs tupSet

mkRelation :: Attributes -> RelationTupleSet -> Either RelationalError Relation
mkRelation attrs tupleSet = do
  --check that all tuples have the same keys
  --check that all tuples have keys (1-N) where N is the attribute count
  case verifyTupleSet attrs tupleSet of 
    Left err -> Left err
    Right verifiedTupleSet -> return $ Relation attrs verifiedTupleSet

relationTrue :: Relation
relationTrue = Relation A.emptyAttributes singletonTupleSet

relationFalse :: Relation
relationFalse = Relation A.emptyAttributes emptyTupleSet

madeTrue :: Either RelationalError Relation
madeTrue = mkRelation A.emptyAttributes (HS.fromList [])

madeFalse :: Either RelationalError Relation
madeFalse = mkRelation A.emptyAttributes emptyTupleSet

{-
--used for predicate filtering- instead of allowing for tuple types to be floating around in Tutorial D, just created a "singleton value" relation with cardinality and arity 1 for tuple atom comparison purposes
singletonValueFromRelation :: Relation -> Either RelationalError Atom
singletonValueFromRelation rel@(Relation _ tupSet) = if cardinality rel /= Countable 1 then    
                                                   Left (PredicateExpressionError "Relational expression must have cardinality 1")
                                                 else if arity rel /= 1 then
                                                        Left (PredicateExpressionError "Relation expression must have arity 1")
                                                      else
                                                        Right value
  where
    value = snd $ head $ M.toList tupMap
    tupMap = tup2Map $ head (HS.toList tupSet)
    tup2Map (RelationTuple tupmap) = tupmap
-}

union :: Relation -> Relation -> Either RelationalError Relation
union (Relation attrs1 tupSet1) (Relation attrs2 tupSet2) = 
  if not (attrs1 == attrs2) 
     then Left $ AttributeNameMismatchError (T.pack (show attrs1 ++ show attrs2))
  else
    Right $ Relation attrs1 newtuples
  where
    newtuples = HS.union tupSet1 tupSet2

project :: S.Set AttributeName -> Relation -> Either RelationalError Relation
project projectionAttrNames rel = 
  if not $ A.attributeNamesContained projectionAttrNames (attributeNames rel)
     then Left $ AttributeNameMismatchError ""
  else
    relFold folder (Right $ Relation newAttrs HS.empty) rel
  where
    newAttrs = attributesForNames projectionAttrNames rel
    folder :: RelationTuple -> Either RelationalError Relation -> Either RelationalError Relation
    folder tupleToProject acc = case acc of
      Left err -> Left err
      Right acc2 -> union acc2 (Relation newAttrs (HS.singleton (tupleProject projectionAttrNames tupleToProject)))
    
rename :: AttributeName -> AttributeName -> Relation -> Either RelationalError Relation
rename oldAttrName newAttrName rel@(Relation oldAttrs oldTupSet) =
  if not attributeValid
       then Left $ AttributeNameMismatchError ""
  else if newAttributeInUse
       then Left $ AttributeNameInUseError newAttrName
  else
    mkRelation newAttrs newTupSet
  where
    newAttributeInUse = A.attributeNamesContained (S.singleton newAttrName) (attributeNames rel)
    attributeValid = A.attributeNamesContained (S.singleton oldAttrName) (attributeNames rel)
    newAttrs = A.renameAttributes oldAttrName newAttrName oldAttrs
    newTupSet = HS.map tupsetmapper oldTupSet
    tupsetmapper tuple = tupleRenameAttribute oldAttrName newAttrName tuple
    
--the algebra should return a relation of one attribute and one row with the arity
arity :: Relation -> Int
arity (Relation attrs _) = A.arity attrs

cardinality :: Relation -> RelationCardinality 
cardinality (Relation _ tupSet) = Countable (HS.size tupSet)

--find tuples where the atoms in the relation which are NOT in the AttributeNameSet are equal
-- create a relation for each tuple where the attributes NOT in the AttributeNameSet are equal
--the attrname set attrs end up in the nested relation

--algorithm:
-- map projection of non-grouped attributes to restriction of matching grouped attribute tuples and then project on grouped attributes to construct the sub-relation
{-
group :: S.Set AttributeName -> AttributeName -> Relation -> Either RelationalError Relation
group groupAttrNames newAttrName rel@(Relation oldAttrs tupleSet) = do
  nonGroupProjection <- project nonGroupAttrNames rel 
  relFold folder (Right (Relation newAttrs emptyTupleSet)) nonGroupProjection
  where
    newAttrs = M.union (attributesForNames nonGroupAttrNames rel) groupAttr
    groupAttr = Attribute newAttrName RelationAtomType (attributesForNames nonGroupAttrNames rel)
    newAttrNameSet = S.insert newAttrName nonGroupAttrNames
    nonGroupAttrNames = S.difference groupAttrNames (attributeNames rel)
    --map the projection to add the additional new attribute
    --create the new attribute (a new relation) by filtering and projecting the tupleSet
    folder tupleFromProjection acc = case acc of 
      Left err -> Left err
      Right acc -> union acc (Relation newAttrs (HS.singleton (tupleExtend tupleFromProjection (matchingRelTuple tupleFromProjection))))
-}
  
--algorithm: self-join with image relation
group :: S.Set AttributeName -> AttributeName -> Relation -> Either RelationalError Relation
group groupAttrNames newAttrName rel = do
  nonGroupProjection <- project nonGroupAttrNames rel
  relMogrify mogrifier newAttrs nonGroupProjection
    where
      newAttrs = A.addAttribute groupAttr (attributesForNames nonGroupAttrNames rel)
      groupAttr = Attribute newAttrName (RelationAtomType (attributesForNames groupAttrNames rel))
      nonGroupAttrNames = A.nonMatchingAttributeNameSet groupAttrNames (attributeNames rel)
      mogrifier tupIn = tupleExtend (traceShowId tupIn) (matchingRelTuple tupIn)
      matchingRelTuple tupIn = case imageRelationFor tupIn rel of
        Right rel2 -> RelationTuple (V.singleton groupAttr) (V.singleton (RelationAtom rel2))
        Left _ -> undefined
{-
      folder tupleIn acc = case acc of
        Left err -> Left err
        Right acc2 -> union acc2 (Relation newAttrs (HS.singleton (tupleExtend tupleIn (matchingRelTuple tupleIn))))
      matchingRelTuple groupTuple = case imageRelationFor groupTuple rel of
        Right rel2 -> RelationTuple (V.singleton groupAttr) (V.singleton (RelationAtom rel2))
        Left _ -> undefined
-}
  
--help restriction function
--returns a subrelation of 
restrictEq :: RelationTuple -> Relation -> Either RelationalError Relation
restrictEq tuple rel = restrict rfilter rel
  where
    rfilter :: RelationTuple -> Bool
    rfilter tupleIn = tupleIntersection tuple tupleIn == tuple
    
-- unwrap relation-valued attribute
-- return error if relval attrs and nongroup attrs overlap
ungroup :: AttributeName -> Relation -> Either RelationalError Relation
ungroup relvalAttrName rel = case attributesForRelval relvalAttrName rel of
  Left err -> Left err
  Right relvalAttrs -> relFold relFolder (Right $ Relation newAttrs emptyTupleSet) rel
   where
    newAttrs = A.addAttributes relvalAttrs nonGroupAttrs
    nonGroupAttrs = A.deleteAttributeName relvalAttrName (attributes rel)
    relFolder :: RelationTuple -> Either RelationalError Relation -> Either RelationalError Relation
    relFolder tupleIn acc = case (tupleUngroup relvalAttrName newAttrs tupleIn) of 
      Right ungroupRelation -> case acc of
        Left err -> Left err
        Right accRel -> do
                        ungrouped <- tupleUngroup relvalAttrName newAttrs tupleIn
                        union accRel ungrouped
      Left err -> Left err
      
--take an relval attribute name and a tuple and ungroup the relval 
tupleUngroup :: AttributeName -> Attributes -> RelationTuple -> Either RelationalError Relation
tupleUngroup relvalAttrName newAttrs tuple = do
  relvalRelation <- relationForAttributeName relvalAttrName tuple
  relFold folder (Right $ Relation newAttrs emptyTupleSet) relvalRelation
  where
    nonGroupTupleProjection = tupleProject nonGroupAttrNames tuple
    nonGroupAttrNames = A.attributeNameSet newAttrs
    folder tupleIn acc = case acc of
      Left err -> Left err
      Right accRel -> union accRel $ Relation newAttrs (HS.singleton (tupleExtend nonGroupTupleProjection tupleIn))
          
attributesForRelval :: AttributeName -> Relation -> Either RelationalError Attributes
attributesForRelval relvalAttrName (Relation attrs _) = do 
  atomType <- A.atomTypeForAttributeName relvalAttrName attrs
  case atomType of 
    (RelationAtomType relAttrs) -> Right relAttrs
    _ -> Left $ AttributeIsNotRelationValuedError relvalAttrName

restrict :: (RelationTuple -> Bool) -> Relation -> Either RelationalError Relation
--restrict rfilter (Relation attrs tupset) = Right $ Relation attrs $ HS.filter rfilter tupset
restrict rfilter (Relation attrs tupset) = Right $ Relation attrs processedTupSet 
  where
    tupSetList = HS.toList tupset
    processedTupSet = HS.fromList ((filter rfilter tupSetList) `P.using` (P.parListChunk 1000 P.rdeepseq))

--joins on columns with the same name- use rename to avoid this- base case: cartesian product
--after changing from string atoms, there needs to be a type-checking step!
--this is a "nested loop" scan as described by the postgresql documentation
join :: Relation -> Relation -> Either RelationalError Relation
join (Relation attrs1 tupSet1) (Relation attrs2 tupSet2) = do
  let newTupSet = HS.foldr tupleSetJoiner emptyTupleSet tupSet1
      tupleSetJoiner tuple1 accumulator = HS.union (singleTupleSetJoin tuple1 tupSet2) accumulator
  newAttrs <- A.joinAttributes attrs1 attrs2
  return $ Relation newAttrs newTupSet
  
--a map should NOT change the structure of a relation, so attributes should be constant                 
relMap :: (RelationTuple -> RelationTuple) -> Relation -> Either RelationalError Relation
relMap mapper (Relation attrs tupleSet) = do
  case forM (HS.toList tupleSet) typeMapCheck of
    Right remappedTupleSet -> Right $ Relation attrs $ HS.fromList remappedTupleSet
    Left err -> Left err
  where
    typeMapCheck tupleIn = do
      let remappedTuple = mapper tupleIn
      if tupleAttributes remappedTuple == tupleAttributes tupleIn 
        then Right remappedTuple 
        else Left $ TupleAttributeTypeMismatchError (A.attributesDifference (tupleAttributes tupleIn) attrs)
      
relMogrify :: (RelationTuple -> RelationTuple) -> Attributes -> Relation -> Either RelationalError Relation
relMogrify mapper newAttributes (Relation _ tupSet) = mkRelation newAttributes (HS.map mapper tupSet)

relFold :: (RelationTuple -> a -> a) -> a -> Relation -> a
relFold folder acc (Relation _ tupleSet) = HS.foldr folder acc tupleSet

--image relation as defined by CJ Date
--given tupleA and relationB, return restricted relation where tuple attributes are not the attribues in tupleA but are attributes in relationB and match the tuple's value 

--check that matching attribute names have the same types
imageRelationFor ::  RelationTuple -> Relation -> Either RelationalError Relation
imageRelationFor matchTuple rel = do
  restricted <- restrictEq matchTuple rel --restrict across matching tuples
  project (A.nonMatchingAttributeNameSet (attributeNames rel) (tupleAttributeNameSet matchTuple)) restricted --project across attributes not in rel
  
    
--returns a relation-valued attribute image relation for each tuple in rel1
--algorithm: 
  {-
imageRelationJoin :: Relation -> Relation -> Either RelationalError Relation
imageRelationJoin rel1@(Relation attrNameSet1 tupSet1) rel2@(Relation attrNameSet2 tupSet2) = do
  Right $ Relation undefined
  where
    matchingAttrs = matchingAttributeNameSet attrNameSet1 attrNameSet2
    newAttrs = nonMatchingAttributeNameSet matchingAttrs $ S.union attrNameSet1 attrNameSet2
  
    tupleSetJoiner tup1 acc = undefined
-}
  
-- returns a relation with tupleCount tuples with a set of integer attributes attributesCount long
-- this is useful for performance and resource usage testing
matrixRelation :: Int -> Int -> Either RelationalError Relation
matrixRelation attributeCount tupleCount = do
  let attrs = A.attributesFromList $ map (\c-> Attribute (T.pack $ "a" ++ show c) IntAtomType) [0 .. attributeCount-1]
      tuple tupleX = RelationTuple attrs (V.generate attributeCount (\_ -> IntAtom tupleX))
      tupleSet = HS.fromList $ map (\c -> tuple c) [0 .. tupleCount]
  mkRelation attrs tupleSet




  