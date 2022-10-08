{-# LANGUAGE GADTs,ExistentialQuantification #-}
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
--import qualified Control.Parallel.Strategies as P
import qualified ProjectM36.TypeConstructorDef as TCD
import qualified ProjectM36.DataConstructorDef as DCD
import qualified Data.Text as T
import Data.Either (isRight)
import System.Random.Shuffle
import Control.Monad.Random

attributes :: Relation -> Attributes
attributes (Relation attrs _ ) = attrs

tupleSet :: Relation -> RelationTupleSet
tupleSet (Relation _ tupSet) = tupSet

attributeNames :: Relation -> S.Set AttributeName
attributeNames (Relation attrs _) = A.attributeNameSet attrs

attributeForName :: AttributeName -> Relation -> Either RelationalError Attribute
attributeForName attrName (Relation attrs _) = A.attributeForName attrName attrs

attributesForNames :: S.Set AttributeName -> Relation -> Attributes
attributesForNames attrNameSet (Relation attrs _) = A.attributesForNames attrNameSet attrs

atomTypeForName :: AttributeName -> Relation -> Either RelationalError AtomType
atomTypeForName attrName (Relation attrs _) = A.atomTypeForAttributeName attrName attrs

mkRelationFromList :: Attributes -> [[Atom]] -> Either RelationalError Relation
mkRelationFromList attrs atomMatrix = do
  Relation attrs <$> mkTupleSetFromList attrs atomMatrix
  
emptyRelationWithAttrs :: Attributes -> Relation  
emptyRelationWithAttrs attrs = Relation attrs emptyTupleSet

mkRelation :: Attributes -> RelationTupleSet -> Either RelationalError Relation
mkRelation attrs tupleSet' =
    --check that all tuples have the same keys
    --check that all tuples have keys (1-N) where N is the attribute count
    case verifyTupleSet attrs tupleSet' of
      Left err -> Left err
      Right verifiedTupleSet -> return $ Relation attrs verifiedTupleSet
    
--less safe version of mkRelation skips verifyTupleSet
--useful for infinite or thunked tuple sets
--instead of returning a Left RelationalError, if a tuple does not match the relation's attributes, the tuple is simply removed
--duplicate tuples are NOT filtered by this creation method
mkRelationDeferVerify :: Attributes -> RelationTupleSet -> Either RelationalError Relation
mkRelationDeferVerify attrs tupleSet' = return $ Relation attrs (RelationTupleSet (filter tupleFilter (asList tupleSet')))
  where
    tupleFilter tuple = isRight (verifyTuple attrs tuple)
    
--return a relation of the same type except without any tuples
relationWithEmptyTupleSet :: Relation -> Relation    
relationWithEmptyTupleSet (Relation attrs _) = emptyRelationWithAttrs attrs

mkRelationFromTuples :: Attributes -> [RelationTuple] -> Either RelationalError Relation
mkRelationFromTuples attrs tupleSetList = do
   tupSet <- mkTupleSet attrs tupleSetList
   mkRelation attrs tupSet

relationTrue :: Relation
relationTrue = Relation A.emptyAttributes singletonTupleSet

relationFalse :: Relation
relationFalse = Relation A.emptyAttributes emptyTupleSet

--if the relation contains one tuple, return it, otherwise Nothing
singletonTuple :: Relation -> Maybe RelationTuple
singletonTuple rel@(Relation _ tupleSet') = if cardinality rel == Finite 1 then
                                         Just $ head $ asList tupleSet'
                                       else
                                         Nothing

-- this is still unncessarily expensive for (bigx union bigx) because each tuple is hashed and compared for equality (when the hashes match), but the major expense is attributesEqual, but we already know that all tuple attributes are equal (it's a precondition)
union :: Relation -> Relation -> Either RelationalError Relation
union (Relation attrs1 tupSet1) (Relation attrs2 tupSet2) =
  if not (A.attributesEqual attrs1 attrs2)
     then Left $ AttributeNamesMismatchError (A.attributeNameSet (A.attributesDifference attrs1 attrs2))
  else
    Right $ Relation attrs1 newtuples
  where
    newtuples = tupleSetUnion attrs1 tupSet1 tupSet2

project :: S.Set AttributeName -> Relation -> Either RelationalError Relation
project attrNames rel@(Relation _ tupSet) = do
  newAttrs <- A.projectionAttributesForNames attrNames (attributes rel)  
  newTupleList <- mapM (tupleProject newAttrs) (asList tupSet)
  pure (Relation newAttrs (RelationTupleSet (HS.toList (HS.fromList newTupleList))))

rename :: AttributeName -> AttributeName -> Relation -> Either RelationalError Relation
rename oldAttrName newAttrName rel@(Relation oldAttrs oldTupSet) 
  | not attributeValid = Left $ AttributeNamesMismatchError (S.singleton oldAttrName)
  | newAttributeInUse = Left $ AttributeNameInUseError newAttrName
  | otherwise = mkRelation newAttrs newTupSet
  where
    newAttributeInUse = A.attributeNamesContained (S.singleton newAttrName) (attributeNames rel)
    attributeValid = A.attributeNamesContained (S.singleton oldAttrName) (attributeNames rel)
    newAttrs = A.renameAttributes oldAttrName newAttrName oldAttrs
    newTupSet = RelationTupleSet $ map tupsetmapper (asList oldTupSet)
    tupsetmapper = tupleRenameAttribute oldAttrName newAttrName

--the algebra should return a relation of one attribute and one row with the arity
arity :: Relation -> Int
arity (Relation attrs _) = A.arity attrs

degree :: Relation -> Int
degree = arity

cardinality :: Relation -> RelationCardinality --we need to detect infinite tuple sets- perhaps with a flag
cardinality (Relation _ tupSet) = Finite (length (asList tupSet))

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
    groupAttr = Attribute newAttrName RelationAtomType (invertedAttributeNames groupAttrNames (attributes rel))
    nonGroupAttrNames = invertAttributeNames (attributes rel) groupAttrNames
    --map the projection to add the additional new attribute
    --create the new attribute (a new relation) by filtering and projecting the tupleSet
    folder tupleFromProjection acc = case acc of
      Left err -> Left err
      Right acc -> union acc (Relation newAttrs (HS.singleton (tupleExtend tupleFromProjection (matchingRelTuple tupleFromProjection))))
-}

--algorithm: self-join with image relation
group :: S.Set AttributeName -> AttributeName -> Relation -> Either RelationalError Relation
group groupAttrNames newAttrName rel = do
  let nonGroupAttrNames = A.nonMatchingAttributeNameSet groupAttrNames (S.fromList (V.toList (A.attributeNames (attributes rel))))
  nonGroupProjectionAttributes <- A.projectionAttributesForNames nonGroupAttrNames (attributes rel)
  groupProjectionAttributes <- A.projectionAttributesForNames groupAttrNames (attributes rel)
  let groupAttr = Attribute newAttrName (RelationAtomType groupProjectionAttributes)
      matchingRelTuple tupIn = case imageRelationFor tupIn rel of
        Right rel2 -> RelationTuple (A.singleton groupAttr) (V.singleton (RelationAtom rel2))
        Left _ -> undefined
      mogrifier tupIn = pure (tupleExtend tupIn (matchingRelTuple tupIn))
      newAttrs = A.addAttribute groupAttr nonGroupProjectionAttributes
  nonGroupProjection <- project nonGroupAttrNames rel
  relMogrify mogrifier newAttrs nonGroupProjection


--help restriction function
--returns a subrelation of
restrictEq :: RelationTuple -> Relation -> Either RelationalError Relation
restrictEq tuple = restrict rfilter
  where
    rfilter :: RelationTuple -> Either RelationalError Bool
    rfilter tupleIn = do
      pure (tupleIntersection tuple tupleIn == tuple)

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
    relFolder tupleIn acc = case acc of
        Left err -> Left err
        Right accRel -> do
                        ungrouped <- tupleUngroup relvalAttrName newAttrs tupleIn
                        accRel `union` ungrouped

--take an relval attribute name and a tuple and ungroup the relval
tupleUngroup :: AttributeName -> Attributes -> RelationTuple -> Either RelationalError Relation
tupleUngroup relvalAttrName newAttrs tuple = do
  relvalRelation <- relationForAttributeName relvalAttrName tuple
  let nonGroupAttrs = A.intersection newAttrs (tupleAttributes tuple)
  nonGroupTupleProjection <- tupleProject nonGroupAttrs tuple
  let folder tupleIn acc = case acc of
        Left err -> Left err
        Right accRel ->
          union accRel $ Relation newAttrs (RelationTupleSet [tupleExtend nonGroupTupleProjection tupleIn])
  relFold folder (Right $ Relation newAttrs emptyTupleSet) relvalRelation

attributesForRelval :: AttributeName -> Relation -> Either RelationalError Attributes
attributesForRelval relvalAttrName (Relation attrs _) = do
  atomType <- A.atomTypeForAttributeName relvalAttrName attrs
  case atomType of
    (RelationAtomType relAttrs) -> Right relAttrs
    _ -> Left $ AttributeIsNotRelationValuedError relvalAttrName

type RestrictionFilter = RelationTuple -> Either RelationalError Bool
restrict :: RestrictionFilter -> Relation -> Either RelationalError Relation
restrict rfilter (Relation attrs tupset) = do
  tuples <- filterM rfilter (asList tupset)
  Right $ Relation attrs (RelationTupleSet tuples)

--joins on columns with the same name- use rename to avoid this- base case: cartesian product
--after changing from string atoms, there needs to be a type-checking step!
--this is a "nested loop" scan as described by the postgresql documentation
join :: Relation -> Relation -> Either RelationalError Relation
join (Relation attrs1 tupSet1) (Relation attrs2 tupSet2) = do
  newAttrs <- A.joinAttributes attrs1 attrs2
  let tupleSetJoiner accumulator tuple1 = do
        joinedTupSet <- singleTupleSetJoin newAttrs tuple1 tupSet2
        return $ joinedTupSet ++ accumulator
  newTupSetList <- foldM tupleSetJoiner [] (asList tupSet1)
  Relation newAttrs <$> mkTupleSet newAttrs newTupSetList
  
-- | Difference takes two relations of the same type and returns a new relation which contains only tuples which appear in the first relation but not the second.
difference :: Relation -> Relation -> Either RelationalError Relation  
difference relA relB = 
  if not (A.attributesEqual (attributes relA) (attributes relB))
  then 
    Left $ AttributeNamesMismatchError (A.attributeNameSet (A.attributesDifference attrsA attrsB))
  else 
    restrict rfilter relA
  where
    attrsA = attributes relA
    attrsB = attributes relB
    rfilter tupInA = relFold (\tupInB acc -> if acc == Right False then pure False else pure (tupInB /= tupInA)) (Right True) relB
      
--a map should NOT change the structure of a relation, so attributes should be constant
relMap :: (RelationTuple -> Either RelationalError RelationTuple) -> Relation -> Either RelationalError Relation
relMap mapper (Relation attrs tupleSet') = 
  case forM (asList tupleSet') typeMapCheck of
    Right remappedTupleSet -> mkRelation attrs (RelationTupleSet remappedTupleSet)
    Left err -> Left err
  where
    typeMapCheck tupleIn = do
      remappedTuple <- mapper tupleIn
      if tupleAttributes remappedTuple == tupleAttributes tupleIn
        then Right remappedTuple
        else Left (TupleAttributeTypeMismatchError (A.attributesDifference (tupleAttributes tupleIn) attrs))

relMogrify :: (RelationTuple -> Either RelationalError RelationTuple) -> Attributes -> Relation -> Either RelationalError Relation
relMogrify mapper newAttributes (Relation _ tupSet) = do
  newTuples <- mapM (fmap (reorderTuple newAttributes) . mapper) (asList tupSet)
                    
  mkRelationFromTuples newAttributes newTuples

relFold :: (RelationTuple -> a -> a) -> a -> Relation -> a
relFold folder acc (Relation _ tupleSet') = foldr folder acc (asList tupleSet')

-- | Generate a randomly-ordered list of tuples from the relation.
toList :: Relation -> IO [RelationTuple]
toList rel = do 
  gen <- newStdGen
  let rel' = evalRand (randomizeTupleOrder rel) gen
  pure (relFold (:) [] rel')

--image relation as defined by CJ Date
--given tupleA and relationB, return restricted relation where tuple attributes are not the attribues in tupleA but are attributes in relationB and match the tuple's value

--check that matching attribute names have the same types
imageRelationFor ::  RelationTuple -> Relation -> Either RelationalError Relation
imageRelationFor matchTuple rel = do
  restricted <- restrictEq matchTuple rel --restrict across matching tuples
  let projectionAttrNames = A.nonMatchingAttributeNameSet (attributeNames rel) (tupleAttributeNameSet matchTuple)
  project projectionAttrNames restricted --project across attributes not in rel

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

-- | Return a Relation describing the types in the mapping.
typesAsRelation :: TypeConstructorMapping -> Either RelationalError Relation
typesAsRelation types = mkRelationFromTuples attrs tuples
  where
    attrs = A.attributesFromList [Attribute "TypeConstructor" TextAtomType,
                                Attribute "DataConstructors" dConsType]
    subAttrs = A.attributesFromList [Attribute "DataConstructor" TextAtomType]
    dConsType = RelationAtomType subAttrs
    tuples = map mkTypeConsDescription types
    
    mkTypeConsDescription (tCons, dConsList) =
      RelationTuple attrs (V.fromList [TextAtom (TCD.name tCons), mkDataConsRelation dConsList])
    
    mkDataConsRelation dConsList = case mkRelationFromTuples subAttrs $ map (\dCons -> RelationTuple subAttrs (V.singleton $ TextAtom $ T.intercalate " " (DCD.name dCons:map (T.pack . show) (DCD.fields dCons)))) dConsList of
      Left err -> error ("mkRelationFromTuples pooped " ++ show err)
      Right rel -> RelationAtom rel
      
-- | Randomly resort the tuples. This is useful for emphasizing that two relations are equal even when they are printed to the console in different orders.
randomizeTupleOrder :: MonadRandom m => Relation -> m Relation
randomizeTupleOrder (Relation attrs tupSet) = 
  Relation attrs . RelationTupleSet <$> shuffleM (asList tupSet)

-- returns a tuple from the tupleset- this is useful for priming folds over the tuples
oneTuple :: Relation -> Maybe RelationTuple
oneTuple (Relation _ (RelationTupleSet [])) = Nothing
oneTuple (Relation _ (RelationTupleSet (x:_))) = Just x

tuplesList :: Relation -> [RelationTuple]
tuplesList (Relation _ tupleSet') = asList tupleSet'
