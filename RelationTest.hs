import Test.HUnit
import RelationType
import Relation
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.HashSet as HS

testList = TestList [testRelation relationTrue, testRelation relationFalse]
main = runTestTT testList

testRelation :: Relation -> Test
testRelation rel = TestCase $ assertEqual "nope" relationValidation (Right rel)
  where
    relationValidation = validateRelation rel

-- run common relation checks
validateRelation :: Relation -> Either RelationalError Relation
validateRelation rel = do
  validateAttrNamesMatchTupleAttrNames rel
  validateAttrTypesMatchTupleAttrTypes rel
  
validateAttrNamesMatchTupleAttrNames :: Relation -> Either RelationalError Relation
validateAttrNamesMatchTupleAttrNames rel@(Relation _ tupMapSet) 
  | HS.null invalidSet = Right rel
  | otherwise = Left (RelationalError 1 "Tuple attributes do not match relation attributes")
  where
    nameCheck (RelationTuple tupMap) = attributeNames rel == M.keysSet tupMap
    relAttrNames = attributeNames rel
    invalidSet =  HS.filter (not . nameCheck) tupMapSet
    
    
validateAttrTypesMatchTupleAttrTypes :: Relation -> Either RelationalError Relation
validateAttrTypesMatchTupleAttrTypes rel@(Relation _ tupMapSet) 
   | HS.null invalidSet = Right rel
   | otherwise = Left (RelationalError 1 "Tuple types do not match relation attribute types")
  where
    tupleTypeCheck (RelationTuple tupMap) = M.null $ M.filterWithKey attrTypeCheck tupMap
    attrTypeCheck attrName atomValue = case attributeTypeForName attrName rel of
      Just t -> atomValueType atomValue == t
      _ -> False
    invalidSet = HS.filter (not . tupleTypeCheck) tupMapSet
    

