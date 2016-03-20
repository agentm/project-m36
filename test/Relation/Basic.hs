{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Error
import ProjectM36.DateExamples
import ProjectM36.DataTypes.Primitive
import ProjectM36.Tuple
import ProjectM36.Atom
import qualified ProjectM36.Attribute as A
import qualified Data.Vector as V
import qualified Data.Set as S
import System.Exit

testList :: Test
testList = TestList [testRelation "relationTrue" relationTrue, testRelation "relationFalse" relationFalse,
                     testMkRelation1,
                     testRename1, testRename2,
                     testRelation "suppliers" suppliersRel,
                     testRelation "products" productsRel,
                     testRelation "supplierProducts" supplierProductsRel]

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testRelation :: String -> Relation -> Test
testRelation testName rel = TestCase $ assertEqual testName relationValidation (Right rel)
  where
    relationValidation = validateRelation rel

-- run common relation checks
validateRelation :: Relation -> Either RelationalError Relation
validateRelation rel = do
  _ <- validateAttrNamesMatchTupleAttrNames rel
  validateAttrTypesMatchTupleAttrTypes rel
  
validateAttrNamesMatchTupleAttrNames :: Relation -> Either RelationalError Relation
validateAttrNamesMatchTupleAttrNames rel@(Relation _ tupSet) 
  | null invalidSet = Right rel
  | otherwise = Left $ AttributeNamesMismatchError S.empty
  where
    nameCheck tuple = attributeNames rel == tupleAttributeNameSet tuple
    invalidSet = filter (not . nameCheck) (asList tupSet)
    
validateAttrTypesMatchTupleAttrTypes :: Relation -> Either RelationalError Relation
validateAttrTypesMatchTupleAttrTypes rel@(Relation attrs tupSet) = foldr (\tuple acc -> 
                                                                              if (tupleAttributes tuple) == attrs && tupleAtomCheck tuple then 
                                                                                acc 
                                                                              else
                                                                                Left $ TupleAttributeTypeMismatchError A.emptyAttributes
                                                                            ) (Right rel) (asList tupSet)
  where
    tupleAtomCheck tuple = V.all (== True) (attrChecks tuple)
    attrChecks tuple = V.map (\attr -> case atomForAttributeName (A.attributeName attr) tuple of
                                 Left _ -> False
                                 Right atom -> (Right $ atomTypeForAtom atom) ==
                                  A.atomTypeForAttributeName (A.attributeName attr) attrs) (attributes rel)
    
simpleRel :: Relation    
simpleRel = case mkRelation attrs tupleSet of
  Right rel -> rel
  Left _ -> undefined
  where
    attrs = A.attributesFromList [Attribute "a" textAtomType, Attribute "b" textAtomType]
    tupleSet = RelationTupleSet [mkRelationTuple attrs (V.fromList [textAtom "spam", textAtom "spam2"])]

--rename tests
testRename1 :: Test
testRename1 = TestCase $ assertEqual "attribute invalid" (rename "a" "b" relationTrue) (Left $ AttributeNamesMismatchError (S.singleton "a"))

testRename2 :: Test
testRename2 = TestCase $ assertEqual "attribute in use" (rename "b" "a" simpleRel) (Left $ AttributeNameInUseError "a")

--mkRelation tests
--test tupleset key mismatch failure
testMkRelation1 :: Test
testMkRelation1 = TestCase $ assertEqual "key mismatch" (Left $ TupleAttributeTypeMismatchError A.emptyAttributes) (mkRelation testAttrs testTupSet) -- the error attribute set is empty due to an optimization- the tuple attrs do not match the atoms' types
  where
    testAttrs = A.attributesFromList [Attribute "a" textAtomType]
    testTupSet = RelationTupleSet [mkRelationTuple testAttrs $ V.fromList [textAtom "v"],
                                   mkRelationTuple testAttrs $ V.fromList [intAtom 2]]

