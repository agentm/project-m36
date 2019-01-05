import Test.HUnit
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Error
import ProjectM36.DateExamples
import ProjectM36.TupleSet
import ProjectM36.Attribute hiding (null, attributeNames)
import ProjectM36.DataTypes.Primitive
import ProjectM36.RelationalExpression
import ProjectM36.Tuple
import qualified ProjectM36.DatabaseContext as DBC
import Control.Monad.Trans.Reader
import qualified ProjectM36.Attribute as A
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import System.Exit


testList :: Test
testList = TestList [testRelation "relationTrue" relationTrue, testRelation "relationFalse" relationFalse,
                     testMkRelation1,
                     testRename1, testRename2,
                     testRelation "suppliers" suppliersRel,
                     testRelation "products" productsRel,
                     testRelation "supplierProducts" supplierProductsRel,
                     testMkRelationFromExprsBadAttrs,
                     testDuplicateAttributes,
                     testExistingRelationType
                    ]

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
                                                                              if tupleAttributes tuple == attrs && tupleAtomCheck tuple then 
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
    attrs = A.attributesFromList [Attribute "a" TextAtomType, Attribute "b" TextAtomType]
    tupleSet = RelationTupleSet [mkRelationTuple attrs (V.fromList [TextAtom "spam", TextAtom "spam2"])]
    
--rename tests
testRename1 :: Test
testRename1 = TestCase $ assertEqual "attribute invalid" (rename "a" "b" relationTrue) (Left $ AttributeNamesMismatchError (S.singleton "a"))

testRename2 :: Test
testRename2 = TestCase $ assertEqual "attribute in use" (rename "b" "a" simpleRel) (Left $ AttributeNameInUseError "a")

--mkRelation tests
--test tupleset key mismatch failure
testMkRelation1 :: Test
testMkRelation1 = TestCase $ assertEqual "key mismatch" expectedError (mkRelation testAttrs testTupSet) -- the error attribute set is empty due to an optimization- the tuple attrs do not match the atoms' types
  where
    expectedError = Left (AtomTypeMismatchError TextAtomType IntAtomType)
    testAttrs = A.attributesFromList [Attribute "a" TextAtomType]
    testTupSet = RelationTupleSet [mkRelationTuple testAttrs $ V.fromList [TextAtom "v"],
                                   mkRelationTuple testAttrs $ V.fromList [IntAtom 2]]

testMkRelationFromExprsBadAttrs :: Test
testMkRelationFromExprsBadAttrs = TestCase $ do
  let context = DBC.empty
  case runReader (evalRelationalExpr (MakeRelationFromExprs (Just [AttributeAndTypeNameExpr "badAttr1" (PrimitiveTypeConstructor "Int" IntAtomType) ()]) [TupleExpr (M.singleton "badAttr2" (NakedAtomExpr (IntAtom 1)))])) (mkRelationalExprState context) of
    Left err -> assertEqual "tuple type mismatch" (TupleAttributeTypeMismatchError (A.attributesFromList [Attribute "badAttr2" IntAtomType])) err
    Right _ -> assertFailure "expected tuple type mismatch"

--creating an empty relation with duplicate attribute names should fail
testDuplicateAttributes :: Test
testDuplicateAttributes = TestCase $ do
  let eRel = mkRelation attrs emptyTupleSet
      attrs = attributesFromList [Attribute "a" IntAtomType, Attribute "a" TextAtomType]
  assertEqual "duplicate attribute names" (Left (DuplicateAttributeNamesError (S.singleton "a"))) eRel
  
testExistingRelationType :: Test
testExistingRelationType = TestCase $ do
  let typeResult = runReader (typeForRelationalExpr (ExistingRelation relationTrue)) (RelationalExprStateElems DBC.empty)
  assertEqual "ExistingRelation with tuples type" (Right relationFalse) typeResult
