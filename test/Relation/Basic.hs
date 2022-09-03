import Test.HUnit
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Error
import ProjectM36.DateExamples
import ProjectM36.DataTypes.Primitive
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.Tuple
import ProjectM36.Attribute (attributesFromList)
import qualified ProjectM36.DatabaseContext as DBC
import qualified ProjectM36.Attribute as A
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import System.Exit
import Data.Hashable (hashWithSalt)


testList :: Test
testList = TestList [testRelation "relationTrue" relationTrue, testRelation "relationFalse" relationFalse,
                     testMkRelation1,
                     testRename1, testRename2,
                     testRelation "suppliers" suppliersRel,
                     testRelation "products" productsRel,
                     testRelation "supplierProducts" supplierProductsRel,
                     testMkRelationFromExprsBadAttrs,
                     testExistingRelationType,
                     testReorderTuple,
                     testRelationEquality
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
                                 Right atom -> Right (atomTypeForAtom atom) ==
                                  A.atomTypeForAttributeName (A.attributeName attr) attrs) (attributesVec (attributes rel))
    
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
  (graph,_) <- freshTransactionGraph context  
  let reenv = mkRelationalExprEnv context graph
      reExpr = MakeRelationFromExprs (Just [AttributeAndTypeNameExpr "badAttr1" (PrimitiveTypeConstructor "Int" IntAtomType) ()]) (TupleExprs () [TupleExpr (M.singleton "badAttr2" (NakedAtomExpr (IntAtom 1)))])
      evald = runRelationalExprM reenv (evalRelationalExpr reExpr)
  case evald of
    Left err -> assertEqual "tuple type mismatch" (TupleAttributeTypeMismatchError (A.attributesFromList [Attribute "badAttr2" IntAtomType])) err
    Right _ -> assertFailure "expected tuple type mismatch"

testExistingRelationType :: Test
testExistingRelationType = TestCase $ do
  (graph, _) <- freshTransactionGraph dateExamples
  let typeResult = runRelationalExprM reenv (typeForRelationalExpr (ExistingRelation relationTrue))
      reenv = mkRelationalExprEnv dateExamples graph
  assertEqual "ExistingRelation with tuples type" (Right relationFalse) typeResult

-- | Ensure that tuple reordering honors the 
testReorderTuple :: Test
testReorderTuple = TestCase $ do
  let tup1 = mkRelationTuple attrs1 (V.fromList [IntAtom 4, TextAtom "test"])
      attrs1 = A.attributesFromList [Attribute "a" IntAtomType, Attribute "b" TextAtomType]
      attrs2 = A.attributesFromList [Attribute "b" TextAtomType, Attribute "a" IntAtomType]
      actual = reorderTuple attrs2 tup1
      expected = mkRelationTuple attrs2 (V.fromList [TextAtom "test", IntAtom 4])
  assertEqual "reorderTuple" expected actual

testRelationEquality :: Test
testRelationEquality = TestCase $ do
  -- relations with changed orders of tuples must hash to the same value and be equal, even in lieu of subrelations
  let r1 = Relation rattrs $ RelationTupleSet {asList = [RelationTuple subrelattrs
                                                        (V.fromList [RelationAtom subrel1])]
                                             }
      rattrs = attributesFromList [Attribute "x" (RelationAtomType subrelattrs)]

      r2 = Relation rattrs $ RelationTupleSet {asList = [RelationTuple subrelattrs
                                 (V.fromList [RelationAtom subrel2])]
                      }
      subrelattrs = attributesFromList [Attribute "y" IntAtomType]           
      subrel1 = Relation subrelattrs tupset1
      tupset1 = RelationTupleSet [RelationTuple subrelattrs (V.fromList [IntAtom 3]),
                                RelationTuple subrelattrs (V.fromList [IntAtom 4])]
      subrel2 = Relation subrelattrs tupset2
      tupset2 = RelationTupleSet [RelationTuple subrelattrs (V.fromList [IntAtom 4]),
                                  RelationTuple subrelattrs (V.fromList [IntAtom 3])]

  assertEqual "relation eq" r1 r2
  assertEqual "relation hash" (hashWithSalt 0 r1) (hashWithSalt 0 r2)
  assertEqual "tupset eq" tupset1 tupset2
  assertEqual "tupset hash" (hashWithSalt 0 tupset1) (hashWithSalt 0 tupset2)
