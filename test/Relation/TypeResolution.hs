-- tests for type system- type variable resolution, deduction
import Test.HUnit
import ProjectM36.Base
import ProjectM36.RelationalExpression
import ProjectM36.Relation
import ProjectM36.Attribute
import ProjectM36.DatabaseContext
import Control.Monad.Trans.Reader
import Control.Monad.State hiding (join)
import System.Exit
import qualified Data.Map as M


testList :: Test
testList = TestList [testTypeWithoutTypeVariables,
                     testTypeWithOneTypeVariable]

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testTypeWithoutTypeVariables :: Test
testTypeWithoutTypeVariables = TestCase $ do
  --data TC1 = TD1 Int Text
  let t1TConsDef = ADTypeConstructorDef "TC1" []
      t1DConsDef = [DataConstructorDef "TD1" [DataConstructorDefTypeConstructorArg intTCons,
                                             DataConstructorDefTypeConstructorArg textTCons
                                            ]]
      t1Cons = ADTypeConstructor "TC1" [intTCons, textTCons]
      intTCons = PrimitiveTypeConstructor "Integer" IntegerAtomType
      textTCons = PrimitiveTypeConstructor "Text" TextAtomType

      addT1Cons = AddTypeConstructor t1TConsDef t1DConsDef
      (eRet, (ctx',_,_)) = runState (evalDatabaseContextExpr addT1Cons) (freshDatabaseState basicDatabaseContext)
      t1RelExpr = MakeRelationFromExprs attrExprs tupleExprs
      attrExprs = Just [AttributeAndTypeNameExpr "r" t1Cons ()]
      tupleExprs = [TupleExpr (M.singleton "r"
                              (ConstructedAtomExpr "TD1"
                               [NakedAtomExpr (IntegerAtom 3), NakedAtomExpr (TextAtom "t")] ()))]
      eRelExprResult = runReader (evalRelationalExpr t1RelExpr) (RelationalExprStateElems ctx')
      expectedRelAttrs = attributesFromList [Attribute "r" (ConstructedAtomType "TC1" M.empty)]
      expectedRel = mkRelationFromList expectedRelAttrs [[ConstructedAtom "TD1" (ConstructedAtomType "TC1" M.empty) [IntegerAtom 3,TextAtom "t"]]]
      

  assertEqual "Add TD1" (Right ()) eRet
  assertEqual "TD1 construction" expectedRel eRelExprResult

testTypeWithOneTypeVariable :: Test
testTypeWithOneTypeVariable = TestCase $ do
  --data T2C a = T2C Integer a
  let t2TConsDef = ADTypeConstructorDef "TC2" []
      t2DConsDef = [DataConstructorDef "TD2" [DataConstructorDefTypeConstructorArg intTCons,
                                             DataConstructorDefTypeVarNameArg "a"
                                            ]]
      t2Cons = ADTypeConstructor "TC2" [intTCons, textTCons]
      intTCons = PrimitiveTypeConstructor "Integer" IntegerAtomType
      textTCons = PrimitiveTypeConstructor "Text" TextAtomType      

      addT2Cons = AddTypeConstructor t2TConsDef t2DConsDef
      (eRet, (ctx',_,_)) = runState (evalDatabaseContextExpr addT2Cons) (freshDatabaseState basicDatabaseContext)
      t2RelExpr mAttrs = MakeRelationFromExprs mAttrs tupleExprs
      attrExprs = Just [AttributeAndTypeNameExpr "r" t2Cons ()]
      tupleExprs = [TupleExpr (M.singleton "r"
                              (ConstructedAtomExpr "TD2"
                               [NakedAtomExpr (IntegerAtom 3), NakedAtomExpr (TextAtom "t")] ()))]
      eRelWithAttrsExprResult = runReader (evalRelationalExpr (t2RelExpr attrExprs)) (RelationalExprStateElems ctx')
      eRelWithoutAttrsExprResult = runReader (evalRelationalExpr (t2RelExpr Nothing)) (RelationalExprStateElems ctx')      
      expectedRelAttrs = attributesFromList [Attribute "r" (ConstructedAtomType "TC2" (M.singleton "a" TextAtomType))]
      expectedRel = mkRelationFromList expectedRelAttrs [[ConstructedAtom "TD2" (ConstructedAtomType "TC2" M.empty) [IntegerAtom 3,TextAtom "t"]]]
      

  assertEqual "Add TD2" (Right ()) eRet
  assertEqual "TD2 construction with attributes" expectedRel eRelWithAttrsExprResult
  assertEqual "TD2 construction without attributes" expectedRel eRelWithoutAttrsExprResult  
  


--catch:
--data X a = X /phantom type
--data X = X a /unmentioned type variable
--data X = Text /re-used data constructor
