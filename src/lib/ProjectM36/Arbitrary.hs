{-# LANGUAGE ExistentialQuantification,FlexibleInstances,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectM36.Arbitrary where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AtomFunctionError
import ProjectM36.AtomType
import ProjectM36.Attribute (atomType)
import ProjectM36.DataConstructorDef as DCD
import ProjectM36.DataTypes.Interval
import ProjectM36.Relation
import qualified Data.Vector as V
import Data.Text (Text)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import qualified Data.ByteString.Char8 as B
import Data.Time
import Control.Monad.Reader
import Data.UUID

arbitrary' :: AtomType -> WithTCMap Gen (Either RelationalError Atom)
arbitrary' IntegerAtomType = 
  Right . IntegerAtom <$> lift (arbitrary :: Gen Integer)

arbitrary' (RelationAtomType attrs)  = do
  tcMap <-ask
  maybeRel <- lift $ runReaderT (arbitraryRelation attrs (0,5)) tcMap
  case maybeRel of
    Left err -> pure $ Left err
    Right rel -> pure $ Right $ RelationAtom rel

arbitrary' IntAtomType = 
  Right . IntAtom <$> lift (arbitrary :: Gen Int)

arbitrary' DoubleAtomType = 
  Right . DoubleAtom <$> lift (arbitrary :: Gen Double)

arbitrary' TextAtomType = 
  Right . TextAtom <$> lift (arbitrary :: Gen Text)

arbitrary' DayAtomType = 
  Right . DayAtom <$>  lift (arbitrary :: Gen Day)

arbitrary' DateTimeAtomType = 
  Right . DateTimeAtom <$> lift (arbitrary :: Gen UTCTime)
  
arbitrary' ByteStringAtomType =
  Right . ByteStringAtom <$> lift (arbitrary :: Gen B.ByteString)

arbitrary' BoolAtomType = 
  Right . BoolAtom <$> lift (arbitrary :: Gen Bool)

arbitrary' UUIDAtomType = 
  Right . UUIDAtom <$> lift (arbitrary :: Gen UUID)

arbitrary' RelationalExprAtomType =
  pure (Right (RelationalExprAtom (ExistingRelation relationTrue))) -- don't bother with arbitrary relational expressions

arbitrary' constructedAtomType@(ConstructedAtomType tcName tvMap)
  --special-casing for Interval type
  | isIntervalAtomType constructedAtomType = createArbitraryInterval (intervalSubType constructedAtomType)
  | otherwise = do 
  tcMap <- ask
  let maybeTCons = findTypeConstructor tcName tcMap
  let eitherTCons = maybeToRight (NoSuchTypeConstructorName tcName) maybeTCons
  let eitherDCDefs = snd <$> eitherTCons
  let eitherGenDCDef = elements <$> eitherDCDefs
  case eitherGenDCDef of
    Left err -> pure $ Left err
    Right genDCDef -> do
      dcDef <- lift genDCDef
      case resolvedAtomTypesForDataConstructorDefArgs tcMap tvMap dcDef of
        Left err -> pure $ Left err
        Right atomTypes -> do
          let genListOfEitherAtom = mapM (\aTy->runReaderT (arbitrary' aTy) tcMap) atomTypes
          listOfEitherAtom <- lift genListOfEitherAtom
          let eitherListOfAtom = sequence listOfEitherAtom
          case eitherListOfAtom of 
            Left err -> pure $ Left err
            Right listOfAtom -> pure $ Right $ ConstructedAtom (DCD.name dcDef) constructedAtomType listOfAtom
arbitrary' (TypeVariableType _) = error "arbitrary on type variable"

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

arbitraryRelationTuple :: Attributes -> WithTCMap Gen (Either RelationalError RelationTuple)
arbitraryRelationTuple attris = do
  tcMap <- ask
  listOfMaybeAType <- lift $ mapM ((\aTy -> runReaderT (arbitrary' aTy) tcMap) . atomType) (V.toList (attributesVec attris))
  case sequence listOfMaybeAType of
    Left err -> pure $ Left err
    Right listOfAttr -> do
      let vectorOfAttr = V.fromList listOfAttr
      pure $ Right $ RelationTuple attris vectorOfAttr

arbitraryWithRange :: Gen (Either RelationalError RelationTuple) -> Range -> Gen [Either RelationalError RelationTuple]
arbitraryWithRange genEitherTuple range = do
  num <- choose range
  vectorOf num genEitherTuple

arbitraryRelation :: Attributes -> Range -> WithTCMap Gen (Either RelationalError Relation)
arbitraryRelation attris range = do
  tcMap <- ask
  let genEitherTuple = runReaderT (arbitraryRelationTuple attris) tcMap
  listOfEitherTuple <- lift $ arbitraryWithRange genEitherTuple range
  let eitherTupleList = sequence listOfEitherTuple
  case eitherTupleList of
    Left err -> pure $ Left err
    Right tupleList ->  pure $ Right $ Relation attris $ RelationTupleSet tupleList

type WithTCMap a = ReaderT TypeConstructorMapping a 

createArbitraryInterval :: AtomType -> WithTCMap Gen (Either RelationalError Atom)
createArbitraryInterval subType = if supportsInterval subType then do
  eBegin <- arbitrary' subType
  eEnd <- arbitrary' subType
  beginopen <- lift (arbitrary :: Gen Bool)
  endopen <- lift (arbitrary :: Gen Bool)
  case eBegin of
    Left err -> pure (Left err)
    Right begin -> 
      case eEnd of
        Left err -> pure (Left err)
        Right end -> 
          case createInterval begin end beginopen endopen of
            Left _ -> createArbitraryInterval subType
            Right val -> pure (Right val)
  else
    pure $ Left (ProjectM36.Error.AtomFunctionUserError (AtomTypeDoesNotSupportIntervalError (prettyAtomType subType)))
