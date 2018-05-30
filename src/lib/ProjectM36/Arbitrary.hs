{-# LANGUAGE ExistentialQuantification,DeriveGeneric,DeriveAnyClass,TypeSynonymInstances,FlexibleInstances,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectM36.Arbitrary where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AtomType
import ProjectM36.Attribute (atomType)
import ProjectM36.DataConstructorDef as DCD
import qualified Data.Vector as V
import Data.Text (Text,pack)
import Test.QuickCheck
import qualified Data.ByteString.Char8 as B
import Data.Time
import Control.Monad.Reader

arbitrary' :: AtomType -> WithTCMap Gen (Either RelationalError Atom)
arbitrary' IntegerAtomType = do
  i <- lift (arbitrary :: Gen Integer)
  pure $ Right $ IntegerAtom i

arbitrary' (RelationAtomType attrs)  = do
  tcMap <-ask
  maybeRel <- lift $ runReaderT (arbitraryRelation attrs (0,5)) tcMap
  case maybeRel of
    Left err -> pure $ Left err
    Right rel -> pure $ Right $ RelationAtom rel

arbitrary' IntAtomType = do
  i <- lift (arbitrary :: Gen Int)
  pure $ Right $ IntAtom i

arbitrary' DoubleAtomType = do
  i <- lift (arbitrary :: Gen Double)
  pure $ Right $ DoubleAtom i

arbitrary' TextAtomType = do
  i <- lift (arbitrary :: Gen Text)
  pure $ Right $ TextAtom i

arbitrary' DayAtomType = do
  i <- lift (arbitrary :: Gen Day)
  pure $ Right $ DayAtom i

arbitrary' DateTimeAtomType = do
  i <- lift (arbitrary :: Gen UTCTime)
  pure $ Right $ DateTimeAtom i
  
arbitrary' ByteStringAtomType = do  
  bs <- lift (arbitrary :: Gen B.ByteString)
  pure $ Right $ ByteStringAtom bs

arbitrary' BoolAtomType = do
  i <- lift (arbitrary :: Gen Bool)
  pure $ Right $ BoolAtom i  

arbitrary' (IntervalAtomType atomTy) = do
  tcMap <- ask
  eitherAtomType <- lift $ runReaderT (arbitrary' atomTy) tcMap
  case eitherAtomType of
    Left err -> pure $ Left err
    Right atomType' -> do
      a <- lift $ (arbitrary :: Gen Bool)
      b <- lift $ (arbitrary :: Gen Bool)
      pure $ Right $ IntervalAtom atomType' atomType' a b

arbitrary' constructedAtomType@(ConstructedAtomType tcName tvMap) = do 
  tcMap <- ask
  let maybeTCons = findTypeConstructor tcName tcMap
  let eitherTCons = maybeToRight (NoSuchTypeConstructorName tcName) maybeTCons
  let eitherDCDefs = snd <$> eitherTCons
  let eitherGenDCDef = elements <$> eitherDCDefs
  case eitherGenDCDef of
    Left err -> pure $ Left err
    Right genDCDef -> do
      dcDef <- lift $ genDCDef
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

instance Arbitrary Text where
  arbitrary = pack <$> elements ["Mary", "Johnny", "Sunny", "Ted"]

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> (arbitrary :: Gen Integer)

instance Arbitrary UTCTime where
 arbitrary = UTCTime <$> arbitrary <*> (secondsToDiffTime <$> choose(0,86400))

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> (arbitrary :: Gen String)

arbitraryRelationTuple :: Attributes -> WithTCMap Gen (Either RelationalError RelationTuple)
arbitraryRelationTuple attris = do
  tcMap <- ask
  listOfMaybeAType <- lift $ sequence $  map ((\aTy -> runReaderT (arbitrary' aTy) tcMap) . atomType) (V.toList attris)
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
