{-# LANGUAGE ExistentialQuantification,DeriveGeneric,DeriveAnyClass,TypeSynonymInstances,FlexibleInstances,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectM36.Arbitrary where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AtomType
import ProjectM36.Attribute (atomType,attributeName)
import ProjectM36.DataConstructorDef as DCD
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Text (Text,unpack,pack)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.QuickCheck.Arbitrary
import qualified Data.ByteString.Char8 as B
import Data.Time
import Data.Time.Clock
import Debug.Trace
import Control.Monad.Reader
import Data.Either
import Data.Either.Combinators

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

arbitrary' BoolAtomType = do
  i <- lift (arbitrary :: Gen Bool)
  pure $ Right $ BoolAtom i

arbitrary' (IntervalAtomType atomTy) = do
  tcMap <- ask
  eitherAtomTypeA <- lift $ runReaderT (arbitrary' atomTy) tcMap
  eitherAtomTypeB <- lift $ runReaderT (arbitrary' atomTy) tcMap
  case (eitherAtomTypeA,eitherAtomTypeB) of
    (Right a, Right b) -> do
      l <- lift $ (arbitrary :: Gen Bool)
      r <- lift $ (arbitrary :: Gen Bool)
      pure $ Right $ IntervalAtom a b l r
    (Left err,_)  -> pure $ Left err
    (Right _, Left err) -> pure $ Left err

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
arbitrary' (TypeVariableType tvName) = undefined

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
