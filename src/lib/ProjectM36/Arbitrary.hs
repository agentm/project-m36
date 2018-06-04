{-# LANGUAGE ExistentialQuantification,FlexibleInstances,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectM36.Arbitrary where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AtomFunctionError
import ProjectM36.AtomType
import ProjectM36.Attribute (atomType,attributeName)
import ProjectM36.DataConstructorDef as DCD
import ProjectM36.DataTypes.Interval
import qualified Data.Vector as V
import Data.Text (Text,pack)
import Test.QuickCheck
import qualified Data.ByteString.Char8 as B
import Data.Time
import Control.Monad.Reader

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

instance Arbitrary Text where
  arbitrary = pack <$> (elements $ map (replicate 3) ['A'..'Z'])

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> (arbitrary :: Gen Integer)

instance Arbitrary UTCTime where
 arbitrary = UTCTime <$> arbitrary <*> (secondsToDiffTime <$> choose(0,86400))

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> (arbitrary :: Gen String)

arbitraryRelationTuple :: Attributes -> WithTCMap Gen (Either RelationalError RelationTuple)
arbitraryRelationTuple attris = do
  tcMap <- ask
  listOfMaybeAType <- lift $ mapM ((\aTy -> runReaderT (arbitrary' aTy) tcMap) . atomType) (V.toList attris)
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
