{-# Language TypeSynonymInstances,FlexibleInstances,OverloadedStrings #-} 
module ProjectM36.MyTest where
import ProjectM36.Base
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.QuickCheck.Arbitrary.ADT
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.Time
import Data.Time.Clock
import qualified Data.Vector as V
import qualified Data.Map as M

{-
instance Arbitrary Atom where
  arbitrary = IntAtom <$> int
    where int = (arbitrary :: Gen Int)

genAtom :: IO Atom
genAtom = do
  seed <- newQCGen
  return $ unGen (arbitrary :: Gen Atom) seed (2^29-1)
-}

instance Arbitrary T.Text where
  arbitrary = T.pack <$> elements ["Mary", "Johnny", "Sunny", "Ted"] --(arbitrary :: Gen String)

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> (arbitrary :: Gen Integer)

instance Arbitrary Attribute where
  arbitrary = genericArbitrary

instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = do
      maxbound <- choose (0,5)
      V.fromList <$> vectorOf maxbound arbitrary

instance {-# OVERLAPPING #-} Arbitrary TypeVarMap where
    arbitrary = undefined

instance Arbitrary UTCTime where
 arbitrary = UTCTime <$> arbitrary <*> (secondsToDiffTime <$> arbitrary)

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> (arbitrary :: Gen String)

-- arbitrary attributes seems to involve dependant-type techniques and seems unneccsary for a dud example.
attributeNames = ["Number","People","Time","Thing"]
atomtypes = [IntAtomType, TextAtomType, DayAtomType, TextAtomType]
attributes = V.fromList $ zipWith Attribute attributeNames atomtypes

instance Arbitrary RelationTuple where
  arbitrary = do
    a <- IntAtom <$> (arbitrary :: Gen Int)
    b <- TextAtom <$> (arbitrary :: Gen T.Text)
    c <- DayAtom <$> (arbitrary :: Gen Day)
    d <- TextAtom <$> (arbitrary :: Gen T.Text)
    let sample = V.fromList [a,b,c,d]
    return $ RelationTuple attributes sample

instance Arbitrary Relation where
  arbitrary = do
    list <- listOf (arbitrary :: Gen RelationTuple)
    return $ Relation attributes (RelationTupleSet list)


instance Arbitrary AtomType where
  arbitrary = genericArbitrary

instance Arbitrary Atom where
  arbitrary = genericArbitrary

instance ToADTArbitrary Atom


--three parts
-- s# generation
-- expr -> Relation
-- keyword arbitrary
-- database context: seed + expr
--
