import ProjectM36.Base
import Criterion.Main
import qualified ProjectM36.Attribute as A
import ProjectM36.Relation
import ProjectM36.Persist
import ProjectM36.RelationalExpression
import ProjectM36.Error
import ProjectM36.Transaction.Persist
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad.Trans.Reader
import qualified ProjectM36.DatabaseContext as DBC
import qualified Data.Set as S
import Data.Monoid
import System.IO.Temp
import System.FilePath
import System.Directory

{-
* create a relation
* restrict a relation
* project a relation
* join two relations 
-}

validate :: Either RelationalError a -> a
validate (Left err) = error (show err)
validate (Right x) = x

createRelation :: Int -> Int -> Either RelationalError Relation
createRelation attributeCount tupleCount = do
  let attrs = A.attributesFromList $ map (\c-> Attribute (T.pack $ "a" ++ show c) IntAtomType) [0 .. attributeCount-1]
      tuple tupleX = RelationTuple attrs (V.generate attributeCount (\_ -> IntAtom (fromIntegral tupleX)))
      tuples = map tuple [0 .. tupleCount]
  mkRelationDeferVerify attrs (RelationTupleSet tuples)

createRelation' :: Int -> Int -> Relation
createRelation' x y = validate (createRelation x y)

restrictRelationToOneTuple :: Int -> Relation ->  Relation
restrictRelationToOneTuple match rel = validate (runReader (evalRelationalExpr restriction) exprState)
 where
  exprState = mkRelationalExprState DBC.empty
  restriction = Restrict predicateMatch (ExistingRelation rel)
  predicateMatch = AttributeEqualityPredicate "a0" (NakedAtomExpr (IntAtom match))

restrictRelationToHalfRelation :: Int -> Relation -> Relation
restrictRelationToHalfRelation cutoff rel = validate (runReader (evalRelationalExpr restriction) exprState)
 where
  exprState = mkRelationalExprState DBC.basicDatabaseContext
  restriction = Restrict predicateMatch (ExistingRelation rel)
  predicateMatch = AtomExprPredicate (FunctionAtomExpr "lte" [AttributeAtomExpr "a0", NakedAtomExpr (IntAtom cutoff)] ())

projectRelationToAttributes :: AttributeNames -> Relation -> Relation
projectRelationToAttributes attrNames rel = validate (runReader (evalRelationalExpr projection) exprState)
 where
  exprState = mkRelationalExprState DBC.empty
  projection = Project attrNames (ExistingRelation rel)

unionRelations :: Relation -> Relation -> Relation
unionRelations relA relB = validate (union relA relB)

joinRelations :: Relation -> Relation -> Relation
joinRelations relA relB = validate (join relA relB)

groupRelation :: AttributeNames -> Relation -> Relation
groupRelation attrNames rel = validate (runReader (evalRelationalExpr (Group attrNames "x" (ExistingRelation rel))) exprState)
 where
  exprState = mkRelationalExprState DBC.empty

bigRelAttrNames :: Int -> Int -> AttributeNames
bigRelAttrNames start end = AttributeNames (S.fromList (map (\i -> "a" <> T.pack (show i)) [start .. end]))

main :: IO ()
main = do
 tmpDir <- getCanonicalTemporaryDirectory
 createDirectoryIfMissing False (tmpDir </> "relvars")
 defaultMain [createRel, restrictRel, projectRel, unionRel, joinRel, groupRel, writeRel tmpDir]
  where
  createRel = bgroup "create" $ map (\tupCount -> bench ("relation 10x" ++ show tupCount) (nf (createRelation' 10) tupCount)) [100, 1000, 10000]
  bigrel10000 = createRelation' 10 10000
  bigrel1000 = createRelation' 10 1000
  bigrel100 = createRelation' 10 100
  
  restrictRel = bgroup "restrict" [restrictOneTupleRel, restrictHalfTuplesRel]
  restrictOneTupleRel = bench "restrict relation 10x10000 to 10x1" (nf (restrictRelationToOneTuple 5000) bigrel10000)
  restrictHalfTuplesRel = bench "restrict relation 10x10000 to 10x5000" (nf (restrictRelationToHalfRelation 5000) bigrel10000)

  projectRel = bgroup "project" [projectOneAttr, projectHalfAttrs]
  projectOneAttr = bench "project 10x1000 to 1x1000" (nf (projectRelationToAttributes (bigRelAttrNames 0 0)) bigrel1000)
  projectHalfAttrs = bench "project 10x1000 to 5x1000" (nf (projectRelationToAttributes (bigRelAttrNames 0 4)) bigrel1000)

  unionRel = bgroup "union" [unionIdenticalRelations1000, unionIdenticalRelations10000]
  unionIdenticalRelations1000 = bench "union identical 10x1000" (nf (unionRelations bigrel1000) bigrel1000)
  unionIdenticalRelations10000 = bench "union identical 10x10000" (nf (unionRelations bigrel10000) bigrel10000)

  joinRel = bgroup "join" [joinIdenticalRelations100]
  joinIdenticalRelations100 = bench "join identical 10x100" (nf (joinRelations bigrel100) bigrel100)

  groupRel = bgroup "group" [group100]
  group100 = bench "group 10x100" (nf (groupRelation (bigRelAttrNames 1 9)) bigrel100)

  writeRel tmpDir = bgroup "write" [writeRel10000 tmpDir]
  writeRel10000 tmpDir = bench "write 10x1000" $ nfIO (writeRelVar FsyncDiskSync tmpDir ("x", bigrel10000))

  