{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectM36.Relation.Representation.BTree where
import ProjectM36.Base
import ProjectM36.Relation
import qualified ProjectM36.Attribute as A
import ProjectM36.Tuple
import ProjectM36.RelExprSize

import Data.STree.BTree as BT

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Control.Monad.ST
import qualified Data.Vector.Algorithms.Intro as VAlg
import Control.Monad (foldM)
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.Data.StreamK as S

-- | Determine if the type represented by the Relation argument is suitable for BTree creation.
suitable :: Attributes -> Bool
suitable attrs =
  case A.toList attrs of
    [oneAttr] -> A.atomType oneAttr == IntAtomType
    _ -> False

construct :: Relation -> Maybe (BT.BTree 16 Int)
construct rel | suitable (attributes rel) = Nothing
              | otherwise = do
                  let folder acc tup =
                        case V.head (tupleAtoms tup) of
                          IntAtom i -> pure $ acc `UV.snoc` i
                          _ -> Nothing
                  vals <- foldM folder UV.empty (asList (tupleSet rel))
                  sortedVals <- pure $ runST $ do
                    mv <- UV.thaw vals
                    VAlg.sort mv
                    UV.freeze mv
                  case BT.build sortedVals of
                    Left _err -> Nothing
                    Right bt -> pure bt

instance Size (BT.BTree l Int) where
  size bt = fromIntegral $ sizeInBytes bt

-- optimization opportunity to get rid of intermediate structure by streaming directly from folding BTree.
toTupleStream :: (Monad m, LineWidth l) => Attributes -> BTree l Int -> S.Stream m RelationTuple
toTupleStream attrs bt = S.toStream $ S.fromFoldable (map (\v -> mkRelationTuple attrs (V.singleton (IntAtom v))) (BT.elems bt))
