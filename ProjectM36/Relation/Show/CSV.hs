{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Relation.Show.CSV where
import ProjectM36.Base
import ProjectM36.Attribute
import Data.Csv
import ProjectM36.Tuple
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import ProjectM36.Error
import qualified Data.Text.Encoding as TE



--spit out error for relations without attributes (since relTrue and relFalse cannot be distinguished then as CSV) and for relations with relation-valued attributes
relationAsCSV :: Relation -> Either RelationalError BS.ByteString
relationAsCSV (Relation attrs tupleSet) = if relValAttrs /= [] then --check for relvalued attributes
                                            Left $ RelationValuedAttributesNotSupportedError (map attributeName relValAttrs)
                                            else if V.length attrs == 0 then --check that there is at least one attribute
                                                   Left $ TupleAttributeCountMismatchError 0
                                                 else
                                                   Right $ encodeByName bsAttrNames (asList tupleSet)
  where
    relValAttrs = V.toList $ V.filter (isRelationAtomType . atomType) attrs
    bsAttrNames = V.map (TE.encodeUtf8 . attributeName) attrs

{-
instance ToRecord RelationTuple where
  toRecord tuple = toRecord $ map toField (V.toList $ tupleAtoms tuple)
-}

instance ToNamedRecord RelationTuple where
  toNamedRecord tuple = namedRecord $ map (\(k,v) -> TE.encodeUtf8 k .= v) (tupleAssocs tuple)

instance DefaultOrdered RelationTuple where
  headerOrder tuple = V.map (TE.encodeUtf8 . attributeName) (tupleAttributes tuple)

instance ToField Atom where
  toField (Atom val) = (TE.encodeUtf8 . toText) val



