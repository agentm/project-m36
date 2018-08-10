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
import ProjectM36.Atom

--spit out error for relations without attributes (since relTrue and relFalse cannot be distinguished then as CSV) and for relations with relation-valued attributes
relationAsCSV :: Relation -> Either RelationalError BS.ByteString
relationAsCSV (Relation attrs tupleSet)  
 --check for relvalued attributes
  | relValAttrs /= [] = 
    Left $ RelationValuedAttributesNotSupportedError (map attributeName relValAttrs)
 --check that there is at least one attribute    
  | V.null attrs =
      Left $ TupleAttributeCountMismatchError 0
  | otherwise = 
    Right $ encodeByName bsAttrNames $ map RecordRelationTuple (asList tupleSet)
  where
    relValAttrs = V.toList $ V.filter (isRelationAtomType . atomType) attrs
    bsAttrNames = V.map (TE.encodeUtf8 . attributeName) attrs

{-
instance ToRecord RelationTuple where
  toRecord tuple = toRecord $ map toField (V.toList $ tupleAtoms tuple)
-}

newtype RecordRelationTuple = RecordRelationTuple {unTuple :: RelationTuple}

instance ToNamedRecord RecordRelationTuple where  
  toNamedRecord rTuple = namedRecord $ map (\(k,v) -> TE.encodeUtf8 k .= RecordAtom v) (tupleAssocs $ unTuple rTuple)
  
instance DefaultOrdered RecordRelationTuple where  
  headerOrder (RecordRelationTuple tuple) = V.map (TE.encodeUtf8 . attributeName) (tupleAttributes tuple)
  
newtype RecordAtom = RecordAtom {unAtom :: Atom}
      
instance ToField RecordAtom where
  toField (RecordAtom (TextAtom atomVal)) = TE.encodeUtf8 atomVal --without this, CSV text atoms are doubly quoted
  toField (RecordAtom atomVal) = (TE.encodeUtf8 . atomToText) atomVal

               
