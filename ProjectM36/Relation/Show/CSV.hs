module ProjectM36.Relation.Show.CSV where
import ProjectM36.Base
import ProjectM36.Attribute
import Data.Csv
import ProjectM36.Tuple
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import ProjectM36.Error
import qualified Data.Text.Encoding as TE
{-# OPTIONS_GHC -fno-warn-orphans #-}

--spit out error for relations without attributes (since relTrue and relFalse cannot be distinguished then as CSV) and for relations with relation-valued attributes
relationAsCSV :: Relation -> Either RelationalError BS.ByteString
relationAsCSV (Relation attrs tupleSet) = if relValAttrs /= [] then --check for relvalued attributes
                                            Left $ RelationValuedAttributesNotSupportedError (map attributeName relValAttrs)
                                            else if V.length attrs == 0 then --check that there is at least one attribute
                                                   Left $ TupleAttributeCountMismatchError 0
                                                 else
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
  toNamedRecord rTuple = namedRecord $ map (\(k,v) -> TE.encodeUtf8 k .= (RecordAtom v)) (tupleAssocs $ unTuple rTuple)
  
instance DefaultOrdered RecordRelationTuple where  
  headerOrder (RecordRelationTuple tuple) = V.map (TE.encodeUtf8 . attributeName) (tupleAttributes tuple)
  
newtype RecordAtom = RecordAtom {unAtom :: Atom}
      
instance ToField RecordAtom where
  toField ratom = case unAtom ratom of
    BoolAtom atom -> toField (if atom then "t" else "f")
    StringAtom atom -> toField atom
    IntAtom atom -> toField atom
    DateTimeAtom atom -> toField (show atom)
    DateAtom atom -> toField (show atom)
    DoubleAtom atom -> toField atom
    RelationAtom _ -> error "Relation atoms cannot be used in CSV."
    ByteStringAtom bs -> toField bs
                 