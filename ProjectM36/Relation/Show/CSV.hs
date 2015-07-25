module ProjectM36.Relation.Show.CSV where
import ProjectM36.Base
import ProjectM36.Attribute
import Data.Csv
import ProjectM36.Tuple
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import ProjectM36.Error
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

--maybe support a header in the future
relationAsCSV :: Relation -> Either RelationalError BS.ByteString
relationAsCSV (Relation attrs tupleSet) = case relValAttrs of 
  [] -> Right $ encodeHeader `BS.append` tupleSetAsCSV tupleSet
  attrs' -> Left $ RelationValuedAttributesNotSupportedError (map attributeName attrs')
  where
    --figure out how to make cassava responsible for printing the header even for non-named records or make NamedRecord instance for RelationTuple ?
    -- this will break on attribute names which need quoting
    encodeHeader = (BS.intercalate (BS.fromStrict . TE.encodeUtf8 . T.pack $ ",") $ V.toList $ V.map (BS.fromStrict . TE.encodeUtf8 . attributeName) attrs) `BS.append` (BS.fromStrict . TE.encodeUtf8 . T.pack) "\n"
    relValAttrs = V.toList $ V.filter (isRelationAtomType . atomType) attrs

tupleSetAsCSV :: RelationTupleSet -> BS.ByteString
tupleSetAsCSV tupleSet = encode (HS.toList tupleSet)

instance ToRecord RelationTuple where
  toRecord tuple = toRecord $ map toField (V.toList $ tupleAtoms tuple)
      
instance ToField Atom where
  toField (BoolAtom atom) = toField (if atom then "t" else "f")
  toField (StringAtom atom) = toField atom
  toField (IntAtom atom) = toField atom
  toField (DateTimeAtom atom) = toField (show atom)
  toField (DateAtom atom) = toField (show atom)
  toField (DoubleAtom atom) = toField atom
  toField (RelationAtom _) = undefined -- CSV does not support nested relations
    
                 