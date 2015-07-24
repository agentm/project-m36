module ProjectM36.Relation.Show.CSV where
import ProjectM36.Base
import Data.Csv
import ProjectM36.Tuple
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

--maybe support a header in the future
relationAsCSV :: Relation -> BS.ByteString
relationAsCSV (Relation _ tupleSet) = tupleSetAsCSV tupleSet

tupleSetAsCSV :: RelationTupleSet -> BS.ByteString
tupleSetAsCSV tupleSet = encode (HS.toList tupleSet)

instance ToRecord RelationTuple where
  toRecord tuple = toRecord $ map toField (V.toList $ tupleAtoms tuple)
      
instance ToField Atom where
  toField (BoolAtom atom) = toField (if atom then "t" else "f")
  toField (StringAtom atom) = toField atom
  toField (IntAtom atom) = toField atom
  toField (DateTimeAtom atom) = toField (show atom)
  toField (RelationAtom _) = undefined -- CSV does not support nested relations- this should likely be detected and rejected
    
                 