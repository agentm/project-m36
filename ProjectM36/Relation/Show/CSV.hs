module ProjectM36.Relation.Show.CSV where
import ProjectM36.Base
import Data.Csv
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BS

--maybe support a header in the future
relationAsCSV :: Relation -> BS.ByteString
relationAsCSV (Relation _ tupleSet) = tupleSetAsCSV tupleSet

tupleSetAsCSV :: RelationTupleSet -> BS.ByteString
tupleSetAsCSV tupleSet = encode $ HS.toList tupleSet

instance ToRecord RelationTuple where
  toRecord (RelationTuple tupMap) = toRecord $ map toField (M.elems tupMap)
      
instance ToField Atom where
  toField (StringAtom atom) = toField atom
  toField (IntAtom atom) = toField atom
  toField (RelationAtom atom) = undefined -- CSV does not support nested relations- this should likely be detected and rejected
    
                 