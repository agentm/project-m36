
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.Error
import ProjectM36.Relation.Show.CSV
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy.Char8 as BS
                     
main = do
  case matrixRelation 10 100000 of
    Right rel -> BS.putStrLn $ relationAsCSV rel
    Left err -> putStrLn (show err)