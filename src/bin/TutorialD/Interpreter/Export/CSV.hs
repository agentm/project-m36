module TutorialD.Interpreter.Export.CSV where
import ProjectM36.Relation.Show.CSV
import TutorialD.Interpreter.Export.Base
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.Base
import ProjectM36.Base
import ProjectM36.Error
import Text.Megaparsec.Text
import qualified Data.ByteString.Lazy as BS
import Control.Exception (try)
import qualified Data.Text as T

exportCSVP :: Parser RelVarDataExportOperator
exportCSVP = do
  reserved ":exportcsv"
  exportExpr <- relExprP
  path <- quotedString
  return $ RelVarDataExportOperator exportExpr (T.unpack path) exportRelationCSV 
                               
exportRelationCSV :: RelVarDataExportOperator -> Relation -> IO (Maybe RelationalError)
exportRelationCSV (RelVarDataExportOperator _  pathOut _) rel =
  case relationAsCSV rel of
    Left err -> return $ Just err
    Right csvData -> do
      writeResult <- try (BS.writeFile pathOut csvData) :: IO (Either IOError ())
      case writeResult of
        Left err -> return $ Just (ExportError $ T.pack (show err))
        Right _ -> return Nothing