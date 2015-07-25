module TutorialD.Interpreter.Export.CSV where
import ProjectM36.Relation.Show.CSV
import TutorialD.Interpreter.Export.Base
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.Base
import ProjectM36.Base
import ProjectM36.RelationalExpression
import ProjectM36.Error
import Control.Monad.State (evalState)
import Text.Parsec.String
import qualified Data.ByteString.Lazy as BS
import Control.Exception (try)
import qualified Data.Text as T

exportCSVP :: Parser DataExportOperator
exportCSVP = do
  reserved ":exportcsv"
  exportExpr <- relExprP
  path <- quotedString
  return $ DataExportOperator (exportRelationCSV exportExpr) path
                               
exportRelationCSV :: RelationalExpr -> DatabaseContext -> FilePath -> IO (Maybe RelationalError)
exportRelationCSV relExpr context pathOut = do                               
  let exportRel = evalState (evalRelationalExpr relExpr) context
  case exportRel of
    Left err -> return $ Just err
    Right exportRel' -> do
      case relationAsCSV exportRel' of
        Left err -> return $ Just err
        Right csvData -> do
          writeResult <- try (BS.writeFile pathOut csvData) :: IO (Either IOError ())
          case writeResult of
            Left err -> return $ Just (ExportError $ T.pack (show err))
            Right _ -> return Nothing