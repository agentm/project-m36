module TutorialD.Interpreter.Import.CSV where
import TutorialD.Interpreter.Import.Base
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Relation
import ProjectM36.Relation.Parse.CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec.String
import TutorialD.Interpreter.Base

importCSVRelation :: RelVarName -> FilePath -> DatabaseContext -> IO (Either RelationalError DatabaseExpr)
importCSVRelation relVarName pathIn context = do
  case M.lookup relVarName (relationVariables context) of
    Nothing -> return $ Left (RelVarNotDefinedError relVarName)
    Just targetRel -> do 
      --TODO: handle filesystem errors
      csvData <- BS.readFile pathIn
      case csvAsRelation csvData (attributes targetRel) of
        Left err -> return $ Left (ParseError $ T.pack (show err))
        Right csvRel -> return $ Right (Insert relVarName (ExistingRelation csvRel))

importCSVP :: Parser DataImportOperator
importCSVP = do
  reserved ":importcsv"
  path <- quotedString
  whiteSpace
  relVarName <- identifier
  return $ DataImportOperator path (importCSVRelation (T.pack relVarName))
  
