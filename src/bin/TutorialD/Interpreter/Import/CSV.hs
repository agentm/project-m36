module TutorialD.Interpreter.Import.CSV where
import TutorialD.Interpreter.Import.Base
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Relation.Parse.CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Text.Megaparsec.Text
import TutorialD.Interpreter.Base
import Control.Exception

importCSVRelation :: RelVarName -> Attributes -> FilePath -> IO (Either RelationalError DatabaseContextExpr)
importCSVRelation relVarName attrs pathIn = do
  --TODO: handle filesystem errors
  csvData <- try (BS.readFile pathIn) :: IO (Either IOError BS.ByteString)
  case csvData of 
    Left err -> return $ Left (ImportError $ T.pack (show err))
    Right csvData' -> case csvAsRelation csvData' attrs of
      Left err -> return $ Left (ParseError $ T.pack (show err))
      Right csvRel -> return $ Right (Insert relVarName (ExistingRelation csvRel))

importCSVP :: Parser RelVarDataImportOperator
importCSVP = do
  reserved ":importcsv"
  path <- quotedString
  spaceConsumer
  relVarName <- identifier
  return $ RelVarDataImportOperator relVarName (T.unpack path) importCSVRelation
  
