{-# LANGUAGE CPP #-}
module TutorialD.Interpreter.Import.CSV where
import TutorialD.Interpreter.Import.Base
import ProjectM36.Base
import ProjectM36.Interpreter
import ProjectM36.Error
import ProjectM36.Relation.Parse.CSV hiding (quotedString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import TutorialD.Interpreter.Base hiding (try)
import Control.Exception

importCSVRelation :: RelVarName -> TypeConstructorMapping -> Attributes -> FilePath -> IO (Either RelationalError DatabaseContextExpr)
importCSVRelation relVarName tConsMap attrs pathIn = do
  --TODO: handle filesystem errors
  csvData <- try (BS.readFile pathIn) :: IO (Either IOError BS.ByteString)
  case csvData of 
    Left err -> pure $ Left (ImportError (ImportFileError (T.pack (show err))))
    Right csvData' -> case csvAsRelation attrs tConsMap csvData' of
      Left err -> pure $ Left (ParseError $ T.pack (show err))
      Right csvRel -> pure $ Right (Insert relVarName (ExistingRelation csvRel))

importCSVP :: Parser RelVarDataImportOperator
importCSVP = do
  reserved ":importcsv"
  path <- quotedString
  spaceConsumer
  relVarName <- identifier
  return $ RelVarDataImportOperator relVarName (T.unpack path) importCSVRelation
  
