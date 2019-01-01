module TutorialD.Interpreter.Import.TutorialD where
import ProjectM36.Base
import TutorialD.Interpreter.Import.Base
import TutorialD.Interpreter.Base hiding (try)
import TutorialD.Interpreter.DatabaseContextExpr
import ProjectM36.Error
import qualified ProjectM36.Error as PM36E
import qualified Data.Text as T
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
--import a file containing TutorialD database context expressions

importTutorialD :: FilePath -> IO (Either RelationalError DatabaseContextExpr)
importTutorialD pathIn = do
  eTutdBytes <- try (BS.readFile pathIn) :: IO (Either IOError BS.ByteString)
  case eTutdBytes of 
    Left err -> return $ Left (ImportError $ T.pack (show err))
    Right tutdBytes ->
      case TE.decodeUtf8' tutdBytes of
        Left err -> pure (Left (ImportError $ T.pack (show err)))
        Right tutdUtf8 -> 
          case parse multipleDatabaseContextExprP "import" tutdUtf8 of
            --parseErrorPretty is new in megaparsec 5
            Left err -> pure (Left (PM36E.ParseError (T.pack (show err))))
            Right expr -> pure (Right expr)
  
tutdImportP :: Parser DatabaseContextDataImportOperator
tutdImportP = do
  reserved ":importtutd" 
  path <- quotedString
  spaceConsumer
  return $ DatabaseContextDataImportOperator (T.unpack path) importTutorialD