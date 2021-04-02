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
import qualified Text.URI as URI
import Text.URI (URI)
import Crypto.Hash.SHA256
import Data.ByteString.Base16 as B16
import Network.HTTP.Simple
import Network.HTTP.Types
--import a file containing TutorialD database context expressions via local files or HTTP/HTTPS

importTutorialDFromFile :: URI -> HashVerification -> IO (Either RelationalError DatabaseContextExpr)
importTutorialDFromFile fileURI mHash = do
  case URI.uriPath fileURI of
    Just (False, _) -> do
      eTutdBytes <- try (BS.readFile filePath) :: IO (Either IOError BS.ByteString)
      case eTutdBytes of 
        Left err -> pure $ Left (ImportError (ImportFileError (T.pack (show err))))
        Right tutdBytes ->
          case validateBytes tutdBytes mHash of
            Left err -> pure (Left err)
            Right tutdBytes' -> importTutorialDBytes tutdBytes'
    _ -> pure (Left (ImportError (InvalidFileURIError (URI.render fileURI))))
  where
    filePath = drop (length ("file://" :: String)) (URI.renderStr fileURI)
        

importTutorialDBytes :: BS.ByteString -> IO (Either RelationalError DatabaseContextExpr)
importTutorialDBytes tutdBytes =
  case TE.decodeUtf8' tutdBytes of
    Left err -> pure (Left (ImportError (ImportFileDecodeError (T.pack (show err)))))
    Right tutdUtf8 -> 
      case parse (multipleDatabaseContextExprP <* eof) "import" tutdUtf8 of
        --parseErrorPretty is new in megaparsec 5
        Left err -> pure (Left (PM36E.ParseError (T.pack (errorBundlePretty err))))
        Right expr -> pure (Right expr)

importTutorialDViaHTTP :: URI -> HashVerification -> IO (Either RelationalError DatabaseContextExpr)
importTutorialDViaHTTP uri mHash = do
  reqURI <- parseRequest (URI.renderStr uri)
  response <- httpBS reqURI
  let scode = getResponseStatus response
  if scode == ok200 then
      let byteBody = getResponseBody response in
      case validateBytes byteBody mHash of
        Right byteBody' -> do
          importTutorialDBytes byteBody'
        Left err -> pure (Left err)
    else
      pure (Left (ImportError (ImportDownloadError (T.pack ("download failed: " <> show scode)))))

validateBytes :: BS.ByteString -> HashVerification -> Either RelationalError BS.ByteString
validateBytes tutdBytes mHash =
  case mHash of
    Nothing -> pure tutdBytes
    Just expectedHashText ->
      case B16.decode (TE.encodeUtf8 expectedHashText) of
        Left _ -> Left (ImportError (InvalidSHA256Error expectedHashText))
        Right expectedHash | hashBytes == expectedHash -> pure tutdBytes
                           | otherwise ->
                             let p = TE.decodeUtf8 . B16.encode in
                             Left (ImportError (SHA256MismatchError (p expectedHash) (p hashBytes)))
 where
    hashBytes = hash tutdBytes

    
tutdImportP :: Parser DatabaseContextDataImportOperator
tutdImportP = do
  reserved ":importtutd" 
  uri <- quoted URI.parser
  spaceConsumer
  mhash <- optional (quoted hex)
  let handler =
        case URI.unRText <$> URI.uriScheme uri of
          Nothing -> fail "URI scheme missing"
          Just "file" -> importTutorialDFromFile
          Just scheme | scheme `elem` ["http", "https"] -> importTutorialDViaHTTP
                      | otherwise -> fail ("unsupported URI scheme: " <> show scheme)
  pure $ DatabaseContextDataImportOperator uri mhash handler
