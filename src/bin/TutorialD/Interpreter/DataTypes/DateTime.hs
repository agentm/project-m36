module TutorialD.Interpreter.DataTypes.DateTime where
import Text.Megaparsec
import ProjectM36.Base
import TutorialD.Interpreter.Base
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T

dateTimeAtomP :: Parser Atom
dateTimeAtomP = do
  dateTime' <- try $ do
    dateTime <- dateTimeStringP
    reserved "::datetime"
    return dateTime 
  return $ DateTimeAtom dateTime'

dateTimeStringP :: Parser UTCTime
dateTimeStringP = do
  dateTimeString <- quotedString
  case parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack dateTimeString) of
    Just utctime -> pure (utctime :: UTCTime)
    Nothing -> fail $ "Failed to parse datetime from " ++ T.unpack dateTimeString