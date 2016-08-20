module TutorialD.Interpreter.DataTypes.DateTime where
import Text.Parsec
import ProjectM36.Base
import Text.Parsec.String
import TutorialD.Interpreter.Base
import Data.Time.Clock
import Data.Time.Format

dateTimeAtomP :: Parser Atom
dateTimeAtomP = do
  dateTime' <- try $ do
    dateTime <- dateTimeStringP
    reserved "::datetime"
    return dateTime 
  return $ Atom dateTime'

dateTimeStringP :: Parser UTCTime
dateTimeStringP = do
  dateTimeString <- quotedString
  case parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" dateTimeString of
    Just utctime -> return $ (utctime :: UTCTime)
    Nothing -> fail $ "Failed to parse datetime from " ++ dateTimeString