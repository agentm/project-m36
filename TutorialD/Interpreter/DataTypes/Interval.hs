module TutorialD.Interpreter.DataTypes.Interval where
import Text.Parsec
import Text.Parsec.String
import ProjectM36.Base hiding (Finite)
import ProjectM36.DataTypes.Interval ()
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.DataTypes.DateTime
import Data.Interval
import Data.Time.Clock

intervalP :: (Ord a) => Parser (Extended a) -> Parser (Interval a)
intervalP unitP = do
  lBoundClosed <- string "(" *> return False <|> 
                  string "[" *> return True
  lBound <- unitP
  reservedOp ","
  uBound <- unitP
  
  uBoundClosed <- string ")" *> return False <|>
                  string "]" *> return True
                  
  return $ interval (lBound, lBoundClosed) (uBound, uBoundClosed)
  
intervalDateTimeUnitP :: Parser (Extended UTCTime)
intervalDateTimeUnitP = Finite <$> dateTimeStringP <|>
  (reserved "inf" *> return PosInf) <|> 
  (reserved "-inf" *> return NegInf)
  
intervalDateTimeAtomP :: Parser Atom
intervalDateTimeAtomP = do
  reserved "interval_datetime("
  intv <- intervalP intervalDateTimeUnitP
  reserved ")"
  return $ Atom intv
  
