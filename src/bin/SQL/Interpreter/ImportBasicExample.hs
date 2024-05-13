module SQL.Interpreter.ImportBasicExample where
import qualified Data.Text as T
import SQL.Interpreter.Base
import ProjectM36.Interpreter

newtype ImportBasicExampleOperator = ImportBasicExampleOperator T.Text
  deriving (Show)

importBasicExampleP :: Parser ImportBasicExampleOperator
importBasicExampleP = do
  reserveds "IMPORT EXAMPLE CJDATE"
  pure (ImportBasicExampleOperator "cjdate")
