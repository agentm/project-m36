module TutorialD.Interpreter.Import.TutorialD where
import ProjectM36.Base
import TutorialD.Interpreter.Import.Base
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.DatabaseExpr
import Text.Parsec.String
import Text.Parsec
import ProjectM36.Error
import Data.Either
import qualified Data.Text as T
--import a file containing TutorialD database context expressions

importTutorialD :: FilePath -> DatabaseContext -> IO (Either RelationalError DatabaseExpr)
importTutorialD pathIn _ = do
  tutdData <- readFile pathIn
  let dbexprsErr = map (parse databaseExprP "") (lines tutdData)
      errs = lefts dbexprsErr
  case errs of
    err:_ -> return $ Left (ParseError (T.pack (show err)))
    [] -> return $ Right (MultipleExpr (rights dbexprsErr))

tutdImportP :: Parser DataImportOperator
tutdImportP = do
  reserved ":importtutd" 
  path <- quotedString
  whiteSpace
  return $ DataImportOperator path importTutorialD