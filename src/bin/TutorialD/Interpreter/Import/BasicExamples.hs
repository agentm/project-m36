{-# LANGUAGE CPP #-}
--includes some hardcoded examples which can be imported even during safe evaluation (no file I/O)
module TutorialD.Interpreter.Import.BasicExamples where
import ProjectM36.DateExamples
import ProjectM36.Base
import ProjectM36.DatabaseContext
import TutorialD.Interpreter.Base

#if !MIN_VERSION_megaparsec(6,0,0)
import Text.Megaparsec.Text
#endif

data ImportBasicExampleOperator = ImportBasicDateExampleOperator
                                deriving (Show)

evalImportBasicExampleOperator :: ImportBasicExampleOperator -> DatabaseContextExpr
evalImportBasicExampleOperator ImportBasicDateExampleOperator = databaseContextAsDatabaseContextExpr dateExamples

importBasicExampleOperatorP :: Parser ImportBasicExampleOperator
importBasicExampleOperatorP = do 
  reservedOp ":importexample"
  example <- identifier
  if example == "date" then
    pure ImportBasicDateExampleOperator
    else
    fail "Unknown example name"
    