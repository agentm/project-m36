--includes some hardcoded examples which can be imported even during safe evaluation (no file I/O)
module TutorialD.Interpreter.Import.BasicExamples where
import ProjectM36.DateExamples
import ProjectM36.Base
import ProjectM36.Interpreter
import ProjectM36.DatabaseContextExpr
import TutorialD.Interpreter.Base

data ImportBasicExampleOperator = ImportBasicDateExampleOperator
                                deriving (Show)

evalImportBasicExampleOperator :: ImportBasicExampleOperator -> DatabaseContextExpr
evalImportBasicExampleOperator ImportBasicDateExampleOperator = resolvedDatabaseContextAsDatabaseContextExpr dateExamples

importBasicExampleOperatorP :: Parser ImportBasicExampleOperator
importBasicExampleOperatorP = do 
  reservedOp ":importexample"
  example <- identifierP
  if example == "cjdate" then
    pure ImportBasicDateExampleOperator
    else
    fail "Unknown example name"
    
