module TutorialD.Interpreter.Export.Base where
import ProjectM36.Base
import ProjectM36.Error
import Data.Monoid

data RelVarDataExportOperator = RelVarDataExportOperator RelationalExpr FilePath (RelVarDataExportOperator -> Relation -> IO (Maybe RelationalError))

instance Show RelVarDataExportOperator where
  show (RelVarDataExportOperator expr path _) = "RelVarDataExportOperator " <> show expr <> " " <>  path

evalRelVarDataExportOperator :: RelVarDataExportOperator -> Relation -> IO (Maybe RelationalError)
evalRelVarDataExportOperator op@(RelVarDataExportOperator _ _ exportFunc) rel = exportFunc op rel

