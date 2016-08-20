module TutorialD.Interpreter.Export.Base where
import ProjectM36.Base
import ProjectM36.Error

data RelVarDataExportOperator = RelVarDataExportOperator RelationalExpr FilePath (RelVarDataExportOperator -> Relation -> IO (Maybe RelationalError))

evalRelVarDataExportOperator :: RelVarDataExportOperator -> Relation -> IO (Maybe RelationalError)
evalRelVarDataExportOperator op@(RelVarDataExportOperator _ _ exportFunc) rel = exportFunc op rel

