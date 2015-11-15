module TutorialD.Interpreter.Export.Base where
import ProjectM36.Base
import ProjectM36.Error

data RelVarDataExportOperator = RelVarDataExportOperator (RelationalExpr -> FilePath -> IO (Maybe RelationalError)) FilePath                                              

evalDataExportOperator :: DataExportOperator -> DatabaseContext -> IO (Maybe RelationalError)
evalDataExportOperator (DataExportOperator exportFunc pathOut) context = exportFunc context pathOut

