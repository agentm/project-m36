module TutorialD.Interpreter.Import.Base where
import ProjectM36.Base
import ProjectM36.Error

-- | import data into a relation variable
data RelVarDataImportOperator = RelVarDataImportOperator RelVarName FilePath (RelVarName -> Attributes -> FilePath -> IO (Either RelationalError DatabaseExpr))

-- | import data into a database context
data DatabaseContextDataImportOperator = DatabaseContextDataImportOperator FilePath (FilePath -> IO (Either RelationalError DatabaseExpr))

-- perhaps create a structure to import a whole transaction graph section in the future

evalRelVarDataImportOperator :: RelVarDataImportOperator -> Attributes -> IO (Either RelationalError DatabaseExpr)
evalRelVarDataImportOperator (RelVarDataImportOperator relVarName path importFunc) attrs = importFunc relVarName attrs path 
        
evalDatabaseContextDataImportOperator :: DatabaseContextDataImportOperator -> IO (Either RelationalError DatabaseExpr)        
evalDatabaseContextDataImportOperator (DatabaseContextDataImportOperator path importFunc) = importFunc path
