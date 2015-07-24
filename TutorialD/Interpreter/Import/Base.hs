module TutorialD.Interpreter.Import.Base where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Relation
import qualified Data.Map as M

data DataImportOperator = DataImportOperator FilePath (FilePath -> DatabaseContext -> IO (Either RelationalError DatabaseExpr))

evalDataImportOperator :: DataImportOperator -> DatabaseContext -> IO (Either RelationalError DatabaseExpr)
evalDataImportOperator (DataImportOperator path importFunc) context = do
  imported <- importFunc path context
  case imported of
        Left err -> return $ Left err
        Right dbexprs -> return $ Right dbexprs