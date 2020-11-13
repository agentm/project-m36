{-# LANGUAGE CPP #-}
module TutorialD.Interpreter.Export.Base where
import ProjectM36.Base
import ProjectM36.Error
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif

data RelVarDataExportOperator = RelVarDataExportOperator RelationalExpr FilePath (RelVarDataExportOperator -> Relation -> IO (Maybe RelationalError))

instance Show RelVarDataExportOperator where
  show (RelVarDataExportOperator expr path _) = "RelVarDataExportOperator " <> show expr <> " " <>  path

evalRelVarDataExportOperator :: RelVarDataExportOperator -> Relation -> IO (Maybe RelationalError)
evalRelVarDataExportOperator op@(RelVarDataExportOperator _ _ exportFunc) = exportFunc op

