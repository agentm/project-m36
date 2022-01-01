{-# LANGUAGE CPP #-}
module TutorialD.Interpreter.Import.Base where
import ProjectM36.Base
import ProjectM36.Error
import Text.URI (URI)
import Data.Text (Text)
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif

-- | import data into a relation variable
data RelVarDataImportOperator = RelVarDataImportOperator RelVarName FilePath (RelVarName -> TypeConstructorMapping -> Attributes -> FilePath -> IO (Either RelationalError DatabaseContextExpr))

instance Show RelVarDataImportOperator where
  show (RelVarDataImportOperator rv path _) = "RelVarDataImportOperator " <> show rv <> " " <> path

type HashVerification = Maybe Text

-- | import data into a database context
data DatabaseContextDataImportOperator = DatabaseContextDataImportOperator URI HashVerification (URI -> HashVerification -> IO (Either RelationalError DatabaseContextExpr))

instance Show DatabaseContextDataImportOperator where
  show (DatabaseContextDataImportOperator uri hash _) =
    "DatabaseContextDataImportOperator " <> show uri <> " " <> show hash

-- perhaps create a structure to import a whole transaction graph section in the future

evalRelVarDataImportOperator :: RelVarDataImportOperator -> TypeConstructorMapping -> Attributes -> IO (Either RelationalError DatabaseContextExpr)
evalRelVarDataImportOperator (RelVarDataImportOperator relVarName path importFunc) tConsMap attrs = importFunc relVarName tConsMap attrs path 
        
evalDatabaseContextDataImportOperator :: DatabaseContextDataImportOperator -> IO (Either RelationalError DatabaseContextExpr)        
evalDatabaseContextDataImportOperator (DatabaseContextDataImportOperator uri hash importFunc) = importFunc uri hash
