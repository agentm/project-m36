--compiling the script requires the IO monad because it must load modules from the filesystem, so we create the function and generate the requisite DatabaseExpr here.
module TutorialD.Interpreter.DatabaseContextIOOperator where
import ProjectM36.Base
import ProjectM36.Interpreter
import ProjectM36.Error
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.Types
import Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (IOException, handle, displayException)

data DatabaseContextIOOperator =
  DatabaseContextIOOp DatabaseContextIOExpr |
  LoadAtomFunctionFromFileOp FunctionName [TypeConstructor] FilePath |
  LoadDatabaseContextFunctionFromFileOp FunctionName [TypeConstructor] FilePath |
  LoadModuleFromFileOp FilePath
  deriving (Show, Eq)

addAtomFunctionExprP :: Parser DatabaseContextIOExpr
addAtomFunctionExprP = dbioexprP "addatomfunction" AddAtomFunction
  
addDatabaseContextFunctionExprP :: Parser DatabaseContextIOExpr
addDatabaseContextFunctionExprP = dbioexprP "adddatabasecontextfunction" AddDatabaseContextFunction

createArbitraryRelationP :: Parser DatabaseContextIOExpr
createArbitraryRelationP = do
  reserved "createarbitraryrelation"
  relVarName <- identifierP
  attrExprs <- makeAttributeExprsP :: Parser [AttributeExpr]
  min' <- fromInteger <$> integer
  _ <- symbol "-"
  max' <- fromInteger <$> integer
  pure $ CreateArbitraryRelation relVarName attrExprs (min',max')
  
dbioexprP :: ParseStr -> (Text -> [TypeConstructor] -> Text -> DatabaseContextIOExpr) -> Parser DatabaseContextIOExpr
dbioexprP res adt = do
  reserved res
  funcName' <- quotedString
  funcType' <- atomTypeSignatureP
  adt funcName' funcType' <$> quotedString

atomTypeSignatureP :: Parser [TypeConstructor]
atomTypeSignatureP = sepBy typeConstructorP arrow

databaseContextIOExprP :: Parser DatabaseContextIOExpr
databaseContextIOExprP = addAtomFunctionExprP <|> 
                   addDatabaseContextFunctionExprP <|> 
                   loadAtomFunctionsP <|>
                   loadDatabaseContextFunctionsP <|>
                   createArbitraryRelationP

databaseContextIOOperatorP :: Parser DatabaseContextIOOperator
databaseContextIOOperatorP = 
  (DatabaseContextIOOp <$> databaseContextIOExprP) <|>
  (reserved "loadatomfunctionfromfile" >> (LoadAtomFunctionFromFileOp <$> quotedString <*> atomTypeSignatureP <*> quotedFilePath)) <|>
  (reserved "loaddatabasecontextfunctionfromfile" >> (LoadDatabaseContextFunctionFromFileOp <$> quotedString <*> atomTypeSignatureP <*> quotedFilePath)) <|>
  loadModuleFromFileP
  
loadAtomFunctionsP :: Parser DatabaseContextIOExpr
loadAtomFunctionsP = do
  reserved "loadatomfunctions"
  LoadAtomFunctions <$> quotedString <*> quotedString <*> fmap unpack quotedString

loadDatabaseContextFunctionsP :: Parser DatabaseContextIOExpr  
loadDatabaseContextFunctionsP = do
  reserved "loaddatabasecontextfunctions"
  LoadDatabaseContextFunctions <$> quotedString <*> quotedString <*> fmap unpack quotedString

loadModuleFromFileP :: Parser DatabaseContextIOOperator
loadModuleFromFileP = do
  reserved "loadmodulefromfile"
  LoadModuleFromFileOp <$> quotedFilePath
                                             
interpretDatabaseContextIOOperator :: DatabaseContextIOOperator -> IO (Either RelationalError DatabaseContextIOExpr)
interpretDatabaseContextIOOperator expr = do
  let loadFromFile typ functionName functionArgs functionFilePath = do
        eFuncBody <- handle ioExcHandler (Right <$> TIO.readFile functionFilePath)
        case eFuncBody of
          Left err -> pure (Left err)
          Right functionBody ->
            pure (Right (typ functionName functionArgs functionBody))
      ioExcHandler :: IOException -> IO (Either RelationalError Text)
      ioExcHandler e = pure (Left (ImportError (ImportFileError (T.pack (displayException e)))))
        
  case expr of
    DatabaseContextIOOp expr' ->
      pure (Right expr')
    LoadAtomFunctionFromFileOp functionName functionArgs functionFilePath ->
      loadFromFile AddAtomFunction functionName functionArgs functionFilePath
    LoadDatabaseContextFunctionFromFileOp functionName functionArgs functionFilePath ->
      loadFromFile AddDatabaseContextFunction functionName functionArgs functionFilePath
    -- pass module contents via Text so that remote client can upload a module, duh
    LoadModuleFromFileOp modPath -> do
      eModBody <- handle ioExcHandler (Right <$> TIO.readFile modPath)
      case eModBody of
        Left err -> pure (Left err)
        Right modBody ->
          pure (Right (LoadModuleWithFunctions modBody))


