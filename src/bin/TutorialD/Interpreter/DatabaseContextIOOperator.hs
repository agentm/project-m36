--compiling the script requires the IO monad because it must load modules from the filesystem, so we create the function and generate the requisite DatabaseExpr here.
module TutorialD.Interpreter.DatabaseContextIOOperator where
import ProjectM36.Base
import ProjectM36.Interpreter
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.Types
import Data.Text

addAtomFunctionExprP :: Parser DatabaseContextIOExpr
addAtomFunctionExprP = dbioexprP "addatomfunction" AddAtomFunction
  
addDatabaseContextFunctionExprP :: Parser DatabaseContextIOExpr
addDatabaseContextFunctionExprP = dbioexprP "adddatabasecontextfunction" AddDatabaseContextFunction

createArbitraryRelationP :: Parser DatabaseContextIOExpr
createArbitraryRelationP = do
  reserved "createarbitraryrelation"
  relVarName <- identifier
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

dbContextIOExprP :: Parser DatabaseContextIOExpr
dbContextIOExprP = addAtomFunctionExprP <|> 
                   addDatabaseContextFunctionExprP <|> 
                   loadAtomFunctionsP <|>
                   loadDatabaseContextFunctionsP <|>
                   createArbitraryRelationP

loadAtomFunctionsP :: Parser DatabaseContextIOExpr
loadAtomFunctionsP = do
  reserved "loadatomfunctions"
  LoadAtomFunctions <$> quotedString <*> quotedString <*> fmap unpack quotedString

loadDatabaseContextFunctionsP :: Parser DatabaseContextIOExpr  
loadDatabaseContextFunctionsP = do
  reserved "loaddatabasecontextfunctions"
  LoadDatabaseContextFunctions <$> quotedString <*> quotedString <*> fmap unpack quotedString
                                             
