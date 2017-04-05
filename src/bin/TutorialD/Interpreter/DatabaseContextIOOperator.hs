--compiling the script requires the IO monad because it must load modules from the filesystem, so we create the function and generate the requisite DatabaseExpr here.
module TutorialD.Interpreter.DatabaseContextIOOperator where
import ProjectM36.Base

import TutorialD.Interpreter.Base
import TutorialD.Interpreter.Types
import Text.Megaparsec
import Text.Megaparsec.Text

addAtomFunctionExprP :: Parser DatabaseContextIOExpr
addAtomFunctionExprP = do
  reserved "addatomfunction"
  funcName <- quotedString
  funcType <- atomTypeSignatureP
  funcScript <- quotedString
  pure $ AddAtomFunction funcName funcType funcScript

atomTypeSignatureP :: Parser [TypeConstructor]
atomTypeSignatureP = sepBy typeConstructorP arrow

addDatabaseContextFunctionExprP :: Parser DatabaseContextIOExpr
addDatabaseContextFunctionExprP = do
  reserved "adddatabasecontextfunction"
  funcName <- quotedString
  funcType <- atomTypeSignatureP
  funcScript <- quotedString
  pure $ AddDatabaseContextFunction funcName funcType funcScript
  
dbContextIOExprP :: Parser DatabaseContextIOExpr
dbContextIOExprP = addAtomFunctionExprP <|> addDatabaseContextFunctionExprP
  
                                             
