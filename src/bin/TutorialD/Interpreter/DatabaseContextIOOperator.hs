--compiling the script requires the IO monad because it must load modules from the filesystem, so we create the function and generate the requisite DatabaseExpr here.
module TutorialD.Interpreter.DatabaseContextIOOperator where
import ProjectM36.Base

import TutorialD.Interpreter.Base
import TutorialD.Interpreter.Types
import Text.Parsec
import Text.Parsec.String
import Data.Text
import Control.Monad

addAtomFunctionExprP :: Parser DatabaseContextIOExpr
addAtomFunctionExprP = do
  reserved "addatomfunction"
  funcName <- liftM pack quotedString
  funcType <- atomTypeSignatureP
  funcScript <- liftM pack quotedString
  pure $ AddAtomFunction funcName funcType funcScript
  
atomTypeSignatureP :: Parser [TypeConstructor]
atomTypeSignatureP = sepBy typeConstructorP arrow

{-
evalDatabaseContextIOOp :: HscEnv -> DatabaseContext -> DatabaseContextIOExpr -> IO (Either RelationalError DatabaseContextIOExpr)
evalDatabaseContextIOOp env context addfunc@(AddAtomFunction _ _ _) = do
  evalDatabaseContextIOExpr context 
  result <- runGhc (Just libdir) $ do
    setSession env
    ret <- typeCheckAtomFunctionScript script
    case ret of
      Just err -> pure (Left err)
      Nothing -> do
        eBody <- compileAtomFunctionScript script
        case eBody of 
          Left err -> pure (Left err)
          Right func -> let atomFunc = AtomFunction {
                              atomFuncName = funcName, 
                              atomFuncType = funcType,
                              atomFuncBody = AtomFunctionBody (Just script) func } in
                        --actually add the func
                        pure $ Right context
  case result of 
    Left err -> Left (AtomFunctionBodyScriptError err)
    Right context -> pure (Right context)
-}