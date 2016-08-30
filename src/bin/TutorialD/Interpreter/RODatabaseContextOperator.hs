{-# LANGUAGE GADTs #-}
module TutorialD.Interpreter.RODatabaseContextOperator where
import ProjectM36.Base
import qualified ProjectM36.Client as C
import Text.Megaparsec
import Text.Megaparsec.Text
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.DatabaseContextExpr
import Control.Monad.State
import qualified Data.Text as T
import ProjectM36.Relation.Show.Gnuplot
import qualified Data.Map as M
import Data.Maybe

--operators which only rely on database context reading
data RODatabaseContextOperator where
  ShowRelation :: RelationalExpr -> RODatabaseContextOperator
  PlotRelation :: RelationalExpr -> RODatabaseContextOperator
  ShowRelationType :: RelationalExpr -> RODatabaseContextOperator
  ShowConstraint :: StringType -> RODatabaseContextOperator
  ShowPlan :: DatabaseContextExpr -> RODatabaseContextOperator
  ShowTypes :: RODatabaseContextOperator
  ShowRelationVariables :: RODatabaseContextOperator
  Quit :: RODatabaseContextOperator
  deriving (Show)

typeP :: Parser RODatabaseContextOperator
typeP = do
  reservedOp ":type"
  expr <- relExprP
  return $ ShowRelationType expr

showRelP :: Parser RODatabaseContextOperator
showRelP = do
  reservedOp ":showexpr"
  expr <- relExprP
  return $ ShowRelation expr

showPlanP :: Parser RODatabaseContextOperator
showPlanP = do
  reservedOp ":showplan"
  expr <- databaseExprP
  return $ ShowPlan expr

showTypesP :: Parser RODatabaseContextOperator
showTypesP = reserved ":showtypes" >> pure ShowTypes

showRelationVariables :: Parser RODatabaseContextOperator
showRelationVariables = reserved ":showrelvars" >> pure ShowRelationVariables

quitP :: Parser RODatabaseContextOperator
quitP = do
  reservedOp ":quit"
  return Quit

showConstraintsP :: Parser RODatabaseContextOperator
showConstraintsP = do
  reservedOp ":constraints"
  constraintName <- option "" identifier
  return $ ShowConstraint constraintName
  
plotRelExprP :: Parser RODatabaseContextOperator  
plotRelExprP = do
  reserved ":plotexpr"
  expr <- relExprP
  return $ PlotRelation expr

roDatabaseContextOperatorP :: Parser RODatabaseContextOperator
roDatabaseContextOperatorP = typeP
             <|> showRelP
             <|> showRelationVariables
             <|> plotRelExprP
             <|> showConstraintsP
             <|> showPlanP
             <|> showTypesP
             <|> quitP

--logically, these read-only operations could happen purely, but not if a remote call is required
evalRODatabaseContextOp :: C.SessionId -> C.Connection -> RODatabaseContextOperator -> IO TutorialDOperatorResult
evalRODatabaseContextOp sessionId conn (ShowRelationType expr) = do
  res <- C.typeForRelationalExpr sessionId conn expr
  case res of
    Left err -> pure $ DisplayErrorResult $ T.pack (show err)
    Right rel -> pure $ DisplayRelationResult rel

evalRODatabaseContextOp sessionId conn (ShowRelation expr) = do
  res <- C.executeRelationalExpr sessionId conn expr
  case res of
    Left err -> pure $ DisplayErrorResult $ T.pack (show err)
    Right rel -> pure $ DisplayRelationResult rel
    
evalRODatabaseContextOp sessionId conn (PlotRelation expr) = do
  res <- C.executeRelationalExpr sessionId conn expr
  pure $ case res of
    Left err -> DisplayErrorResult $ T.pack (show err)
    Right rel -> DisplayIOResult $ do
      err <- plotRelation rel
      when (isJust err) $ putStrLn (show err)

evalRODatabaseContextOp sessionId conn (ShowConstraint name) = do
  eIncDeps <- C.inclusionDependencies sessionId conn
  pure $ case eIncDeps of
    Left err -> DisplayErrorResult $ T.pack (show err)
    Right incDeps -> case name of
      "" -> DisplayResult $ T.pack (show incDeps)
      depName -> case M.lookup depName incDeps of
        Nothing -> DisplayErrorResult "No such constraint."
        Just dep -> DisplayResult $ T.pack (show dep)

evalRODatabaseContextOp sessionId conn (ShowPlan dbExpr) = do
  plan <- C.planForDatabaseContextExpr sessionId conn dbExpr
  pure $ case plan of 
    Left err -> DisplayErrorResult (T.pack (show err))
    Right optDbExpr -> DisplayResult $ T.pack (show optDbExpr)

evalRODatabaseContextOp sessionId conn ShowTypes = do  
  eRel <- C.atomTypesAsRelation sessionId conn
  case eRel of
    Left err -> pure $ DisplayErrorResult (T.pack (show err))
    Right rel -> evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))
    
evalRODatabaseContextOp sessionId conn ShowRelationVariables = do
  eRel <- C.relationVariablesAsRelation sessionId conn
  case eRel of
    Left err -> pure $ DisplayErrorResult (T.pack (show err))
    Right rel -> evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))
  
evalRODatabaseContextOp _ _ (Quit) = pure QuitResult

interpretRODatabaseContextOp :: C.SessionId -> C.Connection -> T.Text -> IO TutorialDOperatorResult
interpretRODatabaseContextOp sessionId conn tutdstring = case parse roDatabaseContextOperatorP "" tutdstring of
  Left err -> pure $ DisplayErrorResult (T.pack (show err))
  Right parsed -> evalRODatabaseContextOp sessionId conn parsed
  
  
