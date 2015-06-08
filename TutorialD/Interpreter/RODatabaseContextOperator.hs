{-# LANGUAGE GADTs,OverloadedStrings #-}
module TutorialD.Interpreter.RODatabaseContextOperator where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.RelationalExpression
import ProjectM36.StaticOptimizer
import Text.Parsec
import Text.Parsec.String
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.DatabaseExpr
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.HashSet as HS
import ProjectM36.Relation.Show.Term

--operators which only rely on database context reading
data RODatabaseContextOperator where
  ShowRelation :: RelationalExpr -> RODatabaseContextOperator
  ShowRelationType :: RelationalExpr -> RODatabaseContextOperator
  ShowConstraint :: StringType -> RODatabaseContextOperator
  ShowPlan :: DatabaseExpr -> RODatabaseContextOperator
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

quitP :: Parser RODatabaseContextOperator
quitP = do
  reservedOp ":quit"
  return Quit

showConstraintsP :: Parser RODatabaseContextOperator
showConstraintsP = do
  reservedOp ":constraints"
  constraintName <- option "" identifier
  return $ ShowConstraint (T.pack constraintName)

roDatabaseContextOperatorP :: Parser RODatabaseContextOperator
roDatabaseContextOperatorP = typeP
             <|> showRelP
             <|> showConstraintsP
             <|> showPlanP
             <|> quitP

evalRODatabaseContextOp :: DatabaseContext -> RODatabaseContextOperator -> TutorialDOperatorResult
evalRODatabaseContextOp context (ShowRelationType expr) = case runState (typeForRelationalExpr expr) context of
  (Right rel, _) -> DisplayResult $ showRelationAttributes (attributes rel)
  (Left err, _) -> DisplayErrorResult $ T.pack (show err)

evalRODatabaseContextOp context (ShowRelation expr) = do
  case runState (evalRelationalExpr expr) context of
    (Left err, _) -> DisplayErrorResult $ T.pack (show err)
    (Right rel, _) -> DisplayResult $ showRelation rel

evalRODatabaseContextOp context (ShowConstraint name) = do
  DisplayResult $ T.pack (show filteredDeps)
  where
    deps = inclusionDependencies context
    filteredDeps = case name of
      "" -> deps
      name2 -> HS.filter (\(InclusionDependency n _ _) -> n == name2) deps

evalRODatabaseContextOp context (ShowPlan dbExpr) = do
  DisplayResult $ T.pack (show plan)
  where
    plan = evalState (applyStaticDatabaseOptimization dbExpr) context

evalRODatabaseContextOp _ (Quit) = QuitResult

interpretRODatabaseContextOp :: DatabaseContext -> String -> TutorialDOperatorResult
interpretRODatabaseContextOp context tutdstring = case parse roDatabaseContextOperatorP "" tutdstring of
  Left err -> DisplayErrorResult (T.pack (show err))
  Right parsed -> evalRODatabaseContextOp context parsed