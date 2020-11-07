{-# LANGUAGE GADTs #-}
module TutorialD.Interpreter.RODatabaseContextOperator where
import ProjectM36.Base
import qualified ProjectM36.DataFrame as DF
import ProjectM36.Error
import ProjectM36.InclusionDependency
import qualified ProjectM36.Client as C
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
  ShowAtomFunctions :: RODatabaseContextOperator
  ShowDatabaseContextFunctions :: RODatabaseContextOperator
  ShowDataFrame :: DF.DataFrameExpr -> RODatabaseContextOperator
  Quit :: RODatabaseContextOperator
  deriving (Show)

typeP :: Parser RODatabaseContextOperator
typeP = do
  colonOp ":type" ":t"
  ShowRelationType <$> relExprP

showRelP :: Parser RODatabaseContextOperator
showRelP = do
  colonOp ":showexpr" ":se"
  ShowRelation <$> relExprP

showPlanP :: Parser RODatabaseContextOperator
showPlanP = do
  colonOp ":showplan" ":sp"
  ShowPlan <$> databaseContextExprP

showTypesP :: Parser RODatabaseContextOperator
showTypesP = colonOp ":showtypes" ":st" >> pure ShowTypes

showRelationVariables :: Parser RODatabaseContextOperator
showRelationVariables = colonOp ":showrelvars" ":srv" >> pure ShowRelationVariables

showAtomFunctionsP :: Parser RODatabaseContextOperator
showAtomFunctionsP = colonOp ":showatomfunctions" ":saf" >> pure ShowAtomFunctions

showDatabaseContextFunctionsP :: Parser RODatabaseContextOperator
showDatabaseContextFunctionsP = colonOp ":showdatabasecontextfunctions" ":sdbf" >> pure ShowDatabaseContextFunctions

quitP :: Parser RODatabaseContextOperator
quitP = do
  colonOp ":quit" ":q"
  return Quit

showConstraintsP :: Parser RODatabaseContextOperator
showConstraintsP = do
  colonOp ":constraints" ":c"
  ShowConstraint <$> option "" identifier
  
plotRelExprP :: Parser RODatabaseContextOperator  
plotRelExprP = do
  colonOp ":plotexpr" ":pe"
  PlotRelation <$> relExprP

roDatabaseContextOperatorP :: Parser RODatabaseContextOperator
roDatabaseContextOperatorP = typeP
             <|> showRelP
             <|> showRelationVariables
             <|> plotRelExprP
             <|> showConstraintsP
             <|> showPlanP
             <|> showTypesP
             <|> showAtomFunctionsP
             <|> showDatabaseContextFunctionsP
             <|> showDataFrameP
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
      when (isJust err) $ print err

evalRODatabaseContextOp sessionId conn (ShowConstraint name) = do
  eIncDeps <- C.inclusionDependencies sessionId conn
  let val = case eIncDeps of
        Left err -> Left err
        Right incDeps -> case name of
          "" -> inclusionDependenciesAsRelation incDeps
          depName -> case M.lookup depName incDeps of
            Nothing -> Left (InclusionDependencyNameNotInUseError depName)
            Just dep -> inclusionDependenciesAsRelation (M.singleton depName dep)
  pure $ case val of
     Left err -> DisplayErrorResult (T.pack (show err))
     Right rel -> DisplayRelationResult rel

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
    
evalRODatabaseContextOp sessionId conn ShowAtomFunctions = do
  eRel <- C.atomFunctionsAsRelation sessionId conn
  case eRel of
    Left err -> pure $ DisplayErrorResult (T.pack (show err))
    Right rel -> evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))
    
evalRODatabaseContextOp sessionId conn ShowDatabaseContextFunctions = do
  eRel <- C.databaseContextFunctionsAsRelation sessionId conn
  case eRel of
    Left err -> pure $ DisplayErrorResult (T.pack (show err))
    Right rel -> evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))
  
evalRODatabaseContextOp sessionId conn (ShowDataFrame dfExpr) = do
  eDataFrame <- C.executeDataFrameExpr sessionId conn dfExpr
  case eDataFrame of
    Left err -> pure (DisplayErrorResult (T.pack (show err)))
    Right dframe -> pure (DisplayDataFrameResult dframe)
 
evalRODatabaseContextOp _ _ Quit = pure QuitResult

interpretRODatabaseContextOp :: C.SessionId -> C.Connection -> T.Text -> IO TutorialDOperatorResult
interpretRODatabaseContextOp sessionId conn tutdstring = case parse roDatabaseContextOperatorP "" tutdstring of
  Left err -> pure $ DisplayErrorResult (T.pack (show err))
  Right parsed -> evalRODatabaseContextOp sessionId conn parsed
  
showDataFrameP :: Parser RODatabaseContextOperator
showDataFrameP = do
  colonOp ":showdataframe" ":sdf"
  relExpr <- relExprP
  reservedOp "orderby"
  attrOrdersExpr <- attrOrdersExprP
  mbOffset <- optional offsetP
  mbLimit <- optional limitP
  pure $ ShowDataFrame (DF.DataFrameExpr relExpr attrOrdersExpr mbOffset mbLimit)


offsetP :: Parser Integer
offsetP = do
  reservedOp "offset"
  natural

limitP :: Parser Integer
limitP = do
  reservedOp "limit"
  natural

attrOrdersExprP :: Parser [DF.AttributeOrderExpr]
attrOrdersExprP = braces (sepBy attrOrderExprP comma)

attrOrderExprP :: Parser DF.AttributeOrderExpr
attrOrderExprP = DF.AttributeOrderExpr <$> identifier <*> orderP

orderP :: Parser DF.Order
orderP = try (reservedOp "ascending" >> pure DF.AscendingOrder) <|> try (reservedOp "descending" >> pure DF.DescendingOrder) <|> pure DF.AscendingOrder
