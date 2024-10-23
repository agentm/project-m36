{-# LANGUAGE GADTs #-}
module TutorialD.Interpreter.RODatabaseContextOperator where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Interpreter
import qualified ProjectM36.DataFrame as DF
import ProjectM36.Error
import ProjectM36.Tuple
import ProjectM36.InclusionDependency
import qualified ProjectM36.Client as C
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.DatabaseContextExpr
import TutorialD.Printer
import Control.Monad (when)
import qualified Data.Text as T
import ProjectM36.Relation.Show.Gnuplot
import ProjectM36.HashSecurely
import qualified Data.Map as M
import Data.Maybe
import Data.Functor

--operators which only rely on database context reading
data RODatabaseContextOperator where
  ShowRelation :: RelationalExpr -> RODatabaseContextOperator
  PlotRelation :: RelationalExpr -> RODatabaseContextOperator
  ShowRelationType :: RelationalExpr -> RODatabaseContextOperator
  ShowConstraints :: StringType -> RODatabaseContextOperator
  ShowPlan :: DatabaseContextExpr -> RODatabaseContextOperator
  ShowTypes :: RODatabaseContextOperator
  ShowRelationVariables :: RODatabaseContextOperator
  ShowAtomFunctions :: RODatabaseContextOperator
  ShowDatabaseContextFunctions :: RODatabaseContextOperator
  ShowNotifications :: RODatabaseContextOperator
  ShowDataFrame :: DF.DataFrameExpr -> RODatabaseContextOperator
  GetDDLHash :: RODatabaseContextOperator
  ShowDDL :: RODatabaseContextOperator
  ShowRegisteredQueries :: RODatabaseContextOperator
  Quit :: RODatabaseContextOperator
  deriving (Show)

typeP :: Parser RODatabaseContextOperator
typeP = do
  colonOp ":type"
  ShowRelationType <$> relExprP

showRelP :: Parser RODatabaseContextOperator
showRelP = do
  colonOp ":showexpr"
  ShowRelation <$> relExprP

showPlanP :: Parser RODatabaseContextOperator
showPlanP = do
  colonOp ":showplan"
  ShowPlan <$> databaseContextExprP

showTypesP :: Parser RODatabaseContextOperator
showTypesP = colonOp ":showtypes" >> pure ShowTypes

showRelationVariables :: Parser RODatabaseContextOperator
showRelationVariables = colonOp ":showrelvars" >> pure ShowRelationVariables

showAtomFunctionsP :: Parser RODatabaseContextOperator
showAtomFunctionsP = colonOp ":showatomfunctions" >> pure ShowAtomFunctions

showDatabaseContextFunctionsP :: Parser RODatabaseContextOperator
showDatabaseContextFunctionsP = colonOp ":showdatabasecontextfunctions" >> pure ShowDatabaseContextFunctions

showNotificationsP :: Parser RODatabaseContextOperator
showNotificationsP = colonOp ":shownotifications" >> pure ShowNotifications

quitP :: Parser RODatabaseContextOperator
quitP = do
  colonOp ":quit"
  return Quit

showConstraintsP :: Parser RODatabaseContextOperator
showConstraintsP = do
  colonOp ":constraints"
  ShowConstraints <$> option "" identifierP
  
plotRelExprP :: Parser RODatabaseContextOperator  
plotRelExprP = do
  colonOp ":plotexpr"
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
             <|> showNotificationsP
             <|> showDataFrameP
             <|> ddlHashP
             <|> showDDLP
             <|> showRegisteredQueriesP
             <|> quitP

--logically, these read-only operations could happen purely, but not if a remote call is required
evalRODatabaseContextOp :: C.SessionId -> C.Connection -> RODatabaseContextOperator -> IO ConsoleResult
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

evalRODatabaseContextOp sessionId conn (ShowConstraints name) = do
  eIncDeps <- C.inclusionDependencies sessionId conn
  let val = case eIncDeps of
        Left err -> Left err
        Right incDeps -> case name of
          "" -> inclusionDependenciesAsRelation incDeps >>= renderRelExprsInIncDeps
          depName -> case M.lookup depName incDeps of
            Nothing -> Left (InclusionDependencyNameNotInUseError depName)
            Just dep -> 
              inclusionDependenciesAsRelation (M.singleton depName dep) >>= renderRelExprsInIncDeps
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
    
evalRODatabaseContextOp sessionId conn ShowNotifications = do
  eRel <- C.notificationsAsRelation sessionId conn
  case eRel of
    Left err -> pure $ DisplayErrorResult (T.pack (show err))
    Right rel -> evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))
  
evalRODatabaseContextOp sessionId conn (ShowDataFrame dfExpr) = do
  eDataFrame <- C.executeDataFrameExpr sessionId conn dfExpr
  case eDataFrame of
    Left err -> pure (DisplayErrorResult (T.pack (show err)))
    Right dframe -> pure (DisplayDataFrameResult dframe)

evalRODatabaseContextOp sessionId conn GetDDLHash = do
  eHash <- C.getDDLHash sessionId conn
  case eHash of
    Left err -> pure (DisplayErrorResult (T.pack (show err)))
    Right h -> do
      let eRel = mkRelationFromList (C.attributesFromList [Attribute "ddlHash" ByteStringAtomType]) [[ByteStringAtom (_unSecureHash h)]]
      case eRel of
        Left err -> pure (DisplayErrorResult (T.pack (show err)))
        Right rel -> 
          evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rel))

evalRODatabaseContextOp sessionId conn ShowDDL = do
  eDDL <- C.ddlAsRelation sessionId conn
  case eDDL of
    Left err -> pure (DisplayErrorResult (T.pack (show err)))
    Right ddl ->
      evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation ddl))

evalRODatabaseContextOp sessionId conn ShowRegisteredQueries = do
  eRv <- C.registeredQueriesAsRelation sessionId conn
  case eRv of
    Left err -> pure (DisplayErrorResult (T.pack (show err)))
    Right rv ->
      evalRODatabaseContextOp sessionId conn (ShowRelation (ExistingRelation rv))      
 
evalRODatabaseContextOp _ _ Quit = pure QuitResult

interpretRODatabaseContextOp :: C.SessionId -> C.Connection -> T.Text -> IO ConsoleResult
interpretRODatabaseContextOp sessionId conn tutdstring = case parse roDatabaseContextOperatorP "" tutdstring of
  Left err -> pure $ DisplayErrorResult (T.pack (show err))
  Right parsed -> evalRODatabaseContextOp sessionId conn parsed
  
showDataFrameP :: Parser RODatabaseContextOperator
showDataFrameP = do
  colonOp ":showdataframe"
  ShowDataFrame <$> dataFrameP

dataFrameP :: Parser DF.DataFrameExpr
dataFrameP = do
  relExpr <- parens relExprP
  attrOrdersExpr <- try attrOrdersExprP <|> pure []
  mbLimit <- optional limitP
  mbOffset <- optional offsetP
  pure $ DF.DataFrameExpr relExpr attrOrdersExpr mbOffset mbLimit

offsetP :: Parser Integer
offsetP = do
  reservedOp "offset"
  natural

limitP :: Parser Integer
limitP = do
  reservedOp "limit"
  natural

attrOrdersExprP :: Parser [DF.AttributeOrderExpr]
attrOrdersExprP = reserved "orderby" *> braces (sepBy attrOrderExprP comma)

attrOrderExprP :: Parser DF.AttributeOrderExpr
attrOrderExprP = DF.AttributeOrderExpr <$> identifierP <*> orderP

orderP :: Parser DF.Order
orderP = try (reservedOp "ascending" >> pure DF.AscendingOrder) <|> try (reservedOp "descending" >> pure DF.DescendingOrder) <|> pure DF.AscendingOrder

-- render RelationalExprAtoms as TutorialD
renderRelExprsInIncDeps :: Relation -> Either RelationalError Relation
renderRelExprsInIncDeps = relMogrify tupMapper attrs
  where
    tupMapper tup = pure $ mkRelationTupleFromMap (M.map mapper (tupleToMap tup))
    mapper (RelationalExprAtom expr) = TextAtom (T.pack (show (prettyRelationalExpr expr)))
    mapper atom = atom
    attrs = C.attributesFromList [Attribute "name" TextAtomType,
                                  Attribute "sub" TextAtomType,
                                  Attribute "super" TextAtomType
                                 ]
    
ddlHashP :: Parser RODatabaseContextOperator
ddlHashP = colonOp ":ddlhash" $> GetDDLHash

showDDLP :: Parser RODatabaseContextOperator
showDDLP = colonOp ":showddl" $> ShowDDL

showRegisteredQueriesP :: Parser RODatabaseContextOperator
showRegisteredQueriesP = colonOp ":showregisteredqueries" $> ShowRegisteredQueries
