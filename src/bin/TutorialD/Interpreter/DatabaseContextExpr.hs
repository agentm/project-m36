{-# LANGUAGE CPP #-}
module TutorialD.Interpreter.DatabaseContextExpr where
import ProjectM36.Base
import ProjectM36.Interpreter
import ProjectM36.DatabaseContext.Types
import ProjectM36.DatabaseContext
import ProjectM36.TransactionGraph.Types
import TutorialD.Interpreter.Base
import qualified Data.Text as T
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.Types
import qualified Data.Map as M
import ProjectM36.StaticOptimizer
import qualified ProjectM36.Error as PM36E
import ProjectM36.Error
import qualified ProjectM36.RelationalExpression as RE
import ProjectM36.Key
import ProjectM36.FunctionalDependency
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.Functor

--parsers which create "database expressions" which modify the database context (such as relvar assignment)
databaseContextExprP :: Parser DatabaseContextExpr
databaseContextExprP = choice [insertP,
                               deleteConstraintP,
                               deleteP,
                               updateP,
                               addConstraintP,
                               keyP,
                               funcDepP,
                               defineP,
                               undefineP,
                               assignP,
                               addNotificationP,
                               removeNotificationP,
                               addTypeConstructorP,
                               removeTypeConstructorP,
                               removeAtomFunctionP,
                               executeDatabaseContextFunctionP,
                               removeDatabaseContextFunctionP,
                               addRegisteredQueryP,
                               removeRegisteredQueryP,
                               nothingP]
            
nothingP :: Parser DatabaseContextExpr            
nothingP = spaceConsumer <* eof >> pure NoOperation

assignP :: Parser DatabaseContextExpr
assignP = do
  relVarName <- try $ do
    relVarName <- relVarNameP
    reservedOp ":="
    return relVarName
  Assign relVarName <$> relExprP
  
multilineSep :: Parser T.Text  
multilineSep = newline >> pure "\n"

multipleDatabaseContextExprP :: Parser DatabaseContextExpr
multipleDatabaseContextExprP = do
  exprs <- filter (/= NoOperation) <$> sepBy1 databaseContextExprP semi
  pure (someDatabaseContextExprs exprs)

insertP :: Parser DatabaseContextExpr
insertP = do
  reservedOp "insert"
  relvar <- relVarNameP
  Insert relvar <$> relExprP

defineP :: Parser DatabaseContextExpr
defineP = do
  relVarName <- try $ do
    relVarName <- relVarNameP
    reservedOp "::"
    return relVarName
  Define relVarName <$> makeAttributeExprsP

undefineP :: Parser DatabaseContextExpr
undefineP = do
  reservedOp "undefine"
  Undefine <$> relVarNameP

deleteP :: Parser DatabaseContextExpr
deleteP = do
  reservedOp "delete"
  Delete <$> relVarNameP <*> option TruePredicate (reservedOp "where" *> restrictionPredicateP)

updateP :: Parser DatabaseContextExpr
updateP = do
  reservedOp "update"
  relVarName <- relVarNameP
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP <* spaceConsumer)
  attributeAssignments <- M.fromList <$> parens (sepBy attributeAssignmentP comma)
  return $ Update relVarName attributeAssignments predicate

data IncDepOp = SubsetOp | EqualityOp

addConstraintP :: Parser DatabaseContextExpr
addConstraintP = do
  reservedOp "constraint" <|> reservedOp "foreign key"
  constraintName <- identifierP
  subset <- relExprP
  op <- (reservedOp "in" $> SubsetOp) <|> (reservedOp "equals" $> EqualityOp)
  superset <- relExprP
  let subsetA = incDepSet constraintName subset superset
      subsetB = incDepSet (constraintName <> "_eqInvert") superset subset --inverted args for equality constraint
      incDepSet nam a b = AddInclusionDependency nam (InclusionDependency a b)
  case op of
    SubsetOp -> pure subsetA
    EqualityOp -> pure (MultipleExpr [subsetA, subsetB])
  
deleteConstraintP :: Parser DatabaseContextExpr  
deleteConstraintP = do
  reserved "deleteconstraint"
  RemoveInclusionDependency <$> identifierP
  
-- key <constraint name> {<uniqueness attributes>} <uniqueness relexpr>
keyP :: Parser DatabaseContextExpr  
keyP = do
  reserved "key"
  keyName <- identifierP
  uniquenessAttrNames <- braces attributeListP
  uniquenessExpr <- relExprP
  let newIncDep = inclusionDependencyForKey uniquenessAttrNames uniquenessExpr
  pure $ AddInclusionDependency keyName newIncDep
  
funcDepP :: Parser DatabaseContextExpr  
funcDepP = do
  reserved "funcdep"
  keyName <- identifierP
  source <- parens attributeListP
  reserved "->"
  dependents <- parens attributeListP
  expr <- relExprP
  let newIncDeps = inclusionDependenciesForFunctionalDependency funcDep
      funcDep = FunctionalDependency source dependents expr
      nameA = keyName <> "_A"
      nameB = keyName <> "_B"
  pure (MultipleExpr [AddInclusionDependency nameA (fst newIncDeps),
                      AddInclusionDependency nameB (snd newIncDeps)])
  
attributeAssignmentP :: Parser (AttributeName, AtomExpr)
attributeAssignmentP = do
  attrName <- identifierP
  reservedOp ":="
  atomExpr <- atomExprP
  pure (attrName, atomExpr)
  
addNotificationP :: Parser DatabaseContextExpr
addNotificationP = do
  reserved "notify"
  notName <- identifierP
  triggerExpr <- relExprP 
  resultOldExpr <- relExprP
  AddNotification notName triggerExpr resultOldExpr <$> relExprP
  
removeNotificationP :: Parser DatabaseContextExpr  
removeNotificationP = do
  reserved "unnotify"
  RemoveNotification <$> identifierP

-- | data Hair = Bald | Color Text
addTypeConstructorP :: Parser DatabaseContextExpr
addTypeConstructorP = do
  reserved "data"
  typeConstructorDef <- typeConstructorDefP
  reservedOp "="
  dataConstructorDefs <- sepBy1 dataConstructorDefP pipe
  pure (AddTypeConstructor typeConstructorDef dataConstructorDefs)

removeTypeConstructorP :: Parser DatabaseContextExpr
removeTypeConstructorP = do
  reserved "undata"
  RemoveTypeConstructor <$> typeConstructorNameP
  
removeAtomFunctionP :: Parser DatabaseContextExpr  
removeAtomFunctionP = do
  reserved "removeatomfunction"
  RemoveAtomFunction <$> quotedString
  
removeDatabaseContextFunctionP :: Parser DatabaseContextExpr
removeDatabaseContextFunctionP = do
  reserved "removedatabasecontextfunction"
  RemoveDatabaseContextFunction <$> quotedString

addRegisteredQueryP :: Parser DatabaseContextExpr
addRegisteredQueryP = do
  reserved "registerquery"
  AddRegisteredQuery <$> quotedString <*> relExprP

removeRegisteredQueryP :: Parser DatabaseContextExpr
removeRegisteredQueryP = do
  reserved "unregisterquery"
  RemoveRegisteredQuery <$> quotedString

executeDatabaseContextFunctionP :: Parser DatabaseContextExpr
executeDatabaseContextFunctionP = do
  reserved "execute"
  funcName' <- functionNameP
  args <- parens (sepBy atomExprP comma)
  pure (ExecuteDatabaseContextFunction funcName' args)
  
databaseExprOpP :: Parser DatabaseContextExpr
databaseExprOpP = multipleDatabaseContextExprP

{-
evalDatabaseContextExpr :: Bool -> DatabaseContext -> DatabaseContextExpr -> Either RelationalError DatabaseContext
evalDatabaseContextExpr useOptimizer context expr = do
    optimizedExpr <- evalState (applyStaticDatabaseOptimization expr) (RE.freshDatabaseState context)
    case runState (RE.evalDatabaseContextExpr (if useOptimizer then optimizedExpr else expr)) (RE.freshDatabaseState context) of
        (Right (), (context',_, _)) -> Right context'
        (Left err, _) -> Left err
-}

interpretDatabaseContextExpr :: DatabaseContext -> TransactionId -> TransactionGraph -> T.Text -> Either RelationalError DatabaseContext
interpretDatabaseContextExpr context transId graph tutdstring =
  case parse databaseExprOpP "" tutdstring of
    Left err -> Left $ PM36E.ParseError (T.pack (show err))
    Right parsed -> do
      let env = RE.mkDatabaseContextEvalEnv transId graph
      RE.dbc_context <$> RE.runDatabaseContextEvalMonad context env (optimizeAndEvalDatabaseContextExpr True parsed)

{-
--no optimization
interpretNO :: DatabaseContext -> String -> (Maybe RelationalError, DatabaseContext)
interpretNO context tutdstring = case parseString tutdstring of
                                    Left err -> (Just err, context)
                                    Right parsed -> runState (evalContextExpr parsed) context
-}

