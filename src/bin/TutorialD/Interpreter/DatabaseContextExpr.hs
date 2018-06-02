module TutorialD.Interpreter.DatabaseContextExpr where
import Text.Megaparsec
import Text.Megaparsec.Text
import ProjectM36.Base
import TutorialD.Interpreter.Base
import qualified Data.Text as T
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.Types
import qualified Data.Map as M
import Control.Monad.State
import ProjectM36.StaticOptimizer
import qualified ProjectM36.Error as PM36E
import ProjectM36.Error
import qualified ProjectM36.RelationalExpression as RE
import ProjectM36.Key
import ProjectM36.FunctionalDependency
import Data.Monoid
import Data.Functor
import Debug.Trace

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
                               nothingP]
            
nothingP :: Parser DatabaseContextExpr            
nothingP = spaceConsumer >> pure NoOperation

assignP :: Parser DatabaseContextExpr
assignP = do
  relVarName <- try $ do
    relVarName <- identifier
    reservedOp ":="
    return relVarName
  expr <- relExprP
  pure $ Assign relVarName expr
  
multilineSep :: Parser T.Text  
multilineSep = newline >> pure "\n"

multipleDatabaseContextExprP :: Parser DatabaseContextExpr
multipleDatabaseContextExprP = 
  MultipleExpr <$> sepBy1 databaseContextExprP semi

insertP :: Parser DatabaseContextExpr
insertP = do
  reservedOp "insert"
  relvar <- identifier
  expr <- relExprP
  pure $ Insert relvar expr

defineP :: Parser DatabaseContextExpr
defineP = do
  relVarName <- try $ do
    relVarName <- identifier
    reservedOp "::"
    return relVarName
  attributeSet <- makeAttributeExprsP
  pure $ Define relVarName attributeSet

undefineP :: Parser DatabaseContextExpr
undefineP = do
  reservedOp "undefine"
  Undefine <$> identifier

deleteP :: Parser DatabaseContextExpr
deleteP = do
  reservedOp "delete"
  Delete <$> identifier <*> option TruePredicate (reservedOp "where" *> restrictionPredicateP)

updateP :: Parser DatabaseContextExpr
updateP = do
  reservedOp "update"
  relVarName <- identifier
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP <* spaceConsumer)
  attributeAssignments <- M.fromList <$> parens (sepBy attributeAssignmentP comma)
  return $ Update relVarName attributeAssignments predicate

data IncDepOp = SubsetOp | EqualityOp

addConstraintP :: Parser DatabaseContextExpr
addConstraintP = do
  reservedOp "constraint" <|> reservedOp "foreign key"
  constraintName <- identifier
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
  constraintName <- identifier
  pure $ RemoveInclusionDependency constraintName
  
-- key <constraint name> {<uniqueness attributes>} <uniqueness relexpr>
keyP :: Parser DatabaseContextExpr  
keyP = do
  reserved "key"
  keyName <- identifier
  uniquenessAttrNames <- braces attributeListP
  uniquenessExpr <- relExprP
  let newIncDep = inclusionDependencyForKey uniquenessAttrNames uniquenessExpr
  pure $ AddInclusionDependency keyName newIncDep
  
funcDepP :: Parser DatabaseContextExpr  
funcDepP = do
  reserved "funcdep"
  keyName <- identifier
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
  attrName <- identifier
  reservedOp ":="
  atomExpr <- atomExprP
  pure (attrName, atomExpr)
  
addNotificationP :: Parser DatabaseContextExpr
addNotificationP = do
  reserved "notify"
  notName <- identifier
  triggerExpr <- relExprP 
  resultOldExpr <- relExprP
  resultNewExpr <- relExprP
  pure $ AddNotification notName triggerExpr resultOldExpr resultNewExpr
  
removeNotificationP :: Parser DatabaseContextExpr  
removeNotificationP = do
  reserved "unnotify"
  notName <- identifier
  pure $ RemoveNotification notName

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
  RemoveTypeConstructor <$> identifier 
  
removeAtomFunctionP :: Parser DatabaseContextExpr  
removeAtomFunctionP = do
  reserved "removeatomfunction"
  RemoveAtomFunction <$> quotedString
  
removeDatabaseContextFunctionP :: Parser DatabaseContextExpr
removeDatabaseContextFunctionP = do
  reserved "removedatabasecontextfunction"
  RemoveDatabaseContextFunction <$> quotedString

executeDatabaseContextFunctionP :: Parser DatabaseContextExpr
executeDatabaseContextFunctionP = do
  reserved "execute"
  funcName <- identifier
  args <- parens (sepBy atomExprP comma)
  pure (ExecuteDatabaseContextFunction funcName args)
  
databaseExprOpP :: Parser DatabaseContextExpr
databaseExprOpP = multipleDatabaseContextExprP

evalDatabaseContextExpr :: Bool -> DatabaseContext -> DatabaseContextExpr -> Either RelationalError DatabaseContext
evalDatabaseContextExpr useOptimizer context expr = do
    optimizedExpr <- evalState (applyStaticDatabaseOptimization expr) (RE.freshDatabaseState context)
    case runState (RE.evalDatabaseContextExpr (if useOptimizer then optimizedExpr else expr)) (RE.freshDatabaseState context) of
        (Right (), (context',_, _)) -> Right context'
        (Left err, _) -> Left err


interpretDatabaseContextExpr :: DatabaseContext -> T.Text -> Either RelationalError DatabaseContext
interpretDatabaseContextExpr context tutdstring = case parse databaseExprOpP "" tutdstring of
                                    Left err -> Left $ PM36E.ParseError (T.pack (show err))
                                    Right parsed -> 
                                      evalDatabaseContextExpr True context parsed

{-
--no optimization
interpretNO :: DatabaseContext -> String -> (Maybe RelationalError, DatabaseContext)
interpretNO context tutdstring = case parseString tutdstring of
                                    Left err -> (Just err, context)
                                    Right parsed -> runState (evalContextExpr parsed) context
-}

