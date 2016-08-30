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
import ProjectM36.RelationalExpression
import ProjectM36.Key
import Data.Monoid

--parsers which create "database expressions" which modify the database context (such as relvar assignment)
databaseExprP :: Parser DatabaseContextExpr
databaseExprP = choice $ map (\p -> p <* optional commentP) [insertP,
                                              deleteConstraintP,
                                              deleteP,
                                              updateP,
                                              addConstraintP,
                                              keyP,
                                              defineP,
                                              undefineP,
                                              assignP,
                                              addNotificationP,
                                              removeNotificationP,
                                              addTypeConstructorP,
                                              removeTypeConstructorP,
                                              nothingP]
            
commentP :: Parser DatabaseContextExpr            
commentP = reserved "--" >> manyTill anyChar eof >> pure NoOperation
            
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

multipleDatabaseContextExprP :: Parser DatabaseContextExpr
multipleDatabaseContextExprP = do
  exprs <- sepBy1 databaseExprP semi
  pure $ MultipleExpr exprs

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
  relVarName <- identifier
  return $ Undefine relVarName

deleteP :: Parser DatabaseContextExpr
deleteP = do
  reservedOp "delete"
  relVarName <- identifier
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP)
  return $ Delete relVarName predicate

updateP :: Parser DatabaseContextExpr
updateP = do
  reservedOp "update"
  relVarName <- identifier
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP <* spaceConsumer)
  attributeAssignments <- liftM M.fromList $ parens (sepBy attributeAssignmentP comma)
  return $ Update relVarName attributeAssignments predicate

data IncDepOp = SubsetOp | EqualityOp

addConstraintP :: Parser DatabaseContextExpr
addConstraintP = do
  reservedOp "constraint" <|> reservedOp "foreign key"
  constraintName <- identifier
  subset <- relExprP
  op <- (reservedOp "in" *> pure SubsetOp) <|> (reservedOp "equals" *> pure EqualityOp)
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
  
attributeAssignmentP :: Parser (AttributeName, AtomExpr)
attributeAssignmentP = do
  attrName <- identifier
  reservedOp ":="
  atomExpr <- atomExprP
  pure $ (attrName, atomExpr)
  
addNotificationP :: Parser DatabaseContextExpr
addNotificationP = do
  reserved "notify"
  notName <- identifier
  triggerExpr <- relExprP 
  resultExpr <- relExprP
  pure $ AddNotification notName triggerExpr resultExpr
  
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

databaseExprOpP :: Parser DatabaseContextExpr
databaseExprOpP = multipleDatabaseContextExprP

evalDatabaseContextExpr :: Bool -> DatabaseContext -> DatabaseContextExpr -> Either RelationalError DatabaseContext
evalDatabaseContextExpr useOptimizer context expr = do
    optimizedExpr <- evalState (applyStaticDatabaseOptimization expr) context
    case runState (evalContextExpr (if useOptimizer then optimizedExpr else expr)) context of
        (Nothing, context') -> Right context'
        (Just err, _) -> Left err


interpretDatabaseContextExpr :: DatabaseContext -> T.Text -> Either RelationalError DatabaseContext
interpretDatabaseContextExpr context tutdstring = case parse databaseExprOpP "" tutdstring of
                                    Left err -> Left $ PM36E.ParseError (T.pack (show err))
                                    Right parsed -> evalDatabaseContextExpr True context parsed

{-
--no optimization
interpretNO :: DatabaseContext -> String -> (Maybe RelationalError, DatabaseContext)
interpretNO context tutdstring = case parseString tutdstring of
                                    Left err -> (Just err, context)
                                    Right parsed -> runState (evalContextExpr parsed) context
-}

