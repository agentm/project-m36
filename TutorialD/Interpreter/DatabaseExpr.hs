{-# LANGUAGE OverloadedStrings #-}
module TutorialD.Interpreter.DatabaseExpr where
import Text.Parsec
import Text.Parsec.String
import ProjectM36.Base
import TutorialD.Interpreter.Base
import qualified Data.Text as T
import TutorialD.Interpreter.RelationalExpr
import qualified Data.Map as M
import Control.Monad.State
import ProjectM36.StaticOptimizer
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.Key

--parsers which create "database expressions" which modify the database context (such as relvar assignment)
databaseExprP :: Parser DatabaseExpr
databaseExprP = insertP
            <|> deleteConstraintP
            <|> deleteP
            <|> updateP
            <|> addConstraintP
            <|> keyP
            <|> defineP
            <|> undefineP
            <|> assignP
            <|> addNotificationP
            <|> removeNotificationP
            <|> addAtomConstructorP
            <|> removeAtomConstructorP

assignP :: Parser DatabaseExpr
assignP = do
  relVarName <- try $ do
    relVarName <- identifier
    reservedOp ":="
    return relVarName
  expr <- relExprP
  return $ Assign (T.pack relVarName) expr

multipleDatabaseExprP :: Parser DatabaseExpr
multipleDatabaseExprP = do
  exprs <- sepBy1 databaseExprP semi
  return $ MultipleExpr exprs

insertP :: Parser DatabaseExpr
insertP = do
  reservedOp "insert"
  relvar <- identifier
  expr <- relExprP
  return $ Insert (T.pack relvar) expr

defineP :: Parser DatabaseExpr
defineP = do
  relVarName <- try $ do
    relVarName <- identifier
    reservedOp "::"
    return relVarName
  attributeSet <- makeAttributeExprsP
  return $ Define (T.pack relVarName) attributeSet

undefineP :: Parser DatabaseExpr
undefineP = do
  reservedOp "undefine"
  relVarName <- identifier
  return $ Undefine (T.pack relVarName)

deleteP :: Parser DatabaseExpr
deleteP = do
  reservedOp "delete"
  relVarName <- identifier
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP)
  return $ Delete (T.pack relVarName) predicate

updateP :: Parser DatabaseExpr
updateP = do
  reservedOp "update"
  relVarName <- identifier
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP <* spaces)
  attributeAssignments <- liftM M.fromList $ parens (sepBy attributeAssignmentP comma)
  return $ Update (T.pack relVarName) (M.mapKeys T.pack $ attributeAssignments) predicate

addConstraintP :: Parser DatabaseExpr
addConstraintP = do
  reservedOp "constraint" <|> reservedOp "foreign key"
  constraintName <- identifier
  subset <- relExprP
  reservedOp "in"
  superset <- relExprP
  return $ AddInclusionDependency (T.pack constraintName) (InclusionDependency subset superset)
  
deleteConstraintP :: Parser DatabaseExpr  
deleteConstraintP = do
  reserved "deleteconstraint"
  constraintName <- identifier
  return $ RemoveInclusionDependency (T.pack constraintName)
  
-- key <constraint name> {<uniqueness attributes>} <uniqueness relexpr>
keyP :: Parser DatabaseExpr  
keyP = do
  reserved "key"
  keyName <- identifier
  uniquenessAttrNames <- braces attributeListP
  uniquenessExpr <- relExprP
  let newIncDep = inclusionDependencyForKey uniquenessAttrNames uniquenessExpr
  return $ AddInclusionDependency (T.pack keyName) newIncDep
  
attributeAssignmentP :: Parser (String, Atom)
attributeAssignmentP = do
  attrName <- identifier
  reservedOp ":="
  atom <- stringAtomP <|> intAtomP
  return $ (attrName, atom)
  
addNotificationP :: Parser DatabaseExpr
addNotificationP = do
  reserved "notify"
  notName <- identifier
  triggerExpr <- relExprP 
  resultExpr <- relExprP
  return $ AddNotification (T.pack notName) triggerExpr resultExpr
  
removeNotificationP :: Parser DatabaseExpr  
removeNotificationP = do
  reserved "unnotify"
  notName <- identifier
  return $ RemoveNotification (T.pack notName)

-- | data Hair = Bald | Color Text
addAtomConstructorP :: Parser DatabaseExpr
addAtomConstructorP = do
  reserved "data"
  typeConstructorName <- identifier
  reservedOp "="
  dataConstructors <- sepBy1 dataConstructorP pipe
  let aCons = AtomConstructor (M.fromList dataConstructors)
  pure (AddAtomConstructor (T.pack typeConstructorName) aCons)

dataConstructorP :: Parser (DataConstructorName, [AtomTypeName])
dataConstructorP = do
  dConsName <- identifier
  atomTypeNames <- sepBy (liftM T.pack identifier) spaces
  pure (T.pack dConsName, atomTypeNames)

removeAtomConstructorP :: Parser DatabaseExpr
removeAtomConstructorP = do
  reserved "undata"
  RemoveAtomConstructor <$> liftM T.pack identifier 

databaseExprOpP :: Parser DatabaseExpr
databaseExprOpP = multipleDatabaseExprP

evalDatabaseExpr :: Bool -> DatabaseContext -> DatabaseExpr -> Either RelationalError DatabaseContext
evalDatabaseExpr useOptimizer context expr = do
    optimizedExpr <- evalState (applyStaticDatabaseOptimization expr) context
    case runState (evalContextExpr (if useOptimizer then optimizedExpr else expr)) context of
        (Nothing, context') -> Right context'
        (Just err, _) -> Left err


interpretDatabaseExpr :: DatabaseContext -> String -> Either RelationalError DatabaseContext
interpretDatabaseExpr context tutdstring = case parse databaseExprOpP "" tutdstring of
                                    Left err -> Left $ ParseError (T.pack (show err))
                                    Right parsed -> evalDatabaseExpr True context parsed

{-
--no optimization
interpretNO :: DatabaseContext -> String -> (Maybe RelationalError, DatabaseContext)
interpretNO context tutdstring = case parseString tutdstring of
                                    Left err -> (Just err, context)
                                    Right parsed -> runState (evalContextExpr parsed) context
-}

