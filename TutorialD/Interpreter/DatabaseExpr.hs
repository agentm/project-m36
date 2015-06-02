module TutorialD.Interpreter.DatabaseExpr where
import Text.Parsec
import Text.Parsec.String
import ProjectM36.Base
import TutorialD.Interpreter.Base
import qualified Data.Text as T
import Control.Applicative ((<*), (*>))
import TutorialD.Interpreter.RelationalExpr
import qualified Data.Map as M
import Control.Monad.State
import ProjectM36.StaticOptimizer
import ProjectM36.Error
import ProjectM36.RelationalExpression

--parsers which create "database expressions" which modify the database context (such as relvar assignment)
databaseExprP :: Parser DatabaseExpr
databaseExprP = insertP
            <|> deleteP
            <|> updateP
            <|> constraintP
            <|> defineP
            <|> undefineP
            <|> assignP

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
  attributeSet <- makeAttributesP
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

constraintP :: Parser DatabaseExpr
constraintP = do
  reservedOp "constraint"
  constraintName <- identifier
  subset <- relExprP
  reservedOp "in"
  superset <- relExprP
  return $ AddInclusionDependency (InclusionDependency (T.pack constraintName) subset superset)

attributeAssignmentP :: Parser (String, Atom)
attributeAssignmentP = do
  attrName <- identifier
  reservedOp ":="
  atom <- stringAtomP <|> intAtomP
  return $ (attrName, atom)

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
