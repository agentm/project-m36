{-# LANGUAGE GADTs #-}
module TutorialDInterpreter where
import Relation
import RelationType
import RelationTuple
import RelationTupleSet
import RelationalError
import RelationExpr
import RelationTerm
import RelationStaticOptimizer
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.List as L
import Control.Applicative (liftA, (<*), (*>))
import Control.Monad.State
import System.Console.Haskeline
import System.IO
import System.Directory (getHomeDirectory)

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser tutD
        where tutD = emptyDef {
                Token.reservedOpNames = ["join", "where", "union", "group", "ungroup"],
                Token.reservedNames = [],
                Token.identStart = letter <|> char '_',
                Token.identLetter = alphaNum <|> char '_' <|> char '#'} -- # needed for Date examples

parens = Token.parens lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
braces = Token.braces lexer
identifier = Token.identifier lexer
comma = Token.comma lexer
semi = Token.semi lexer

--used in projection
attributeList :: Parser [AttributeName]
attributeList = sepBy identifier comma

makeRelation :: Parser RelationalExpr
makeRelation = do
  reservedOp "relation"
  attrs <- (try makeAttributes <|> return M.empty)
  if not (M.null attrs) then do
    return $ MakeStaticRelation attrs emptyTupleSet
    else do
    tuples <- braces (sepBy tupleP comma)
    return $ MakeStaticRelation (tupleAttributes (head tuples)) (HS.fromList tuples)

--used in relation creation
makeAttributes :: Parser Attributes
makeAttributes = do
   attrList <- braces (sepBy attributeAndType comma)
   return $ M.fromList $ map toAttributeAssocList attrList
     where
       toAttributeAssocList attr@(Attribute attrName _) = (attrName, attr)

attributeAndType :: Parser Attribute
attributeAndType = do
  attrName <- identifier
  attrTypeName <- identifier
  --convert type name into type
  case tutDTypeToAtomType attrTypeName of
    Just t -> return $ Attribute attrName t
    Nothing -> fail (attrTypeName ++ " is not a valid type name.")
    
tupleP :: Parser RelationTuple    
tupleP = do
  reservedOp "tuple"
  attrValues <- braces (sepBy tupleAtomP comma)
  return $ RelationTuple (M.fromList attrValues)
  
tupleAtomP :: Parser (AttributeName, Atom)
tupleAtomP = do
  attributeName <- identifier
  attrType <- identifier
  atom <- parens (stringAtomP <|> intAtomP)
  case tutDTypeToAtomType attrType of
    Nothing -> fail (attrType ++ " is not a valid type name.")
    Just typeA -> if typeA == atomTypeForAtom atom then
                    return $ (attributeName, atom)
                  else
                    fail "type mismatch in tuple generation"
  
--convert Tutorial D type to AtomType
tutDTypeToAtomType :: String -> Maybe AtomType
tutDTypeToAtomType tutDType = case tutDType of
  "char" -> Just StringAtomType
  "int" -> Just IntAtomType
  _ -> Nothing

atomTypeToTutDType :: AtomType -> Maybe String
atomTypeToTutDType atomType = case atomType of
  StringAtomType -> Just "char"
  IntAtomType -> Just "int"
  --RelationAtomType rel -> 
  _ -> Nothing

relVarP :: Parser RelationalExpr
relVarP = liftA RelationVariable identifier

relTerm = parens relExpr
          <|> makeRelation
          <|> relVarP
          
projectOp = do
  attrs <- braces attributeList
  return $ Project (S.fromList attrs)
  
assignP :: Parser DatabaseExpr
assignP = do
  relVarName <- try $ do
    relVarName <- identifier
    reservedOp ":="
    return relVarName
  expr <- relExpr
  return $ Assign relVarName expr
  
renameClause = do
  oldAttr <- identifier 
  reservedOp "as"
  newAttr <- identifier
  return $ (oldAttr, newAttr)
  
renameP :: Parser (RelationalExpr -> RelationalExpr)
renameP = do
  reservedOp "rename"
  (oldAttr, newAttr) <- braces renameClause
  return $ Rename oldAttr newAttr 
  
whereClauseP = do
  reservedOp "where"  
  boolExpr <- restrictionPredicateP
  return $ Restrict boolExpr
  
groupClause = do  
  attrs <- braces attributeList
  reservedOp "as"
  newAttrName <- identifier
  return $ (attrs, newAttrName)
  
groupP :: Parser (RelationalExpr -> RelationalExpr)
groupP = do
  reservedOp "group"
  (groupAttrList, groupAttrName) <- parens groupClause
  return $ Group (S.fromList groupAttrList) groupAttrName
  
--in "Time and Relational Theory" (2014), Date's Tutorial D grammar for ungroup takes one attribute, while in previous books, it take multiple arguments. Let us assume that nested ungroups are the same as multiple attributes.
ungroupP :: Parser (RelationalExpr -> RelationalExpr)
ungroupP = do
  reservedOp "ungroup"
  rvaAttrName <- identifier
  return $ Ungroup rvaAttrName
  
relOperators = [
  [Postfix projectOp],
  [Postfix renameP],
  [Postfix whereClauseP],
  [Postfix groupP],
  [Postfix ungroupP],
  [Infix (reservedOp "join" >> return Join) AssocLeft],
  [Infix (reservedOp "union" >> return Union) AssocLeft],
  [Infix (reservedOp "=" >> return Equals) AssocNone]
  ]

relExpr :: Parser RelationalExpr
relExpr = buildExpressionParser relOperators relTerm

databaseExpr :: Parser DatabaseExpr
databaseExpr = insertP
            <|> deleteP
            <|> updateP
            <|> constraintP
            <|> defineP
            <|> undefineP
            <|> assignP
            
multipleDatabaseExpr :: Parser DatabaseExpr
multipleDatabaseExpr = do
  exprs <- sepBy1 databaseExpr semi
  return $ MultipleExpr exprs
  
insertP :: Parser DatabaseExpr
insertP = do
  reservedOp "insert"
  relvar <- identifier
  expr <- relExpr
  return $ Insert relvar expr
  
defineP :: Parser DatabaseExpr
defineP = do
  relVarName <- try $ do
    relVarName <- identifier
    reservedOp "::"
    return relVarName
  attributes <- makeAttributes
  return $ Define relVarName attributes
  
undefineP :: Parser DatabaseExpr
undefineP = do
  reservedOp "undefine"
  relVarName <- identifier
  return $ Undefine relVarName
  
deleteP :: Parser DatabaseExpr  
deleteP = do
  reservedOp "delete"
  relVarName <- identifier
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP)
  return $ Delete relVarName predicate
  
updateP :: Parser DatabaseExpr
updateP = do
  reservedOp "update"
  relVarName <- identifier  
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP <* spaces)
  attributeAssignments <- liftM M.fromList $ parens (sepBy attributeAssignment comma)
  return $ Update relVarName attributeAssignments predicate
  
constraintP :: Parser DatabaseExpr
constraintP = do
  reservedOp "constraint"
  constraintName <- identifier
  subset <- relExpr
  reservedOp "in"
  superset <- relExpr
  return $ AddInclusionDependency (InclusionDependency constraintName subset superset)
  
attributeAssignment :: Parser (String, Atom)
attributeAssignment = do
  attrName <- identifier
  reservedOp ":="
  atom <- stringAtomP <|> intAtomP
  return $ (attrName, atom)
  
parseString :: String -> Either RelationalError DatabaseExpr
parseString str = case parse multipleDatabaseExpr "" str of
  Left err -> Left $ ParseError (show err)
  Right r -> Right r

data TutorialDOperator where
  ShowRelation :: RelationalExpr -> TutorialDOperator
  ShowRelationType :: RelationalExpr -> TutorialDOperator
  ShowConstraint :: String -> TutorialDOperator
  Quit :: TutorialDOperator
  deriving (Show)
  
typeP :: Parser TutorialDOperator  
typeP = do
  reservedOp ":t"
  expr <- relExpr
  return $ ShowRelationType expr
  
showRelP :: Parser TutorialDOperator
showRelP = do
  reservedOp ":s"
  expr <- relExpr
  return $ ShowRelation expr
  
quitP :: Parser TutorialDOperator
quitP = do
  reservedOp ":q"
  return Quit
  
showConstraintsP :: Parser TutorialDOperator
showConstraintsP = do
  reservedOp ":c"
  constraintName <- option "" identifier
  return $ ShowConstraint constraintName
  
interpreterOps :: Parser TutorialDOperator
interpreterOps = typeP 
                 <|> showRelP
                 <|> showConstraintsP
                 <|> quitP

showRelationAttributes :: Relation -> String
showRelationAttributes rel = "{" ++ concat (L.intersperse ", " $ map showAttribute attrs) ++ "}"
  where
    showAttribute (Attribute name atomType) = name ++ " " ++ case atomTypeToTutDType atomType of
      Just t -> show t
      Nothing -> "unknown"
    attrs = values (attributes rel)
    values m = map snd (M.toAscList m)
    
data TutorialDOperatorResult = QuitResult |
                               DisplayResult String |
                               DisplayErrorResult String |
                               NoActionResult
    
evalTutorialDOp :: DatabaseContext -> TutorialDOperator -> TutorialDOperatorResult
evalTutorialDOp context (ShowRelationType expr) = case runState (typeForRelationalExpr expr) context of
  (Right rel, _) -> DisplayResult $ showRelationAttributes rel
  (Left err, _) -> DisplayErrorResult $ show err
  
evalTutorialDOp context (ShowRelation expr) = do
  case runState (evalRelationalExpr expr) context of 
    (Left err, _) -> DisplayErrorResult $ show err
    (Right rel, _) -> DisplayResult $ showRelation rel
    
evalTutorialDOp context (ShowConstraint name) = do
  DisplayResult $ show filteredDeps
  where
    deps = inclusionDependencies context
    filteredDeps = case name of
      "" -> deps
      name -> HS.filter (\(InclusionDependency n _ _) -> n == name) deps
      
evalTutorialDOp context (Quit) = QuitResult
  
example1 = "relA {a,b, c}"
example2 = "relA join relB"
example3 = "relA join relB {x,y,z}"
example4 = "(relA) {x,y,z}"
example5 = "relA union relB"
example6 = "rv1 := true"
example7 = "rv1 := relA union relB"
example8 = "relA := true; relB := false"
example9 = "relA := relation { SNO CHAR }"
  
interpret :: DatabaseContext -> String -> (Maybe RelationalError, DatabaseContext)
interpret context tutdstring = case parseString tutdstring of
                                    Left err -> (Just err, context)
                                    Right parsed -> runState (evaluator parsed) context
                               where 
                                 evaluator expr = do
                                   context <- get
                                   optExpr <- applyStaticDatabaseOptimization expr 
                                   --case optExpr of
                                   case optExpr of
                                     Left err -> return $ Just err
                                     Right optExprA -> evalContextExpr optExprA

--no optimization
interpretNO :: DatabaseContext -> String -> (Maybe RelationalError, DatabaseContext)
interpretNO context tutdstring = case parseString tutdstring of
                                    Left err -> (Just err, context)
                                    Right parsed -> runState (evalContextExpr parsed) context

                                    
-- for interpreter-specific operations                               
interpretOps :: DatabaseContext -> String -> TutorialDOperatorResult
interpretOps context instring = case parse interpreterOps "" instring of
  Left err -> NoActionResult
  Right ops ->  evalTutorialDOp context ops
  
reprLoop :: DatabaseContext -> IO ()
reprLoop context = do
  homeDirectory <- getHomeDirectory
  let settings = defaultSettings {historyFile = Just (homeDirectory ++ "/.tutd_history")}      
  maybeLine <- runInputT settings $ do
                     getInputLine "TutorialD: "
  case maybeLine of
    Nothing -> return ()
    Just line -> do 
    case interpretOps context line of
      QuitResult -> return ()
      DisplayErrorResult err -> hPutStrLn stderr ("ERR: " ++ err) >> reprLoop context
      DisplayResult out -> do
        putStrLn out
        reprLoop context
      NoActionResult -> do
      let (value, contextup) = interpret context line 
      case value of
        Nothing -> reprLoop contextup
        (Just err) -> do
          hPutStrLn stderr ("ERR:" ++ show err)
          reprLoop context
      
restrictionPredicateP :: Parser RestrictionPredicateExpr
restrictionPredicateP = buildExpressionParser predicateOperators predicateTerm
  where
    predicateOperators = [
      [Prefix (reservedOp "not" >> return NotPredicate)],
      [Infix (reservedOp "and" >> return AndPredicate) AssocLeft],
      [Infix (reservedOp "or" >> return OrPredicate) AssocLeft]
      ]
    predicateTerm = parens restrictionPredicateP
                    <|> try restrictionAttributeEqualityP
                    <|> try relationalBooleanExpr

                    
relationalBooleanExpr :: Parser RestrictionPredicateExpr                   
relationalBooleanExpr = do
  relexpr <- relExpr
  return $ RelationalExprPredicate relexpr

restrictionAttributeEqualityP :: Parser RestrictionPredicateExpr
restrictionAttributeEqualityP = do
  attributeName <- identifier
  reservedOp "="
  atom <- stringAtomP <|> intAtomP
  return $ AttributeEqualityPredicate attributeName atom
  
quotedChar = noneOf "\""
           <|> try (string "\"\"" >> return '"')
           
stringAtomP = liftA StringAtom (string "\"" *> many quotedChar <* string "\"")

intAtomP = do
  intstr <- many digit
  return $ IntAtom (read intstr)
  