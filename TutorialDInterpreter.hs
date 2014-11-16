{-# LANGUAGE GADTs #-}
module TutorialDInterpreter where
import Relation
import RelationType
import RelationalError
import RelationExpr
import RelationTerm
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.List as L
import Control.Applicative (liftA)
import Control.Monad.State
import System.Console.Readline

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser tutD
        where tutD = emptyDef {
                Token.reservedOpNames = ["join", "where", "union", "group", "ungroup"],
                Token.reservedNames = [],
                Token.identStart = letter,
                Token.identLetter = alphaNum}

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
  attrs <- makeAttributes
  return $ MakeStaticRelation attrs HS.empty

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
          <|> (try defineP)
          <|> relVarP
          
projectOp = do
  attrs <- braces attributeList
  return $ Project (S.fromList attrs)
  
assignP = do
  relVarName <- identifier
  reservedOp ":="
  return $ Assign relVarName
  
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
  
insertP :: Parser (RelationalExpr -> RelationalExpr)
insertP = do
  reservedOp "insert"
  relvar <- identifier
  return $ Insert relvar
  
defineP :: Parser RelationalExpr
defineP = do
  relVarName <- identifier
  reservedOp "::"
  attributes <- makeAttributes
  return $ Define relVarName attributes
  
relOperators = [
  [Postfix projectOp],
  [Postfix renameP],
  [Postfix groupP],
  [Postfix ungroupP],
  [Infix (reservedOp "join" >> return Join) AssocLeft],
  [Infix (reservedOp "union" >> return Union) AssocLeft],
  [Prefix insertP],
  
  [Prefix (try assignP)]
  ]

relExpr :: Parser RelationalExpr
relExpr = buildExpressionParser relOperators relTerm

multipleRelExpr :: Parser RelationalExpr
multipleRelExpr = do 
  exprs <- sepBy1 relExpr semi
  return $ MultipleExpr exprs 
  
parseString :: String -> Either RelationalError RelationalExpr
parseString str = case parse multipleRelExpr "" str of
  Left err -> Left $ ParseError (show err)
  Right r -> Right r

data TutorialDOperator where
  ShowRelationVariableType :: String -> TutorialDOperator
  
interpreterOps :: Parser TutorialDOperator
interpreterOps = do
  reservedOp ":t"
  relVarName <- identifier
  return $ ShowRelationVariableType relVarName

showRelationAttributes :: Relation -> String
showRelationAttributes rel = "{" ++ concat (L.intersperse ", " $ map showAttribute attrs) ++ "}"
  where
    showAttribute (Attribute name atomType) = name ++ " " ++ case atomTypeToTutDType atomType of
      Just t -> show t
      Nothing -> "unknown"
    attrs = values (attributes rel)
    values m = map snd (M.toAscList m)
    
evalTutorialDOp :: RelVarContext -> TutorialDOperator -> String
evalTutorialDOp context (ShowRelationVariableType relVarName) = case M.lookup relVarName context of
  Just rel -> showRelationAttributes rel
  Nothing -> relVarName ++ " not defined"
  
example1 = "relA {a,b, c}"
example2 = "relA join relB"
example3 = "relA join relB {x,y,z}"
example4 = "(relA) {x,y,z}"
example5 = "relA union relB"
example6 = "rv1 := true"
example7 = "rv1 := relA union relB"
example8 = "relA := true; relB := false"
example9 = "relA := relation { SNO CHAR }"
  
interpret :: RelVarContext -> String -> (Either RelationalError Relation, RelVarContext)
interpret context tutdstring = case parseString tutdstring of
                                    Left err -> (Left err, context)
                                    Right parsed -> runState (eval parsed) context
                                    
-- for interpreter-specific operations                               
interpretOps :: RelVarContext -> String -> Maybe String                                    
interpretOps context instring = case parse interpreterOps "" instring of
  Left err -> Nothing
  Right ops -> Just $ evalTutorialDOp context ops
  
reprLoop :: RelVarContext -> IO ()
reprLoop context = do
  maybeLine <- readline "TutorialD: "
  case maybeLine of
    Nothing -> return ()
    Just line -> do 
      addHistory line
      
      case interpretOps context line of
        Just out -> do 
          putStrLn out
          reprLoop context
        Nothing -> return ()
        
      let (value, contextup) = interpret context line 
      case value of
        (Right rel) -> do
          putStrLn $ showRelation rel
          reprLoop contextup
        (Left err) -> do
          putStrLn $ show err
          reprLoop context
      
