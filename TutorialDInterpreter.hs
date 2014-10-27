module TutorialDInterpreter where
import RelationType
import Relation
import RelationExpr
import RelationTuple
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Control.Applicative ((<$), (<*), (*>), liftA, liftA2)
import Control.Monad.State

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser tutD
        where tutD = emptyDef {
                Token.reservedOpNames = ["join", "where", "union"],
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
attributeList = braces (sepBy identifier comma)

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

relVarP :: Parser RelationalExpr
relVarP = liftA RelationVariable identifier

relTerm = parens relExpr
          <|> makeRelation
          <|> relVarP
          
projectOp = do
  attrs <- attributeList
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

relOperators = [
  [Postfix projectOp],
  [Postfix renameP],
  [Infix (reservedOp "join" >> return Join) AssocLeft],
  [Infix (reservedOp "union" >> return Union) AssocLeft],
  [Prefix (try assignP)]
  ]

relExpr :: Parser RelationalExpr
relExpr = buildExpressionParser relOperators relTerm

multipleRelExpr :: Parser RelationalExpr
multipleRelExpr = do 
  exprs <- sepBy1 relExpr semi
  return $ MultipleExpr exprs 
  
parseString :: String -> RelationalExpr
parseString str = case parse multipleRelExpr "" str of
  Left err -> error $ show err
  Right r -> r
  
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
interpret context tutdstring = runState (eval (parseString tutdstring)) context

