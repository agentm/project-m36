module TutorialD.Interpreter.RelationalExpr where
import Text.Parsec
import Text.Parsec.Expr
import ProjectM36.Base
import Text.Parsec.String
import TutorialD.Interpreter.Base
import qualified Data.Text as T
import qualified ProjectM36.Attribute as A
import ProjectM36.TupleSet
import ProjectM36.Tuple
import ProjectM36.Relation
import qualified Data.HashSet as HS
import ProjectM36.Atom
import qualified Data.Vector as V
import qualified Data.Set as S
import Control.Applicative (liftA, (<*), (*>), (<$>))
import Data.Functor.Identity (Identity)
import Data.Time.Format
import System.Locale

atomTypeP :: Parser AtomType
atomTypeP = (reserved "char" *> return StringAtomType) <|>
  (reserved "int" *> return IntAtomType) <|>
  (reserved "datetime" *> return DateTimeAtomType) <|>
  (reserved "date" *> return DateAtomType) <|>
  (reserved "double" *> return DoubleAtomType) <|>
  (reserved "bool" *> return BoolAtomType) <|>
  (RelationAtomType <$> (reserved "relation" *> makeAttributesP))

--used in projection
attributeListP :: Parser AttributeNames
attributeListP = do
  but <- try (string "all but " <* whiteSpace) <|> string ""
  let constructor = if but == "" then AttributeNames else InvertedAttributeNames
  attrs <- sepBy identifier comma
  return $ constructor (S.fromList (map T.pack attrs))

makeRelationP :: Parser RelationalExpr
makeRelationP = ExistingRelation <$> relationP

--used in relation creation
makeAttributesP :: Parser Attributes
makeAttributesP = do
   attrList <- braces (sepBy attributeAndTypeP comma)
   return $ A.attributesFromList attrList

attributeAndTypeP :: Parser Attribute
attributeAndTypeP = do
  attrName <- identifier
  --convert type name into type
  atomType <- atomTypeP
  return $ Attribute (T.pack attrName) atomType

tupleP :: Parser RelationTuple
tupleP = do
  reservedOp "tuple"
  attrAssocs <- braces (sepBy tupleAtomP comma)
  let attrs = A.attributesFromList $ map (\(attrName, atom) -> Attribute attrName (atomTypeForAtom atom)) attrAssocs
  let atoms = V.fromList $ map snd attrAssocs
  return $ mkRelationTuple attrs atoms

tupleAtomP :: Parser (AttributeName, Atom)
tupleAtomP = do
  attributeName <- identifier
  atom <- atomP
  return $ (T.pack attributeName, atom)      
    
projectP :: Parser (RelationalExpr -> RelationalExpr)
projectP = do
  attrs <- braces attributeListP
  return $ Project attrs

renameClauseP :: Parser (String, String)
renameClauseP = do
  oldAttr <- identifier
  reservedOp "as"
  newAttr <- identifier
  return $ (oldAttr, newAttr)

renameP :: Parser (RelationalExpr -> RelationalExpr)
renameP = do
  reservedOp "rename"
  (oldAttr, newAttr) <- braces renameClauseP
  return $ Rename (T.pack oldAttr) (T.pack newAttr)

whereClauseP :: Parser (RelationalExpr -> RelationalExpr)
whereClauseP = do
  reservedOp "where"
  boolExpr <- restrictionPredicateP
  return $ Restrict boolExpr

groupClauseP :: Parser (AttributeNames, String)
groupClauseP = do
  attrs <- braces attributeListP
  reservedOp "as"
  newAttrName <- identifier
  return $ (attrs, newAttrName)

groupP :: Parser (RelationalExpr -> RelationalExpr)
groupP = do
  reservedOp "group"
  (groupAttrList, groupAttrName) <- parens groupClauseP
  return $ Group groupAttrList (T.pack groupAttrName)

--in "Time and Relational Theory" (2014), Date's Tutorial D grammar for ungroup takes one attribute, while in previous books, it take multiple arguments. Let us assume that nested ungroups are the same as multiple attributes.
ungroupP :: Parser (RelationalExpr -> RelationalExpr)
ungroupP = do
  reservedOp "ungroup"
  rvaAttrName <- identifier
  return $ Ungroup (T.pack rvaAttrName)

extendP :: Parser (RelationalExpr -> RelationalExpr)
extendP = do
  reservedOp ":"
  tupleExpr <- braces tupleExpressionP
  return $ Extend tupleExpr

{-
rewrite summarize as extend as evaluate
it turns out that summarize must be a relational expression primitive because it requires

summarize SP per (SP{SNO}) add (sum(@QTY) as SUMQ) === ((SP rename {QTY as _}) group ({_} as x)) : {QTY:=sum(@x)}{S#,QTY}
so, in general,
summarize rX per (relExpr) add (func(@attr) as funcd) === ((rX rename {attr as _}) group ({_} as _)) : {QTY:=sum(@_)}{relExpr, _}
-}
{-
summarizeP :: Parser (RelationalExpr -> RelationalExpr)
summarizeP = do
  reservedOp "per"
  -- "summarize in Tutorial D requires the heading of the per relation to be a subset  of that of the relation to be summarized"
  perExpr <- parens relExpr
  reservedOp "add"
  (funcAtomExpr, newAttrName) <- parens summaryP
  --rewrite summarize as extend
  --return $ Summarize funcAtomExpr newAttrName perExpr

summaryP :: Parser (AtomExpr, AttributeName)
summaryP = do
  expr <- atomExprP
  reserved "as"
  attrName <- identifier
  return (expr, T.pack attrName)
-}

relOperators :: [[Operator String () Identity RelationalExpr]]
relOperators = [
  [Postfix projectP],
  [Postfix renameP],
  [Postfix whereClauseP],
  [Postfix groupP],
  [Postfix ungroupP],
  [Infix (reservedOp "join" >> return Join) AssocLeft],
  [Infix (reservedOp "union" >> return Union) AssocLeft],
  [Infix (reservedOp "=" >> return Equals) AssocNone],
  [Postfix extendP]
  ]

relExprP :: Parser RelationalExpr
relExprP = buildExpressionParser relOperators relTerm

relVarP :: Parser RelationalExpr
relVarP = liftA (RelationVariable . T.pack) identifier

relTerm :: Parser RelationalExpr
relTerm = parens relExprP
          <|> makeRelationP
          <|> relVarP

restrictionPredicateP :: Parser RestrictionPredicateExpr
restrictionPredicateP = buildExpressionParser predicateOperators predicateTerm
  where
    predicateOperators = [
      [Prefix (reservedOp "not" >> return NotPredicate)],
      [Infix (reservedOp "and" >> return AndPredicate) AssocLeft],
      [Infix (reservedOp "or" >> return OrPredicate) AssocLeft]
      ]
    predicateTerm = parens restrictionPredicateP
    		    <|> try restrictionAtomExprP	   
                    <|> try restrictionAttributeEqualityP
                    <|> try relationalBooleanExprP


relationalBooleanExprP :: Parser RestrictionPredicateExpr
relationalBooleanExprP = do
  relexpr <- relExprP
  return $ RelationalExprPredicate relexpr
  
restrictionAttributeEqualityP :: Parser RestrictionPredicateExpr
restrictionAttributeEqualityP = do
  attributeName <- identifier
  reservedOp "="
  atomexpr <- atomExprP
  return $ AttributeEqualityPredicate (T.pack attributeName) atomexpr

restrictionAtomExprP :: Parser RestrictionPredicateExpr --atoms which are of type "boolean"
restrictionAtomExprP = do
  _ <- char '^' -- not ideal, but allows me to continue to use a context-free grammar
  AtomExprPredicate <$> atomExprP

multiTupleExpressionP :: Parser [TupleExpr]
multiTupleExpressionP = sepBy tupleExpressionP comma

tupleExpressionP :: Parser TupleExpr
tupleExpressionP = attributeTupleExpressionP

attributeTupleExpressionP :: Parser TupleExpr
attributeTupleExpressionP = do
  newAttr <- identifier
  reservedOp ":="
  atom <- atomExprP
  return $ AttributeTupleExpr (T.pack newAttr) atom

atomExprP :: Parser AtomExpr
atomExprP = try functionAtomExprP <|>
  attributeAtomExprP <|>
  nakedAtomExprP <|>
  relationalAtomExprP

attributeAtomExprP :: Parser AtomExpr
attributeAtomExprP = do
  _ <- string "@"
  attrName <- identifier
  return $ AttributeAtomExpr (T.pack attrName)

nakedAtomExprP :: Parser AtomExpr
nakedAtomExprP = NakedAtomExpr <$> atomP

atomP :: Parser Atom
atomP = dateTimeAtomP <|> dateAtomP <|> stringAtomP <|> doubleAtomP <|> intAtomP <|> boolAtomP <|> relationAtomP

functionAtomExprP :: Parser AtomExpr
functionAtomExprP = do
  funcName <- identifier
  argList <- parens (sepBy atomExprP comma)
  return $ FunctionAtomExpr (T.pack funcName) argList

relationalAtomExprP :: Parser AtomExpr
relationalAtomExprP = RelationAtomExpr <$> relExprP

stringAtomP :: Parser Atom
stringAtomP = liftA (StringAtom . T.pack) quotedString

dateTimeAtomP :: Parser Atom
dateTimeAtomP = do
  dateTimeString' <- try $ do
    dateTimeString <- quotedString
    reserved "::datetime"
    return dateTimeString
  --deprecated in time 1.5- needs update for GHC 7.10
  case parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" dateTimeString' of
    Just utctime -> return $ DateTimeAtom utctime
    Nothing -> fail "Failed to parse datetime"
    
dateAtomP :: Parser Atom    
dateAtomP = do
  dateString' <- try $ do
    dateString <- quotedString
    reserved "::date"
    return dateString
  case parseTime defaultTimeLocale "%Y-%m-%d" dateString' of
    Just utctime -> return $ DateAtom utctime
    Nothing -> fail "Failed to parse date"
    
doubleAtomP :: Parser Atom    
doubleAtomP = DoubleAtom <$> (try float)
  
intAtomP :: Parser Atom
intAtomP = (IntAtom . fromIntegral) <$> integer

boolAtomP :: Parser Atom
boolAtomP = do
  val <- char 't' <|> char 'f'
  return $ BoolAtom (val == 't')
  
relationAtomP :: Parser Atom
relationAtomP = RelationAtom <$> relationP

--relation constructor -- no choice but to propagate relation-construction errors as parse errors. Perhaps this could be improved in the future
relationP :: Parser Relation
relationP = do
  reserved "relation"
  attrs <- (try makeAttributesP <|> return A.emptyAttributes)
  if not (A.null attrs) then do
    case mkRelation attrs emptyTupleSet of 
      Left err -> fail (show err)
      Right rel -> return rel
    else do
    tuples <- braces (sepBy tupleP comma)
    case mkRelation (tupleAttributes (head tuples)) (HS.fromList tuples) of
      Left err -> fail (show err)
      Right rel -> return rel
  