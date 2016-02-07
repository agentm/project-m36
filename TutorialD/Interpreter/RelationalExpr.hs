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
import ProjectM36.Atom
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Functor.Identity (Identity)
import Data.Time.Format
import Control.Applicative (liftA)
import Data.ByteString.Base64 as B64
import Data.Text.Encoding as TE
import TutorialD.Interpreter.DataTypes.DateTime
import TutorialD.Interpreter.DataTypes.Interval

import Data.Time.Calendar (Day)

--nuke this
{-
atomTypeP :: Parser AtomType
atomTypeP = (reserved "char" *> return stringAtomType) <|>
  (reserved "int" *> return intAtomType) <|>
  (reserved "datetime" *> return dateTimeAtomType) <|>
  (reserved "date" *> return dateAtomType) <|>
  (reserved "double" *> return doubleAtomType) <|>
  (reserved "bool" *> return boolAtomType) <|>
  (reserved "bytestring" *> return byteStringAtomType) <|>
  (RelationAtomType <$> (reserved "relation" *> makeAttributesP))
-}

--used in projection
attributeListP :: Parser AttributeNames
attributeListP = do
  but <- try (string "all but " <* whiteSpace) <|> string ""
  let constructor = if but == "" then AttributeNames else InvertedAttributeNames
  attrs <- sepBy identifier comma
  return $ constructor (S.fromList (map T.pack attrs))

makeRelationP :: Parser RelationalExpr
makeRelationP = do
  reserved "relation"
  attrs <- (try makeAttributesP <|> return A.emptyAttributes)
  if not (A.null attrs) then do
    case mkRelation attrs emptyTupleSet of 
      Left err -> fail (show err)
      Right rel -> pure (ExistingRelation rel)
    else do -- empty attributes
      tupleExprs <- try (braces (sepBy tupleExprP comma)) <|> pure []
      pure $ MakeRelationFromAtomExprs attrs 


makeAttributeExprsP :: Parser [AttributeExpr]
makeAttributeExprsP = do
  attrList <- bra

--used in relation creation
makeAttributeExprsP :: Parser [AttributeExprs]
makeAttributeExprsP = do
   attrList <- braces (sepBy attributeAndTypeNameP comma)
   return $ A.attributesFromList attrList


attributeAndTypeNameP :: Parser AttributeExpr
attributeAndTypeNameP = do
  attrName <- identifier
  --convert type name into type
  atomType <- identifier
  return $ AttributeAndTypeNameExpr (T.pack attrName) atomType

tupleExprP :: Parser TupleExpr
tupleExprP = do
  reservedOp "tuple"
  attrAssocs <- braces (sepBy tupleAtomExprP comma)
  let attrs = A.attributesFromList $ map fst attrAssocs
  pure (TupleExpr attrs (map snd attrAssocs))

tupleAtomExprP :: Parser (AttributeName, AtomExpr)
tupleAtomExprP = do
  attributeName <- identifier
  atomExpr <- atomExprP
  return $ (T.pack attributeName, atomExpr)
  
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
  tupleExpr <- braces extendTupleExpressionP
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

multiTupleExpressionP :: Parser [ExtendTupleExpr]
multiTupleExpressionP = sepBy extendTupleExpressionP comma

extendTupleExpressionP :: Parser ExtendTupleExpr
extendTupleExpressionP = attributeExtendTupleExpressionP

attributeExtendTupleExpressionP :: Parser ExtendTupleExpr
attributeExtendTupleExpressionP = do
  newAttr <- identifier
  reservedOp ":="
  atom <- atomExprP
  return $ AttributeExtendTupleExpr (T.pack newAttr) atom

atomExprP :: Parser AtomExpr
atomExprP = try functionAtomExprP <|>
  attributeAtomExprP <|>
  nakedAtomExprP <|>
  relationalAtomExprP <|>
  constructedAtomP

attributeAtomExprP :: Parser AtomExpr
attributeAtomExprP = do
  _ <- string "@"
  attrName <- identifier
  return $ AttributeAtomExpr (T.pack attrName)

nakedAtomExprP :: Parser AtomExpr
nakedAtomExprP = NakedAtomExpr <$> atomP

-- | Uses square brackets for TutorialD support.
constructedAtomP :: Parser AtomExpr
constructedAtomP = do
  reserved "["
  dConsName <- identifier
  dConsArgs <- sepBy atomExprP spaces
  reserved "]"
  pure $ ConstructedAtomExpr (T.pack dConsName) dConsArgs

atomP :: Parser Atom
atomP = dateTimeAtomP <|> 
        intervalDateTimeAtomP <|>
        dateAtomP <|> 
        maybeTextAtomP <|> 
        maybeIntAtomP <|>
        byteStringAtomP <|>
        stringAtomP <|> 
        doubleAtomP <|> 
        intAtomP <|> 
        boolAtomP <|> 
        relationAtomP
        
functionAtomExprP :: Parser AtomExpr
functionAtomExprP = do
  funcName <- identifier
  argList <- parens (sepBy atomExprP comma)
  return $ FunctionAtomExpr (T.pack funcName) argList

relationalAtomExprP :: Parser AtomExpr
relationalAtomExprP = RelationAtomExpr <$> relExprP

stringAtomP :: Parser Atom
stringAtomP = liftA (Atom . T.pack) quotedString

--until polymorphic type constructors and algebraic data types are properly supported, such constructs must be unfortunately hard-coded
maybeTextAtomP :: Parser Atom
maybeTextAtomP = do
  maybeText <- try $ ((Just . T.pack <$> (reserved "Just" *> quotedString)) <|> 
                      (reserved "Nothing" *> return Nothing)) <* reserved "::maybe char"   
  return $ Atom maybeText

--refactor to use polymorphic runtime constructors
maybeIntAtomP :: Parser Atom  
maybeIntAtomP = do
  maybeInt <- try $ ((Just . fromIntegral <$> (reserved "Just" *> integer)) <|>
                     (reserved "Nothing" *> return Nothing)) <* reserved "::maybe int"
  return $ Atom (maybeInt :: Maybe Int)

--refactor to use constructors
dateAtomP :: Parser Atom    
dateAtomP = do
  dateString' <- try $ do
    dateString <- quotedString
    reserved "::date"
    return dateString
  case parseTimeM False defaultTimeLocale "%Y-%m-%d" dateString' of
    Just todaytime -> return $ Atom (todaytime :: Day)
    Nothing -> fail "Failed to parse date"

doubleAtomP :: Parser Atom    
doubleAtomP = Atom <$> (try float)

intAtomP :: Parser Atom
intAtomP = do
  i <- integer
  return $ Atom ((fromIntegral i) :: Int)

boolAtomP :: Parser Atom
boolAtomP = do
  val <- char 't' <|> char 'f'
  return $ Atom (val == 't')
  
relationAtomExprP :: Parser AtomExpr
relationAtomExprP = RelationAtomExpr <$> relationExprP

-- deprecated for removal- use constructors
byteStringAtomP :: Parser Atom
byteStringAtomP = do
  byteString' <- try $ do
    byteString <- quotedString
    reserved "::bytestring"
    return byteString
  case B64.decode $ TE.encodeUtf8 (T.pack byteString') of
    Left err -> fail err
    Right bsVal -> return $ Atom bsVal

{-
--relation constructor -- no choice but to propagate relation-construction errors as parse errors. Perhaps this could be improved in the future
relationP :: Parser Relation
relationP = do
  reserved "relation"
  attrs <- (try makeAttributesP <|> return A.emptyAttributes)
  if not (A.null attrs) then do
    case mkRelation attrs emptyTupleSet of 
      Left err -> fail (show err)
      Right rel -> return rel
    else do -- empty attributes
    tuples <- try (braces (sepBy tupleP comma)) <|> pure []
    let tupleAttrs = if null tuples then A.emptyAttributes else tupleAttributes (head tuples)
    case mkRelationFromTuples tupleAttrs tuples of
      Left err -> fail (show err)
      Right rel -> return rel
-}  