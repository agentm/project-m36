module TutorialD.Interpreter.RelationalExpr where
import Text.Parsec
import Text.Parsec.Expr
import ProjectM36.Base
import Text.Parsec.String
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.Types
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Functor.Identity (Identity)
import Control.Applicative (liftA)

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
  attrExprs <- (try makeAttributeExprsP <|> pure [])
  if not (null attrExprs) then do
      pure $ MakeEmptyRelation attrExprs
    else do -- empty attributes, assume tuple expressions
      tupleExprs <- try (braces (sepBy tupleExprP comma)) <|> pure []
      pure $ MakeRelationFromTupleExprs tupleExprs 

--used in relation creation
makeAttributeExprsP :: Parser [AttributeExpr]
makeAttributeExprsP = braces (sepBy attributeAndTypeNameP comma)

attributeAndTypeNameP :: Parser AttributeExpr
attributeAndTypeNameP = do
  attrName <- identifier
  --convert type name into type
  typeCons <- typeConstructorP
  return $ AttributeAndTypeNameExpr (T.pack attrName) typeCons
  
--abstract data type parser- in this context, the type constructor must not include any type arguments
--Either Text Int
adTypeConstructorP :: Parser TypeConstructor
adTypeConstructorP = do
  tConsName <- capitalizedIdentifier
  tConsArgs <- many typeConstructorArgP
  pure $ ADTypeConstructor (T.pack tConsName) tConsArgs

tupleExprP :: Parser TupleExpr
tupleExprP = do
  reservedOp "tuple"
  attrAssocs <- braces (sepBy tupleAtomExprP comma)
  pure (TupleExpr (M.fromList attrAssocs))

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
            try constructedAtomExprP <|>
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

-- | Uses square brackets for TutorialD support.
constructedAtomExprP :: Parser AtomExpr
constructedAtomExprP = do
  dConsName <- identifier  
  --_ <- char '['
  dConsArgs <- sepBy atomExprP spaces
  --_ <- char ']'
  pure $ ConstructedAtomExpr (T.pack dConsName) dConsArgs

-- used only for primitive type parsing ?
atomP :: Parser Atom
atomP = stringAtomP <|> 
        doubleAtomP <|> 
        intAtomP <|> 
        boolAtomP
        
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
relationAtomExprP = RelationAtomExpr <$> makeRelationP
