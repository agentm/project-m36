module TutorialD.Interpreter.RelationalExpr where
import Text.Megaparsec
import Text.Megaparsec.Expr
import ProjectM36.Base
import Text.Megaparsec.Text
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.Types
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sort)
import Control.Applicative (liftA)
import ProjectM36.MiscUtils

class RelationalMarkerExpr a where
  parseMarkerP :: Parser a

instance RelationalMarkerExpr () where
  parseMarkerP = pure ()

--used in projection
attributeListP :: Parser AttributeNames
attributeListP = do
  but <- try (string "all but " <* spaceConsumer) <|> string ""
  let constructor = if but == "" then AttributeNames else InvertedAttributeNames
  attrs <- sepBy identifier comma
  pure $ constructor (S.fromList attrs)

makeRelationP :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
makeRelationP = do
  reserved "relation"
  attrExprs <- try (liftA Just makeAttributeExprsP) <|> pure Nothing
  tupleExprs <- braces (sepBy tupleExprP comma) <|> pure []
  pure $ MakeRelationFromExprs attrExprs tupleExprs

--used in relation creation
makeAttributeExprsP :: RelationalMarkerExpr a => Parser [AttributeExprBase a]
makeAttributeExprsP = braces (sepBy attributeAndTypeNameP comma)

attributeAndTypeNameP :: RelationalMarkerExpr a => Parser (AttributeExprBase a)
attributeAndTypeNameP = AttributeAndTypeNameExpr <$> identifier <*> typeConstructorP <*> parseMarkerP
  
--abstract data type parser- in this context, the type constructor must not include any type arguments
--Either Text Int
adTypeConstructorP :: Parser TypeConstructor
adTypeConstructorP = do
  tConsName <- capitalizedIdentifier
  tConsArgs <- many typeConstructorArgP
  pure $ ADTypeConstructor tConsName tConsArgs

tupleExprP :: RelationalMarkerExpr a => Parser (TupleExprBase a)
tupleExprP = do
  reservedOp "tuple"
  attrAssocs <- braces (sepBy tupleAtomExprP comma)
  --detect duplicate attribute names
  let dupAttrNames = dupes (sort (map fst attrAssocs))
  if length dupAttrNames /= 0 then                    
    fail ("Attribute names duplicated: " ++ show dupAttrNames)
    else
    pure (TupleExpr (M.fromList attrAssocs))

tupleAtomExprP :: RelationalMarkerExpr a => Parser (AttributeName, AtomExprBase a)
tupleAtomExprP = do
  attributeName <- identifier
  atomExpr <- atomExprP
  pure $ (attributeName, atomExpr)
  
projectP :: RelationalMarkerExpr a => Parser (RelationalExprBase a  -> RelationalExprBase a)
projectP = do
  attrs <- braces attributeListP
  pure $ Project attrs

renameClauseP :: Parser (T.Text, T.Text)
renameClauseP = do
  oldAttr <- identifier
  reservedOp "as"
  newAttr <- identifier
  pure $ (oldAttr, newAttr)

renameP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a)
renameP = do
  reservedOp "rename"
  renameList <- braces (sepBy renameClauseP comma)
  case renameList of
    [] -> pure (Restrict TruePredicate) --no-op when rename list is empty
    renames -> do
      pure $ \expr -> foldl (\acc (oldAttr, newAttr) -> Rename oldAttr newAttr acc) expr renames

whereClauseP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a)
whereClauseP = reservedOp "where" *> (Restrict <$> restrictionPredicateP)

groupClauseP :: Parser (AttributeNames, T.Text)
groupClauseP = do
  attrs <- braces attributeListP
  reservedOp "as"
  newAttrName <- identifier
  return $ (attrs, newAttrName)

groupP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a)
groupP = do
  reservedOp "group"
  (groupAttrList, groupAttrName) <- parens groupClauseP
  pure $ Group groupAttrList groupAttrName

--in "Time and Relational Theory" (2014), Date's Tutorial D grammar for ungroup takes one attribute, while in previous books, it take multiple arguments. Let us assume that nested ungroups are the same as multiple attributes.
ungroupP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a)
ungroupP = do
  reservedOp "ungroup"
  rvaAttrName <- identifier
  pure $ Ungroup rvaAttrName

extendP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a)
extendP = do
  reservedOp ":"
  tupleExpr <- braces extendTupleExpressionP
  return $ Extend tupleExpr

relOperators :: RelationalMarkerExpr a => [[Operator Parser (RelationalExprBase a)]]
relOperators = [
  [Postfix projectP],
  [Postfix renameP],
  [Postfix whereClauseP],
  [Postfix groupP],
  [Postfix ungroupP],
  [InfixL (reservedOp "join" >> return Join)],
  [InfixL (reservedOp "union" >> return Union)],
  [InfixL (reservedOp "minus" >> return Difference)],
  [InfixN (reservedOp "=" >> return Equals)],
  [Postfix extendP]
  ]

relExprP :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
relExprP = makeExprParser relTerm relOperators

relVarP :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
relVarP = RelationVariable <$> identifier <*> parseMarkerP

relTerm :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
relTerm = parens relExprP
          <|> makeRelationP
          <|> relVarP

restrictionPredicateP :: RelationalMarkerExpr a => Parser (RestrictionPredicateExprBase a)
restrictionPredicateP = makeExprParser predicateTerm predicateOperators
  where
    predicateOperators = [
      [Prefix (reservedOp "not" >> return NotPredicate)],
      [InfixL (reservedOp "and" >> return AndPredicate)],
      [InfixL (reservedOp "or" >> return OrPredicate)]
      ]
    predicateTerm = parens restrictionPredicateP
                    <|> try restrictionAtomExprP
                    <|> try restrictionAttributeEqualityP
                    <|> try relationalBooleanExprP


relationalBooleanExprP :: RelationalMarkerExpr a => Parser (RestrictionPredicateExprBase a)
relationalBooleanExprP = do
  relexpr <- relExprP
  return $ RelationalExprPredicate relexpr
  
restrictionAttributeEqualityP :: RelationalMarkerExpr a => Parser (RestrictionPredicateExprBase a)
restrictionAttributeEqualityP = do
  attributeName <- identifier
  reservedOp "="
  atomexpr <- atomExprP
  return $ AttributeEqualityPredicate attributeName atomexpr

restrictionAtomExprP :: RelationalMarkerExpr a=> Parser (RestrictionPredicateExprBase a) --atoms which are of type "boolean"
restrictionAtomExprP = do
  _ <- char '^' -- not ideal, but allows me to continue to use a context-free grammar
  AtomExprPredicate <$> atomExprP

multiTupleExpressionP :: RelationalMarkerExpr a => Parser [ExtendTupleExprBase a]
multiTupleExpressionP = sepBy extendTupleExpressionP comma

extendTupleExpressionP :: RelationalMarkerExpr a => Parser (ExtendTupleExprBase a)
extendTupleExpressionP = attributeExtendTupleExpressionP

attributeExtendTupleExpressionP :: RelationalMarkerExpr a => Parser (ExtendTupleExprBase a)
attributeExtendTupleExpressionP = do
  newAttr <- identifier
  reservedOp ":="
  atom <- atomExprP
  return $ AttributeExtendTupleExpr newAttr atom

atomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
atomExprP = consumeAtomExprP True

consumeAtomExprP :: RelationalMarkerExpr a => Bool -> Parser (AtomExprBase a)
consumeAtomExprP consume = try functionAtomExprP <|>
            try (parens (constructedAtomExprP True)) <|>
            constructedAtomExprP consume <|>
            attributeAtomExprP <|>
            nakedAtomExprP <|>
            relationalAtomExprP

attributeAtomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
attributeAtomExprP = do
  _ <- string "@"
  attrName <- identifier
  return $ AttributeAtomExpr attrName

nakedAtomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
nakedAtomExprP = NakedAtomExpr <$> atomP

constructedAtomExprP :: RelationalMarkerExpr a => Bool -> Parser (AtomExprBase a)
constructedAtomExprP consume = do
  dConsName <- capitalizedIdentifier
  dConsArgs <- if consume then sepBy (consumeAtomExprP False) spaceConsumer else pure []
  marker <- parseMarkerP
  pure $ ConstructedAtomExpr dConsName dConsArgs marker
  
-- used only for primitive type parsing ?
atomP :: Parser Atom
atomP = stringAtomP <|> 
        doubleAtomP <|> 
        intAtomP <|> 
        boolAtomP
        
functionAtomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
functionAtomExprP = do
  funcName <- identifier
  argList <- parens (sepBy atomExprP comma)
  marker <- parseMarkerP
  return $ FunctionAtomExpr funcName argList marker

relationalAtomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
relationalAtomExprP = RelationAtomExpr <$> relExprP

stringAtomP :: Parser Atom
stringAtomP = liftA TextAtom quotedString

doubleAtomP :: Parser Atom    
doubleAtomP = DoubleAtom <$> (try float)

intAtomP :: Parser Atom
intAtomP = do
  i <- integer
  return $ IntAtom (fromIntegral i)

boolAtomP :: Parser Atom
boolAtomP = do
  val <- char 't' <|> char 'f'
  return $ BoolAtom (val == 't')
  
relationAtomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
relationAtomExprP = RelationAtomExpr <$> makeRelationP
