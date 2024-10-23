{-# LANGUAGE CPP #-}
module TutorialD.Interpreter.RelationalExpr where
import Text.Megaparsec
#if MIN_VERSION_megaparsec(7,0,0)
import Control.Monad.Combinators.Expr
#else
import Text.Megaparsec.Expr
#endif
import ProjectM36.Base
import ProjectM36.Interpreter
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.Types
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sort)
import ProjectM36.MiscUtils
import Control.Monad (void)

--used in projection
attributeListP :: RelationalMarkerExpr a => Parser (AttributeNamesBase a)
attributeListP =
  (reservedOp "all but" >>
   InvertedAttributeNames . S.fromList <$> sepBy attributeNameP comma) <|>

  (reservedOp "all from" >>
   RelationalExprAttributeNames <$> relExprP) <|>

  (reservedOp "union of" >>
   UnionAttributeNames <$> braces attributeListP <*> braces attributeListP) <|>

  (reservedOp "intersection of" >>
  IntersectAttributeNames <$> braces attributeListP <*> braces attributeListP) <|>
   
  (AttributeNames . S.fromList <$> sepBy attributeNameP comma) 
  

makeRelationP :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
makeRelationP = do
  reserved "relation"
  attrExprs <- try (fmap Just makeAttributeExprsP) <|> pure Nothing
  tupleExprs <- braces (sepBy tupleExprP comma) <|> pure []
  marker <- parseMarkerP
  pure $ MakeRelationFromExprs attrExprs (TupleExprs marker tupleExprs)


--abstract data type parser- in this context, the type constructor must not include any type arguments
--Either Text Int
adTypeConstructorP :: Parser TypeConstructor
adTypeConstructorP = do
  tConsName <- typeConstructorNameP
  tConsArgs <- many typeConstructorP
  pure $ ADTypeConstructor tConsName tConsArgs

tupleExprP :: RelationalMarkerExpr a => Parser (TupleExprBase a)
tupleExprP = do
  reservedOp "tuple"
  attrAssocs <- braces (sepBy tupleAtomExprP comma)
  --detect duplicate attribute names
  let dupAttrNames = dupes (sort (map fst attrAssocs))
  if not (null dupAttrNames) then
    fail ("Attribute names duplicated: " ++ show dupAttrNames)
    else
    pure (TupleExpr (M.fromList attrAssocs))

tupleAtomExprP :: RelationalMarkerExpr a => Parser (AttributeName, AtomExprBase a)
tupleAtomExprP = do
  attributeName <- attributeNameP
  atomExpr <- atomExprP
  pure (attributeName, atomExpr)

projectP :: RelationalMarkerExpr a => Parser (RelationalExprBase a  -> RelationalExprBase a)
projectP = Project <$> braces attributeListP

renameClauseP :: Parser (T.Text, T.Text)
renameClauseP = do
  oldAttr <- attributeNameP
  reservedOp "as"
  newAttr <- attributeNameP
  pure (oldAttr, newAttr)

renameP :: Parser (RelationalExprBase a -> RelationalExprBase a)
renameP = do
  reservedOp "rename"
  renameList <- braces (sepBy renameClauseP comma)
  case renameList of
    [] -> pure id
    renames ->
      pure $ Rename (S.fromList renames)

whereClauseP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a)
whereClauseP = reservedOp "where" *> (Restrict <$> restrictionPredicateP)

groupClauseP :: RelationalMarkerExpr a => Parser (AttributeNamesBase a, T.Text)
groupClauseP = do
  attrs <- braces attributeListP
  reservedOp "as"
  newAttrName <- attributeNameP
  pure (attrs, newAttrName)

groupP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a)
groupP = do
  reservedOp "group"
  (groupAttrList, groupAttrName) <- parens groupClauseP
  pure $ Group groupAttrList groupAttrName

--in "Time and Relational Theory" (2014), Date's Tutorial D grammar for ungroup takes one attribute, while in previous books, it take multiple arguments. Let us assume that nested ungroups are the same as multiple attributes.
ungroupP :: Parser (RelationalExprBase a -> RelationalExprBase a)
ungroupP = do
  reservedOp "ungroup"
  Ungroup <$> attributeNameP

extendP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a)
extendP = do
  reservedOp ":"
  extends <- braces (sepBy extendTupleExpressionP comma)
  case extends of
    [] -> pure (Restrict TruePredicate)
    extends' ->
      pure $ \expr -> foldl (flip Extend) expr extends'

semijoinP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a -> RelationalExprBase a)
semijoinP = do
  reservedOp "semijoin" <|> reservedOp "matching"
  pure (\exprA exprB ->
         Project (RelationalExprAttributeNames exprA) (Join exprA exprB))

antijoinP :: RelationalMarkerExpr a => Parser (RelationalExprBase a -> RelationalExprBase a -> RelationalExprBase a)
antijoinP = do
  reservedOp "not matching" <|> reservedOp "antijoin"
  pure (\exprA exprB ->
         Difference exprA (
           Project (RelationalExprAttributeNames exprA) (Join exprA exprB)))

relOperators :: RelationalMarkerExpr a => [[Operator Parser (RelationalExprBase a)]]
relOperators = [
  [Postfix projectP],
  [Postfix renameP],
  [Postfix whereClauseP],
  [Postfix groupP],
  [Postfix ungroupP],
  [InfixL (reservedOp "join" >> return Join)],
  [InfixL semijoinP],
  [InfixL antijoinP],
  [InfixL (reservedOp "union" >> return Union)],
  [InfixL (reservedOp "minus" >> return Difference)],
  [InfixN (reservedOp "=" >> return Equals),
   InfixN (reservedOp "/=" >> return NotEquals)],
  [Postfix extendP]
  ]

relExprP :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
relExprP = try withMacroExprP <|> makeExprParser relTerm relOperators

relVarP :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
relVarP = RelationVariable <$> relVarNameP <*> parseMarkerP

relTerm :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
relTerm = parens relExprP
          <|> makeRelationP
          <|> relVarP
          <|> relationValuedAttributeP

relationValuedAttributeP :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
relationValuedAttributeP = do
  RelationValuedAttribute <$> (single '@' *> attributeNameP)

restrictionPredicateP :: RelationalMarkerExpr a => Parser (RestrictionPredicateExprBase a)
restrictionPredicateP = makeExprParser predicateTerm predicateOperators
  where
    predicateOperators = [
      [Prefix (reservedOp "not" >> return NotPredicate)],
      [InfixL (reservedOp "and" >> return AndPredicate)],
      [InfixL (reservedOp "or" >> return OrPredicate)]
      ]
    predicateTerm = try (parens restrictionPredicateP)
                    <|> try restrictionAttributeEqualityP
                    <|> try relationalBooleanExprP
                    <|> restrictionAtomExprP                                        


relationalBooleanExprP :: RelationalMarkerExpr a => Parser (RestrictionPredicateExprBase a)
relationalBooleanExprP = RelationalExprPredicate <$> (parens relExprP <|> relTerm)
  --we can't actually detect if the type is relational boolean, so we just pass it to the next phase

restrictionAttributeEqualityP :: RelationalMarkerExpr a => Parser (RestrictionPredicateExprBase a)
restrictionAttributeEqualityP = do
  attributeName <- attributeNameP
  reservedOp "="
  AttributeEqualityPredicate attributeName <$> atomExprP

restrictionAtomExprP :: RelationalMarkerExpr a => Parser (RestrictionPredicateExprBase a) --atoms which are of type "boolean"
restrictionAtomExprP = AtomExprPredicate <$> consumeAtomExprP False

multiTupleExpressionP :: RelationalMarkerExpr a => Parser [ExtendTupleExprBase a]
multiTupleExpressionP = sepBy extendTupleExpressionP comma

extendTupleExpressionP :: RelationalMarkerExpr a => Parser (ExtendTupleExprBase a)
extendTupleExpressionP = attributeExtendTupleExpressionP

attributeExtendTupleExpressionP :: RelationalMarkerExpr a => Parser (ExtendTupleExprBase a)
attributeExtendTupleExpressionP = do
  newAttr <- attributeNameP
  reservedOp ":="
  AttributeExtendTupleExpr newAttr <$> atomExprP

atomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
atomExprP = consumeAtomExprP True

consumeAtomExprP :: RelationalMarkerExpr a => Bool -> Parser (AtomExprBase a)
consumeAtomExprP consume =
  try functionAtomExprP <|>
  ifThenAtomExprP <|>  
  boolAtomExprP <|> -- we do this before the constructed atom parser to consume True and False 
  try (parens (constructedAtomExprP True)) <|>
  constructedAtomExprP consume <|>
  try subrelationAttributeExprP <|>  
  relationalAtomExprP <|>
  attributeAtomExprP <|>
  try nakedAtomExprP

attributeAtomExprP :: Parser (AtomExprBase a)
attributeAtomExprP = do
  _ <- string "@"
  AttributeAtomExpr <$> attributeNameP

nakedAtomExprP :: Parser (AtomExprBase a)
nakedAtomExprP = NakedAtomExpr <$> atomP

constructedAtomExprP :: RelationalMarkerExpr a => Bool -> Parser (AtomExprBase a)
constructedAtomExprP consume = do
  dConsName <- dataConstructorNameP
  dConsArgs <- if consume then sepBy (consumeAtomExprP False) spaceConsumer else pure []
  ConstructedAtomExpr dConsName dConsArgs <$> parseMarkerP

-- used only for primitive type parsing ?
atomP :: Parser Atom
atomP = stringAtomP <|>
        try doubleAtomP <|>
        integerAtomP <|>
        boolAtomP

-- Haskell-like if-then-else expression for TutorialD
ifThenAtomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
ifThenAtomExprP = do
  reserved "if"
  ifE <- atomExprP
  reserved "then"
  thenE <- atomExprP
  reserved "else"
  IfThenAtomExpr ifE thenE <$> atomExprP


-- "@relattr.subrelattr"
subrelationAttributeNameP :: Parser (AttributeName, AttributeName)
subrelationAttributeNameP = do
  void $ single '@'
  relAttr <- uncapitalizedOrQuotedIdentifier
  void $ single '.'
  subrelAttr <- uncapitalizedOrQuotedIdentifier
  spaceConsumer
  pure (relAttr, subrelAttr)

subrelationAttributeExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
subrelationAttributeExprP = do
  void $ single '@'
  relAttr <- uncapitalizedOrQuotedIdentifier
  void $ single '.'
  subrelAttr <- uncapitalizedOrQuotedIdentifier
  spaceConsumer
  pure (SubrelationAttributeAtomExpr relAttr subrelAttr)

functionAtomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
functionAtomExprP =
  FunctionAtomExpr <$> functionNameP <*> parens (sepBy atomExprP comma) <*> parseMarkerP

relationalAtomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
relationalAtomExprP = do
  expr <- relExprP
  case expr of
    RelationValuedAttribute attrName -> pure $ AttributeAtomExpr attrName
    other -> pure $ RelationAtomExpr other

stringAtomP :: Parser Atom
stringAtomP = TextAtom <$> quotedString

doubleAtomP :: Parser Atom
doubleAtomP = DoubleAtom <$> float

integerAtomP :: Parser Atom
integerAtomP = IntegerAtom <$> integer

boolP :: Parser Bool
boolP =
  (chunk "True" >> spaceConsumer >> pure True) <|>
  (chunk "False" >> spaceConsumer >> pure False)
  
boolAtomP :: Parser Atom
boolAtomP = 
  BoolAtom <$> boolP

boolAtomExprP :: Parser (AtomExprBase a)
boolAtomExprP =
  NakedAtomExpr <$> boolAtomP

relationAtomExprP :: RelationalMarkerExpr a => Parser (AtomExprBase a)
relationAtomExprP = RelationAtomExpr <$> makeRelationP

withMacroExprP :: RelationalMarkerExpr a => Parser (RelationalExprBase a)
withMacroExprP = do
  reservedOp "with"
  views <- parens (sepBy1 createMacroP comma)
  With views <$> relExprP

createMacroP :: RelationalMarkerExpr a => Parser (WithNameExprBase a, RelationalExprBase a)
createMacroP = do 
  name <- identifierP
  reservedOp "as"
  expr <- relExprP
  marker <- parseMarkerP
  pure (WithNameExpr name marker, expr)


