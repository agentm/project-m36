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

--used in projection
attributeListP :: Parser AttributeNames
attributeListP = do
  but <- try (string "all but " <* spaceConsumer) <|> string ""
  let constructor = if but == "" then AttributeNames else InvertedAttributeNames
  attrs <- sepBy identifier comma
  pure $ constructor (S.fromList attrs)

makeRelationP :: Parser RelationalExpr
makeRelationP = do
  reserved "relation"
  attrExprs <- try (liftA Just makeAttributeExprsP) <|> pure Nothing
  tupleExprs <- braces (sepBy tupleExprP comma) <|> pure []
  pure $ MakeRelationFromExprs attrExprs tupleExprs 

--used in relation creation
makeAttributeExprsP :: Parser [AttributeExpr]
makeAttributeExprsP = braces (sepBy attributeAndTypeNameP comma)

attributeAndTypeNameP :: Parser AttributeExpr
attributeAndTypeNameP = do
  attrName <- identifier
  --convert type name into type
  typeCons <- typeConstructorP
  return $ AttributeAndTypeNameExpr attrName typeCons
  
--abstract data type parser- in this context, the type constructor must not include any type arguments
--Either Text Int
adTypeConstructorP :: Parser TypeConstructor
adTypeConstructorP = do
  tConsName <- capitalizedIdentifier
  tConsArgs <- many typeConstructorArgP
  pure $ ADTypeConstructor tConsName tConsArgs

tupleExprP :: Parser TupleExpr
tupleExprP = do
  reservedOp "tuple"
  attrAssocs <- braces (sepBy tupleAtomExprP comma)
  --detect duplicate attribute names
  let dupAttrNames = dupes (sort (map fst attrAssocs))
  if length dupAttrNames /= 0 then                    
    fail ("Attribute names duplicated: " ++ show dupAttrNames)
    else
    pure (TupleExpr (M.fromList attrAssocs))

tupleAtomExprP :: Parser (AttributeName, AtomExpr)
tupleAtomExprP = do
  attributeName <- identifier
  atomExpr <- atomExprP
  pure $ (attributeName, atomExpr)
  
projectP :: Parser (RelationalExpr -> RelationalExpr)
projectP = do
  attrs <- braces attributeListP
  pure $ Project attrs

renameClauseP :: Parser (T.Text, T.Text)
renameClauseP = do
  oldAttr <- identifier
  reservedOp "as"
  newAttr <- identifier
  pure $ (oldAttr, newAttr)

renameP :: Parser (RelationalExpr -> RelationalExpr)
renameP = do
  reservedOp "rename"
  (oldAttr, newAttr) <- braces renameClauseP
  return $ Rename oldAttr newAttr

whereClauseP :: Parser (RelationalExpr -> RelationalExpr)
whereClauseP = do
  reservedOp "where"
  boolExpr <- restrictionPredicateP
  return $ Restrict boolExpr

groupClauseP :: Parser (AttributeNames, T.Text)
groupClauseP = do
  attrs <- braces attributeListP
  reservedOp "as"
  newAttrName <- identifier
  return $ (attrs, newAttrName)

groupP :: Parser (RelationalExpr -> RelationalExpr)
groupP = do
  reservedOp "group"
  (groupAttrList, groupAttrName) <- parens groupClauseP
  pure $ Group groupAttrList groupAttrName

--in "Time and Relational Theory" (2014), Date's Tutorial D grammar for ungroup takes one attribute, while in previous books, it take multiple arguments. Let us assume that nested ungroups are the same as multiple attributes.
ungroupP :: Parser (RelationalExpr -> RelationalExpr)
ungroupP = do
  reservedOp "ungroup"
  rvaAttrName <- identifier
  pure $ Ungroup rvaAttrName

extendP :: Parser (RelationalExpr -> RelationalExpr)
extendP = do
  reservedOp ":"
  tupleExpr <- braces extendTupleExpressionP
  return $ Extend tupleExpr

relOperators :: [[Operator Parser RelationalExpr]]
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

relExprP :: Parser RelationalExpr
relExprP = makeExprParser relTerm relOperators

relVarP :: Parser RelationalExpr
relVarP = liftA RelationVariable identifier

relTerm :: Parser RelationalExpr
relTerm = parens relExprP
          <|> makeRelationP
          <|> relVarP

restrictionPredicateP :: Parser RestrictionPredicateExpr
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


relationalBooleanExprP :: Parser RestrictionPredicateExpr
relationalBooleanExprP = do
  relexpr <- relExprP
  return $ RelationalExprPredicate relexpr
  
restrictionAttributeEqualityP :: Parser RestrictionPredicateExpr
restrictionAttributeEqualityP = do
  attributeName <- identifier
  reservedOp "="
  atomexpr <- atomExprP
  return $ AttributeEqualityPredicate attributeName atomexpr

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
  return $ AttributeExtendTupleExpr newAttr atom

atomExprP :: Parser AtomExpr
atomExprP = consumeAtomExprP True

consumeAtomExprP :: Bool -> Parser AtomExpr
consumeAtomExprP consume = try functionAtomExprP <|>
            try (parens (constructedAtomExprP True)) <|>
            constructedAtomExprP consume <|>
            attributeAtomExprP <|>
            nakedAtomExprP <|>
            relationalAtomExprP

attributeAtomExprP :: Parser AtomExpr
attributeAtomExprP = do
  _ <- string "@"
  attrName <- identifier
  return $ AttributeAtomExpr attrName

nakedAtomExprP :: Parser AtomExpr
nakedAtomExprP = NakedAtomExpr <$> atomP

constructedAtomExprP :: Bool -> Parser AtomExpr
constructedAtomExprP consume = do
  dConsName <- capitalizedIdentifier
  dConsArgs <- if consume then sepBy (consumeAtomExprP False) spaceConsumer else pure []
  pure $ ConstructedAtomExpr dConsName dConsArgs
  
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
  return $ FunctionAtomExpr funcName argList

relationalAtomExprP :: Parser AtomExpr
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
  
relationAtomExprP :: Parser AtomExpr
relationAtomExprP = RelationAtomExpr <$> makeRelationP
