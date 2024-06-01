{-# LANGUAGE TypeFamilies #-}
module SQL.Interpreter.Select where
import ProjectM36.Interpreter
import ProjectM36.SQL.Select
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr as E
import SQL.Interpreter.Base
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import Data.Functor
import qualified Data.List.NonEmpty as NE


parseSelect :: Text -> Either ParserError Select
parseSelect = parse (selectP <* semi <* eof) "<interactive>"

parseQuery :: Text -> Either ParserError Query
parseQuery = parse (queryP <* semi <* eof) "<interactive>"
  
queryP :: Parser Query
queryP = E.makeExprParser queryTermP queryOpP
  where
    queryTermP = (QuerySelect <$> selectP) <|>
                (QueryValues <$> valuesP) <|>
                (QueryTable <$> tableP)
    queryOpP = [[infixOpP "union" UnionQueryOperator,
                 infixOpP "intersect" IntersectQueryOperator,
                 infixOpP "except" ExceptQueryOperator
                ]]
    infixOpP nam op =
      E.InfixL $ do
        reserved nam
        pure (QueryOp op)

valuesP :: Parser [[ScalarExpr]]
valuesP = do
  reserved "values"
  sepByComma1 tupleP

tupleP :: Parser [ScalarExpr]
tupleP = parens (sepByComma1 scalarExprP)

tableP :: Parser TableName
tableP = do
  reserved "table"
  tableNameP

tableNameP :: Parser TableName
tableNameP = TableName <$> qualifiedNameP'
  
selectP :: Parser Select
selectP = do
  withClause' <- optional withP
  reserved "select"
--  distinctOptions
  projection <- selectItemListP
  tExpr <- optional tableExprP
  pure (Select { distinctness = Nothing,
                 projectionClause = projection,
                 tableExpr = tExpr,
                 withClause = withClause'
               })
  
  
selectItemListP :: Parser [SelectItem]
selectItemListP = sepBy1 selectItemP comma

selectItemP :: Parser SelectItem
selectItemP = (,) <$> scalarExprP <*> optional (reserved "as" *> columnAliasP)


tableExprP :: Parser TableExpr
tableExprP =
  TableExpr <$> fromP <*> optional whereP <*> option [] groupByP <*> optional havingP <*> option [] orderByP <*> limitP <*> offsetP

fromP :: Parser [TableRef]
fromP = reserved "from" *> (concat <$> sepByComma trefs)
  where
    trefs = (:) <$> nonJoinTref <*> many joinP
    nonJoinTref = choice [parens $ QueryTableRef <$> selectP,
                          try (AliasedTableRef <$> simpleRef <*> (reserved "as" *> tableAliasP)),
                          simpleRef]
    simpleRef = SimpleTableRef <$> tableNameP
    joinP = do
      joinType <- joinTypeP
      tref <- nonJoinTref
      case joinType of -- certain join types require join conditions, others not
        InnerJoin -> InnerJoinTableRef tref <$> joinConditionP
        RightOuterJoin -> RightOuterJoinTableRef tref <$> joinConditionP
        LeftOuterJoin -> LeftOuterJoinTableRef tref <$> joinConditionP
        FullOuterJoin -> FullOuterJoinTableRef tref <$> joinConditionP
        CrossJoin -> pure $ CrossJoinTableRef tref
        NaturalJoin -> pure $ NaturalJoinTableRef tref
      
joinConditionP :: Parser JoinCondition
joinConditionP = do
  (JoinOn <$> (reserved "on" *> (JoinOnCondition <$> scalarExprP))) <|>
   JoinUsing <$> (reserved "using" *> parens (sepBy1 unqualifiedColumnNameP comma))

joinTypeP :: Parser JoinType
joinTypeP = choice [reserveds "cross join" $> CrossJoin,
                    reserveds "inner join" $> InnerJoin,
                    (reserveds "left outer join" <|>
                     reserveds "left join") $> LeftOuterJoin,
                    (reserveds "right outer join" <|>
                     reserveds "right join") $> RightOuterJoin,
                     (reserveds "full join" <|>
                      reserveds "full outer join") $> FullOuterJoin,
                     (reserved "inner join" <|> reserveds "join") $> InnerJoin,
                     reserved "natural join" $> NaturalJoin]
  

whereP :: Parser RestrictionExpr
whereP = reserved "where" *> (RestrictionExpr <$> scalarExprP)

groupByP :: Parser [GroupByExpr]
groupByP =
  reserveds "group by" *> sepBy1 (GroupByExpr <$> scalarExprP) comma

havingP :: Parser HavingExpr
havingP = reserved "having" *> (HavingExpr <$> scalarExprP)

orderByP :: Parser [SortExpr]
orderByP =
  reserveds "order by" *> sepByComma1 (SortExpr <$> scalarExprP <*> optional directionP <*> optional nullsOrderP)
  where
    directionP = (reserved "asc" $> Ascending) <|>
                 (reserved "desc" $> Descending)
    nullsOrderP = (reserveds "nulls first" $> NullsFirst) <|>
                  (reserveds "nulls last" $> NullsLast)
  
nameP :: Parser Text
nameP = (quotedIdentifier <|> identifier) <* spaceConsumer

qualifiedNameP' :: Parser [Text]
qualifiedNameP' = sepBy1 nameP (symbol ".")

columnAliasP :: Parser ColumnAlias
columnAliasP = ColumnAlias <$> (quotedIdentifier <|> identifier)

tableAliasP :: Parser TableAlias
tableAliasP = TableAlias <$> (quotedIdentifier <|> identifier)

unqualifiedColumnNameP :: Parser UnqualifiedColumnName
unqualifiedColumnNameP = UnqualifiedColumnName <$> nameP

scalarExprP :: QualifiedNameP a => Parser (ScalarExprBase a)
scalarExprP = E.makeExprParser scalarTermP scalarExprOp

scalarExprOp :: QualifiedNameP a => [[E.Operator Parser (ScalarExprBase a)]]
scalarExprOp =
  [[qComparisonOp],
   [prefixSymbol "+",
    prefixSymbol "-"],
    [binarySymbolL "^"],
    map binarySymbolL ["\"","*","%"],
    map binarySymbolL ["+","-"],
    [binarySymbolR "||",
     prefixSymbol "~",
     binarySymbolR "&",
     binarySymbolR "|"],
    [binarySymbolN "like",
     E.Postfix $ try inSuffixP,
     E.Postfix betweenSuffixP
     --binarySymbolsN ["not", "like"]
     ],
    map binarySymbolN [">=","<=","!=","<>","=", "<",">"],
    [postfixKeywords ["is","null"], postfixKeywords ["is","not","null"]],
{-    [binarySymbolsN ["is", "distinct", "from"],
     binarySymbolsN ["is", "not", "distinct", "from"]],-}
    [binarySymbolL "and"],
    [prefixSymbol "not"],
    [binarySymbolL "or"]
    -- AT TIME ZONE
    ]
 where
   prefixSymbol s = E.Prefix $ PrefixOperator <$> qualifiedOperatorP s
   binarySymbolL s = E.InfixL $ binary s
   binary s = do
     op <- qualifiedOperatorP s
     pure (`BinaryOperator` op)
   binarySymbolR s = E.InfixR $ binary s
   binarySymbolN s = E.InfixN $ binary s
   qComparisonOp = E.Postfix $ try quantifiedComparisonSuffixP
   postfixKeywords kws = E.Postfix $ do
     try $ reserveds' kws
     pure (\a -> PostfixOperator a (OperatorName kws))
                     

qualifiedOperatorP :: Text -> Parser OperatorName
qualifiedOperatorP sym =
  OperatorName <$> segmentsP (splitOn "." sym) <* spaceConsumer
  where
    segmentsP :: [Text] -> Parser [Text]
    segmentsP segments = case segments of
      [] -> error "empty operator"
      [seg] -> do
        final <- qualifiedNameSegment seg
        pure [final]
      (seg:remainder) -> do
        first <- qualifiedNameSegment seg
        _ <- char '.'
        rem' <- segmentsP remainder
        pure (first:rem')
    

betweenSuffixP :: QualifiedNameP a => Parser (ScalarExprBase a -> ScalarExprBase a)
betweenSuffixP = do
  reserved "between"
  arg1 <- scalarExprP
  reserved "and"
  arg2 <- scalarExprP
  pure (\x -> BetweenOperator x arg1 arg2)

inSuffixP :: QualifiedNameP a => Parser (ScalarExprBase a -> ScalarExprBase a)
inSuffixP = do
  matchIn <|> matchNotIn
  where
    matchIn = do
      reserved "in"
      pred' <- inPredicateValue
      pure (\sexpr -> InExpr In sexpr pred')
    matchNotIn = do
      reserved "not" >> reserved "in"
      pred' <- inPredicateValue      
      pure (\sexpr -> InExpr NotIn sexpr pred')
    inPredicateValue = parens ((InQueryExpr <$> selectP) <|>
                                (InList <$> sepBy1 scalarExprP comma)) <|>
                       InScalarExpr <$> scalarExprP

  
quantifiedComparisonSuffixP :: QualifiedNameP a => Parser (ScalarExprBase a -> ScalarExprBase a)
quantifiedComparisonSuffixP = do
  op <- comparisonOperatorP
  quantOp <- (reserved "any" $> QCAny) <|>
             (reserved "some" $> QCSome) <|>
             (reserved "all" $> QCAll)
  subq <- parens selectP
  pure (\sexpr -> QuantifiedComparison { qcExpr = sexpr,
                                         qcOperator = op,
                                         qcPredicate = quantOp,
                                         qcQuery = subq })

comparisonOperatorP :: Parser ComparisonOperator
comparisonOperatorP = choice (map (\(match', op) -> reserved match' $> op) ops)
    where ops =[(">", OpGT),
                 ("<", OpLT),
                 ("=", OpEQ),
                 (">=", OpGTE),
                 ("<=", OpLTE),
                 ("<>", OpNE),
                 ("!=", OpNE)]

simpleLiteralP :: Parser (ScalarExprBase a)
simpleLiteralP = try doubleLiteralP <|> integerLiteralP <|> booleanLiteralP <|> stringLiteralP <|> nullLiteralP

doubleLiteralP :: Parser (ScalarExprBase a)
doubleLiteralP = DoubleLiteral <$> double

booleanLiteralP :: Parser (ScalarExprBase a)
booleanLiteralP = BooleanLiteral <$> ((reserved "true" $> True) <|> (reserved "false" $> False))

integerLiteralP :: Parser (ScalarExprBase a)
integerLiteralP = IntegerLiteral <$> integer

stringLiteralP :: Parser (ScalarExprBase a)
stringLiteralP = StringLiteral <$> stringP
  where
    stringP = do
      void $ char '\''
      stringEndP
    stringEndP = do
      capture <- T.pack <$> manyTill printChar (char '\'')
      choice [char '\'' *> (do
                               rest <- stringEndP
                               pure $ T.concat [capture, "'",rest]), --quoted quote
              pure capture
             ] <* spaceConsumer

nullLiteralP :: Parser (ScalarExprBase a)
nullLiteralP = 
  reserved "NULL" $> NullLiteral
  
scalarTermP :: QualifiedNameP a => Parser (ScalarExprBase a)
scalarTermP = choice [
  existsP,  
  simpleLiteralP,
    --,subQueryExpr
    caseExprP,
    --,cast
--    subquery,
--    pseudoArgFunc, -- includes NOW, NOW(), CURRENT_USER, TRIM(...), etc.
    scalarFunctionP,
    Identifier <$> qualifiedNameP
    ]
  <?> "scalar expression"

caseExprP :: QualifiedNameP a => Parser (ScalarExprBase a)
caseExprP = do
  let whenThenClause = do
        reserved "when"
        cond <- scalarExprP
        reserved "then"
        result <- scalarExprP
        pure (cond, result)
      elseClause = do
        reserved "else"
        scalarExprP
  reserved "case"
  conditionals <- some whenThenClause
  mElse <- optional elseClause
  reserved "end"
  pure (CaseExpr { caseWhens = conditionals,
                   caseElse = mElse })
       
scalarFunctionP :: QualifiedNameP a => Parser (ScalarExprBase a)
scalarFunctionP =
  try $
  FunctionApplication <$> functionNameP <*> parens (sepByComma scalarExprP)

existsP :: Parser (ScalarExprBase a)
existsP = do
  reserved "exists"
  ExistsExpr <$> parens selectP

-- used to distinguish between sections which may include an asterisk and those which cannot
class QualifiedNameP a where
  qualifiedNameP :: Parser a

-- | col, table.col, table.*, *
instance QualifiedNameP ColumnProjectionName where
  qualifiedNameP = columnProjectionNameP

instance QualifiedNameP ColumnName where
  qualifiedNameP = columnNameP

columnNameP :: Parser ColumnName
columnNameP = 
  ColumnName <$> sepBy1 nameP (char '.') <* spaceConsumer    

columnProjectionNameP :: Parser ColumnProjectionName
columnProjectionNameP =
  ColumnProjectionName <$> sepBy1 ((ProjectionName <$> nameP) <|> (char '*' $> Asterisk)) (char '.') <* spaceConsumer
  
functionNameP :: Parser FuncName
functionNameP = do
  FuncName <$> sepBy1 nameP (char '.') <* spaceConsumer

withExprAliasP :: Parser WithExprAlias
withExprAliasP = WithExprAlias <$> nameP

limitP :: Parser (Maybe Integer)
limitP = optional (reserved "limit" *> integer)

offsetP :: Parser (Maybe Integer)
offsetP = optional (reserved "offset" *> integer)

withP :: Parser WithClause
withP = do
  reserved "with"
  recursive <- try (reserved "recursive" $> True) <|> pure False  
  wExprs <- sepByComma1 $ do
    wName <- withExprAliasP
    reserved "as"
    wSelect <- parens selectP
    pure (WithExpr wName wSelect)
  pure (WithClause recursive (NE.fromList wExprs))
      


