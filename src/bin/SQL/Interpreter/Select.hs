{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module SQL.Interpreter.Select where
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr as E
import SQL.Interpreter.Base
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import Data.Functor
import Data.Functor.Foldable.TH
import qualified Data.List.NonEmpty as NE

-- we use an intermediate data structure because it may need to be probed into order to create a proper relational expression
data Select = Select { distinctness :: Maybe Distinctness,
                       projectionClause :: [SelectItem],
                       tableExpr :: Maybe TableExpr,
                       withClause :: Maybe WithClause
                     }
              deriving (Show, Eq)

emptySelect :: Select
emptySelect = Select { distinctness = Nothing,
                       projectionClause = [],
                       tableExpr = Nothing,
                       withClause = Nothing
                     }

data WithClause = WithClause { isRecursive :: Bool,
                               withExprs :: NE.NonEmpty WithExpr }
                  deriving (Show, Eq)

data WithExpr = WithExpr WithExprAlias Select
  deriving (Show, Eq)

newtype WithExprAlias = WithExprAlias Text
  deriving (Show, Eq)

data InFlag = In | NotIn
  deriving (Show, Eq)

data ComparisonOperator = OpLT | OpGT | OpGTE | OpEQ | OpNE | OpLTE
  deriving (Show, Eq)

data QuantifiedComparisonPredicate = QCAny | QCSome | QCAll
  deriving (Show,Eq)

data TableRef = SimpleTableRef TableName
              | InnerJoinTableRef TableRef JoinCondition
              | RightOuterJoinTableRef TableRef JoinCondition
              | LeftOuterJoinTableRef TableRef JoinCondition
              | FullOuterJoinTableRef TableRef JoinCondition
              | CrossJoinTableRef TableRef
              | NaturalJoinTableRef TableRef
              | AliasedTableRef TableRef TableAlias
              | QueryTableRef Select
              deriving (Show, Eq)

-- distinguish between projection attributes which may include an asterisk and scalar expressions (such as in a where clause) where an asterisk is invalid
type ProjectionScalarExpr = ScalarExprBase ColumnProjectionName
type ScalarExpr = ScalarExprBase ColumnName

data ScalarExprBase n =
  IntegerLiteral Integer
  | DoubleLiteral Double
  | StringLiteral Text
  | NullLiteral
    -- | Interval
  | Identifier n
  | BinaryOperator (ScalarExprBase n) OperatorName (ScalarExprBase n)
  | PrefixOperator OperatorName (ScalarExprBase n)
  | PostfixOperator (ScalarExprBase n) ColumnName
  | BetweenOperator (ScalarExprBase n) (ScalarExprBase n) (ScalarExprBase n)
  | FunctionApplication FuncName (ScalarExprBase n)
  | CaseExpr { caseWhens :: [([ScalarExprBase n],ScalarExprBase n)],
               caseElse :: Maybe (ScalarExprBase n) }
  | QuantifiedComparison { qcExpr :: ScalarExprBase n,
                           qcOperator :: ComparisonOperator,
                           qcPredicate :: QuantifiedComparisonPredicate,
                           qcQuery :: Select }
    
  | InExpr InFlag (ScalarExprBase n) InPredicateValue
    -- | ExistsSubQuery Select
    -- | UniqueSubQuery Select
    -- | ScalarSubQuery Select
  | BooleanOperatorExpr (ScalarExprBase n) BoolOp (ScalarExprBase n)
  | ExistsExpr Select
  deriving (Show, Eq)

data BoolOp = AndOp | OrOp
  deriving (Eq, Show)

data InPredicateValue = InList [ScalarExpr] | InQueryExpr Select | InScalarExpr ScalarExpr
  deriving (Eq, Show)

data GroupByExpr = Group ScalarExpr
  deriving (Show, Eq)

data HavingExpr = Having ScalarExpr
  deriving (Show, Eq)

data SortExpr = SortExpr ScalarExpr (Maybe Direction) (Maybe NullsOrder)
  deriving (Show, Eq)

data Direction = Ascending | Descending
  deriving (Show, Eq)

data NullsOrder = NullsFirst | NullsLast
  deriving (Show, Eq)

data JoinType = InnerJoin | RightOuterJoin | LeftOuterJoin | FullOuterJoin | CrossJoin | NaturalJoin
  deriving (Show, Eq)

data JoinCondition = JoinOn JoinOnCondition | JoinUsing [UnqualifiedColumnName]
  deriving (Show, Eq)

newtype JoinOnCondition = JoinOnCondition ScalarExpr
  deriving (Show, Eq)

data ColumnProjectionName = ColumnProjectionName [ProjectionName] --dot-delimited reference
  deriving (Show, Eq, Ord)

data ProjectionName = ProjectionName Text | Asterisk
  deriving (Show, Eq, Ord)

data ColumnName = ColumnName [Text]
  deriving (Show, Eq, Ord)

data UnqualifiedColumnName = UnqualifiedColumnName Text
  deriving (Show, Eq, Ord)

data TableName = TableName [Text]
  deriving (Show, Eq, Ord)

data OperatorName = OperatorName [Text]
  deriving (Show, Eq, Ord)

newtype ColumnAlias = ColumnAlias { unColumnAlias :: Text }
  deriving (Show, Eq, Ord)

newtype TableAlias = TableAlias { unTableAlias :: Text }
  deriving (Show, Eq, Ord, Monoid, Semigroup)

newtype FuncName = FuncName [Text]
  deriving (Show, Eq)

data Distinctness = Distinct | All deriving (Show, Eq)

queryExprP :: Parser Select
queryExprP = tableP <|> selectP

tableP :: Parser Select
tableP = do
  reserved "table"
  tname <- tableNameP
  pure $ emptySelect { tableExpr = Just $ emptyTableExpr { fromClause = [SimpleTableRef tname] } }

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
  
type SelectItem = (ProjectionScalarExpr, Maybe ColumnAlias)
  
selectItemListP :: Parser [SelectItem]
selectItemListP = sepBy1 selectItemP comma

selectItemP :: Parser SelectItem
selectItemP = (,) <$> scalarExprP <*> optional (reserved "as" *> columnAliasP)

newtype RestrictionExpr = RestrictionExpr ScalarExpr
  deriving (Show, Eq)

data TableExpr =
  TableExpr { fromClause :: [TableRef],
              whereClause :: Maybe RestrictionExpr,
              groupByClause :: [GroupByExpr],
              havingClause :: Maybe HavingExpr,
              orderByClause :: [SortExpr],
              limitClause :: Maybe Integer,
              offsetClause :: Maybe Integer
                           }
  deriving (Show, Eq)

emptyTableExpr :: TableExpr
emptyTableExpr = TableExpr { fromClause = [],
                             whereClause = Nothing,
                             groupByClause = [],
                             havingClause = Nothing,
                             orderByClause = [],
                             limitClause = Nothing,
                             offsetClause = Nothing }

tableExprP :: Parser TableExpr
tableExprP =
  TableExpr <$> fromP <*> optional whereP <*> option [] groupByP <*> optional havingP <*> option [] orderByP <*> limitP <*> offsetP

fromP :: Parser [TableRef]
fromP = reserved "from" *> (concat <$> sepByComma trefs)
  where
    trefs = ((:) <$> nonJoinTref <*> many joinP)
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
  reserveds "group by" *> sepBy1 (Group <$> scalarExprP) comma

havingP :: Parser HavingExpr
havingP = reserved "having" *> (Having <$> scalarExprP)

orderByP :: Parser [SortExpr]
orderByP =
  reserveds "order by" *> (sepByComma1 (SortExpr <$> scalarExprP <*> optional directionP <*> optional nullsOrderP))
  where
    directionP = (reserved "asc" $> Ascending) <|>
                 (reserved "desc" $> Descending)
    nullsOrderP = (reserveds "nulls first" $> NullsFirst) <|>
                  (reserveds "nulls last" $> NullsLast)
  
nameP :: Parser Text
nameP = quotedIdentifier <|> identifier

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
    map binarySymbolN ["<",">",">=","<=","!=","<>","="],
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
     pure (\a b -> BinaryOperator a op b)
   binarySymbolR s = E.InfixR $ binary s
   binarySymbolN s = E.InfixN $ binary s
   qComparisonOp = E.Postfix $ try quantifiedComparisonSuffixP

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
simpleLiteralP = try doubleLiteralP <|> integerLiteralP <|> stringLiteralP <|> nullLiteralP

doubleLiteralP :: Parser (ScalarExprBase a)
doubleLiteralP = DoubleLiteral <$> double

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
             ]

nullLiteralP :: Parser (ScalarExprBase a)
nullLiteralP = 
  reserved "NULL" *> pure NullLiteral
  
scalarTermP :: QualifiedNameP a => Parser (ScalarExprBase a)
scalarTermP = choice [
  existsP,  
  simpleLiteralP,
    --,subQueryExpr
--    caseExpr,
    --,cast
--    subquery,
--    pseudoArgFunc, -- includes NOW, NOW(), CURRENT_USER, TRIM(...), etc.
    Identifier <$> qualifiedNameP
    ]
  <?> "scalar expression"


existsP :: Parser (ScalarExprBase a)
existsP = do
  reserved "exists"
  ExistsExpr <$> parens selectP

-- used to distinguish between sections which may include an asterisk and those which cannot
class QualifiedNameP a where
  qualifiedNameP :: Parser a

-- | col, table.col, table.*, *
instance QualifiedNameP ColumnProjectionName where
  qualifiedNameP =
    ColumnProjectionName <$> sepBy1 ((ProjectionName <$> nameP) <|> (char '*' $> Asterisk)) (char '.') <* spaceConsumer

instance QualifiedNameP ColumnName where
  qualifiedNameP = ColumnName <$> sepBy1 nameP (char '.') <* spaceConsumer

withExprAliasP :: Parser WithExprAlias
withExprAliasP = WithExprAlias <$> nameP

limitP :: Parser (Maybe Integer)
limitP = optional (reserved "limit" *> integer)

offsetP :: Parser (Maybe Integer)
offsetP = optional (reserved "offset" *> integer)

withP :: Parser WithClause
withP = do
  reserved "with"
  recursive <- try (reserved "recursive" *> pure True) <|> pure False  
  wExprs <- sepByComma1 $ do
    wName <- withExprAliasP
    reserved "as"
    wSelect <- parens selectP
    pure (WithExpr wName wSelect)
  pure (WithClause recursive (NE.fromList wExprs))
      
makeBaseFunctor ''ScalarExprBase

