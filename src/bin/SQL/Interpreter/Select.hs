module SQL.Interpreter.Select where
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr as E
import SQL.Interpreter.Base
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import Data.Functor

-- we use an intermediate data structure because it may need to be probed into order to create a proper relational expression
data Select = Select { distinctness :: Maybe Distinctness,
                       projectionClause :: [SelectItem],
                       tableExpr :: Maybe TableExpr
                     }
              deriving (Show, Eq)

data InFlag = In | NotIn
  deriving (Show, Eq)

data ComparisonOperator = OpLT | OpGT | OpGTE | OpEQ | OpNE | OpLTE
  deriving (Show, Eq)

data QuantifiedComparisonPredicate = QCAny | QCSome | QCAll
  deriving (Show,Eq)

data TableRef = SimpleTableRef QualifiedName
              | JoinTableRef JoinType TableRef (Maybe JoinCondition)
              | AliasedTableRef TableRef AliasName
              | QueryTableRef Select
              deriving (Show, Eq)

-- distinguish between projection attributes which may include an asterisk and scalar expressions (such as in a where clause) where an asterisk is invalid
type ProjectionScalarExpr = ScalarExprBase QualifiedProjectionName
type ScalarExpr = ScalarExprBase QualifiedName

data ScalarExprBase n = IntegerLiteral Integer
                | DoubleLiteral Double
                | StringLiteral Text
                -- | Interval
                | Identifier n
                | BinaryOperator (ScalarExprBase n) QualifiedName (ScalarExprBase n)
                | PrefixOperator QualifiedName (ScalarExprBase n)
                | PostfixOperator (ScalarExprBase n) QualifiedName
                | BetweenOperator (ScalarExprBase n) (ScalarExprBase n) (ScalarExprBase n)
                | FunctionApplication QualifiedName (ScalarExprBase n)
                | CaseExpr { caseWhens :: [([ScalarExprBase n],ScalarExprBase n)],
                             caseElse :: Maybe (ScalarExprBase n) }
                | QuantifiedComparison { qcExpr :: (ScalarExprBase n),
                                         qcOperator :: ComparisonOperator,
                                         qcPredicate :: QuantifiedComparisonPredicate,
                                         qcQuery :: Select }
                    
                | InExpr InFlag (ScalarExprBase n) InPredicateValue
                -- | ExistsSubQuery Select
                -- | UniqueSubQuery Select
                -- | ScalarSubQuery Select
                deriving (Show, Eq)

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

data JoinCondition = JoinOn ScalarExpr | JoinUsing [QualifiedName]
  deriving (Show, Eq)

data Alias = Alias QualifiedName (Maybe AliasName)
  deriving (Show, Eq)

data QualifiedProjectionName = QualifiedProjectionName [ProjectionName] --dot-delimited reference
  deriving (Show, Eq, Ord)

data ProjectionName = ProjectionName Text | Asterisk
  deriving (Show, Eq, Ord)

data QualifiedName = QualifiedName [Text]
  deriving (Show, Eq)

newtype AliasName = AliasName Text
  deriving (Show, Eq)
                       
data Distinctness = Distinct | All deriving (Show, Eq)
                         
selectP :: Parser Select
selectP = do
  reserved "select"
--  distinctOptions
  projection <- selectItemListP
  tExpr <- optional tableExprP
  pure (Select { distinctness = Nothing,
                 projectionClause = projection,
                 tableExpr = tExpr
               })
  
type SelectItem = (ProjectionScalarExpr, Maybe AliasName)
  
selectItemListP :: Parser [SelectItem]
selectItemListP = sepBy1 selectItemP comma

selectItemP :: Parser SelectItem
selectItemP = (,) <$> scalarExprP <*> optional (reserved "as" *> aliasNameP)

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

tableExprP :: Parser TableExpr
tableExprP =
  TableExpr <$> fromP <*> optional whereP <*> option [] groupByP <*> optional havingP <*> option [] orderByP <*> limitP <*> offsetP

fromP :: Parser [TableRef]
fromP = reserved "from" *> ((:) <$> nonJoinTref <*> sepByComma joinP)
  where
    nonJoinTref = choice [parens $ QueryTableRef <$> selectP,
                          try (AliasedTableRef <$> simpleRef <*> (reserved "as" *> aliasNameP)),
                          simpleRef]
    simpleRef = SimpleTableRef <$> qualifiedNameP              
    joinP = JoinTableRef <$> joinTypeP <*> nonJoinTref <*> optional joinConditionP
      
joinConditionP :: Parser JoinCondition
joinConditionP = do
  (JoinOn <$> (reserved "on" *> scalarExprP)) <|>
   JoinUsing <$> (reserved "using" *> parens (sepBy1 qualifiedNameP comma))

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

aliasNameP :: Parser AliasName
aliasNameP = AliasName <$> (quotedIdentifier <|> identifier)

--qualifiedNameP :: Parser QualifiedName
--qualifiedNameP = 

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

qualifiedOperatorP :: Text -> Parser QualifiedName
qualifiedOperatorP sym =
  QualifiedName <$> segmentsP (splitOn "." sym)
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
simpleLiteralP = try doubleLiteralP <|> integerLiteralP <|> stringLiteralP

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
  
scalarTermP :: QualifiedNameP a => Parser (ScalarExprBase a)
scalarTermP = choice [
  simpleLiteralP,
    --,subQueryExpr
--    caseExpr,
    --,cast
--    subquery,
--    pseudoArgFunc, -- includes NOW, NOW(), CURRENT_USER, TRIM(...), etc.
    Identifier <$> qualifiedNameP]
  <?> "scalar expression"

-- used to distinguish between sections which may include an asterisk and those which cannot
class QualifiedNameP a where
  qualifiedNameP :: Parser a

-- | col, table.col, table.*, *
instance QualifiedNameP QualifiedProjectionName where
  qualifiedNameP =
    QualifiedProjectionName <$> sepBy1 ((ProjectionName <$> nameP) <|> (char '*' $> Asterisk)) (char '.') <* spaceConsumer

instance QualifiedNameP QualifiedName where
  qualifiedNameP = QualifiedName <$> sepBy1 nameP (char '.')

limitP :: Parser (Maybe Integer)
limitP = optional (reserved "limit" *> integer)

offsetP :: Parser (Maybe Integer)
offsetP = optional (reserved "offset" *> integer)
      
