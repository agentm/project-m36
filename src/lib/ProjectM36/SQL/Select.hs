{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies, DeriveTraversable, GeneralizedNewtypeDeriving, DerivingVia, DeriveAnyClass, DeriveGeneric, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
module ProjectM36.SQL.Select where
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Functor.Foldable.TH
import Codec.Winery
import GHC.Generics
import Control.DeepSeq
import Data.Hashable

data Query = QuerySelect Select |
             QueryValues [[ScalarExpr]] |
             QueryTable TableName |
             QueryOp QueryOperator Query Query
             deriving (Show, Eq, Generic, NFData)
             deriving Serialise via WineryVariant Query

data QueryOperator = UnionQueryOperator | IntersectQueryOperator | ExceptQueryOperator
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant QueryOperator

data Select = Select { distinctness :: Maybe Distinctness,
                       projectionClause :: [SelectItem],
                       tableExpr :: Maybe TableExpr,
                       withClause :: Maybe WithClause
                     }
              deriving (Show, Eq, Generic, NFData)
              deriving Serialise via WineryRecord Select

instance Hashable Select

emptySelect :: Select
emptySelect = Select { distinctness = Nothing,
                       projectionClause = [],
                       tableExpr = Nothing,
                       withClause = Nothing
                     }

type SelectItem = (ProjectionScalarExpr, Maybe ColumnAlias)

data WithClause = WithClause { isRecursive :: Bool,
                               withExprs :: NE.NonEmpty WithExpr }
                  deriving (Show, Eq, Generic, NFData)
                  deriving Serialise via WineryRecord WithClause
                  deriving Hashable

data WithExpr = WithExpr WithExprAlias Select
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant WithExpr
  deriving Hashable

newtype WithExprAlias = WithExprAlias Text
  deriving (Show, Eq, Generic)
  deriving Serialise via WineryVariant WithExprAlias
  deriving newtype NFData
  deriving anyclass Hashable

data InFlag = In | NotIn
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant InFlag
  deriving Hashable

data ComparisonOperator = OpLT | OpGT | OpGTE | OpEQ | OpNE | OpLTE
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant ComparisonOperator

instance Hashable ComparisonOperator

data QuantifiedComparisonPredicate = QCAny | QCSome | QCAll
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant QuantifiedComparisonPredicate

instance Hashable QuantifiedComparisonPredicate

data TableRef = SimpleTableRef TableName
              | InnerJoinTableRef TableRef JoinCondition
              | RightOuterJoinTableRef TableRef JoinCondition
              | LeftOuterJoinTableRef TableRef JoinCondition
              | FullOuterJoinTableRef TableRef JoinCondition
              | CrossJoinTableRef TableRef
              | NaturalJoinTableRef TableRef
              | AliasedTableRef TableRef TableAlias
              | QueryTableRef Select
              deriving (Show, Eq, Generic, NFData)
              deriving Serialise via WineryVariant TableRef
              deriving Hashable

-- distinguish between projection attributes which may include an asterisk and scalar expressions (such as in a where clause) where an asterisk is invalid
type ProjectionScalarExpr = ScalarExprBase ColumnProjectionName

deriving via WineryVariant ProjectionScalarExpr instance Serialise ProjectionScalarExpr

type ScalarExpr = ScalarExprBase ColumnName

deriving via WineryVariant ScalarExpr instance Serialise ScalarExpr

data ScalarExprBase n =
  IntegerLiteral Integer
  | DoubleLiteral Double
  | StringLiteral Text
  | BooleanLiteral Bool
  | NullLiteral
    -- | Interval
  | Identifier n
  | BinaryOperator (ScalarExprBase n) OperatorName (ScalarExprBase n)
  | PrefixOperator OperatorName (ScalarExprBase n)
  | PostfixOperator (ScalarExprBase n) OperatorName
  | BetweenOperator (ScalarExprBase n) (ScalarExprBase n) (ScalarExprBase n)
  | FunctionApplication FuncName [ScalarExprBase n]
  | CaseExpr { caseWhens :: [(ScalarExprBase n,ScalarExprBase n)],
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
  deriving (Show, Eq, Generic, NFData)

instance (Hashable n, Eq n) => Hashable (ScalarExprBase n)

data BoolOp = AndOp | OrOp
  deriving (Eq, Show, Generic, NFData)
  deriving Serialise via WineryVariant BoolOp
  deriving Hashable

data InPredicateValue = InList [ScalarExpr] | InQueryExpr Select | InScalarExpr ScalarExpr
  deriving (Eq, Show, Generic, NFData)
  deriving Serialise via WineryVariant InPredicateValue
  deriving Hashable

data GroupByExpr = GroupByExpr ProjectionScalarExpr
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant GroupByExpr
  deriving Hashable

data HavingExpr = HavingExpr ProjectionScalarExpr
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant HavingExpr
  deriving Hashable

data SortExpr = SortExpr ScalarExpr (Maybe Direction) (Maybe NullsOrder)
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant SortExpr  
  deriving Hashable

data Direction = Ascending | Descending
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant Direction  
  deriving Hashable

data NullsOrder = NullsFirst | NullsLast
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant NullsOrder  
  deriving Hashable

data JoinType = InnerJoin | RightOuterJoin | LeftOuterJoin | FullOuterJoin | CrossJoin | NaturalJoin
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant JoinType  

data JoinCondition = JoinOn JoinOnCondition | JoinUsing [UnqualifiedColumnName]
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant JoinCondition
  deriving Hashable

newtype JoinOnCondition = JoinOnCondition ScalarExpr
  deriving (Show, Eq, Generic)
  deriving Serialise via WineryVariant JoinOnCondition
  deriving newtype NFData
  deriving newtype Hashable

data ColumnProjectionName = ColumnProjectionName [ProjectionName] --dot-delimited reference
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant ColumnProjectionName

instance Hashable ColumnProjectionName

data ProjectionName = ProjectionName Text | Asterisk
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant ProjectionName
  deriving Hashable

data ColumnName = ColumnName [Text]
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant ColumnName
  deriving Hashable

data UnqualifiedColumnName = UnqualifiedColumnName Text
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant UnqualifiedColumnName
  deriving Hashable

data TableName = TableName [Text]
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant TableName
  deriving Hashable

data OperatorName = OperatorName [Text]
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant OperatorName

instance Hashable OperatorName

newtype ColumnAlias = ColumnAlias { unColumnAlias :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving Serialise via WineryVariant ColumnAlias
  deriving newtype NFData
  deriving newtype Hashable

newtype TableAlias = TableAlias { unTableAlias :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving Serialise via WineryVariant TableAlias
  deriving newtype (Monoid, Semigroup, NFData)
  deriving newtype Hashable

newtype FuncName = FuncName [Text]
  deriving (Show, Eq, Generic, Ord)
  deriving Serialise via WineryVariant FuncName
  deriving newtype NFData

instance Hashable FuncName

data Distinctness = Distinct | All
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant Distinctness

instance Hashable Distinctness

newtype RestrictionExpr = RestrictionExpr ScalarExpr
  deriving (Show, Eq, Generic)
  deriving Serialise via WineryVariant RestrictionExpr
  deriving newtype NFData
  deriving newtype Hashable

data TableExpr =
  TableExpr { fromClause :: [TableRef],
              whereClause :: Maybe RestrictionExpr,
              groupByClause :: [GroupByExpr],
              havingClause :: Maybe HavingExpr,
              orderByClause :: [SortExpr],
              limitClause :: Maybe Integer,
              offsetClause :: Maybe Integer
                           }
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryRecord TableExpr
  deriving Hashable

emptyTableExpr :: TableExpr
emptyTableExpr = TableExpr { fromClause = [],
                             whereClause = Nothing,
                             groupByClause = [],
                             havingClause = Nothing,
                             orderByClause = [],
                             limitClause = Nothing,
                             offsetClause = Nothing }

makeBaseFunctor ''ScalarExprBase

