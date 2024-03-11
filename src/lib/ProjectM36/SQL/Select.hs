{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies, DeriveTraversable, GeneralizedNewtypeDeriving, DerivingVia, DeriveAnyClass, DeriveGeneric, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
module ProjectM36.SQL.Select where
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Functor.Foldable.TH
import Codec.Winery
import GHC.Generics
import Control.DeepSeq

data Query = QuerySelect Select |
             QueryValues [[ScalarExpr]] |
             QueryTable TableName
             deriving (Show, Eq, Generic, NFData)
             deriving Serialise via WineryVariant Query

data Select = Select { distinctness :: Maybe Distinctness,
                       projectionClause :: [SelectItem],
                       tableExpr :: Maybe TableExpr,
                       withClause :: Maybe WithClause
                     }
              deriving (Show, Eq, Generic, NFData)
              deriving Serialise via WineryRecord Select

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

data WithExpr = WithExpr WithExprAlias Select
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant WithExpr

newtype WithExprAlias = WithExprAlias Text
  deriving (Show, Eq, Generic)
  deriving Serialise via WineryVariant WithExprAlias
  deriving newtype NFData 

data InFlag = In | NotIn
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant InFlag

data ComparisonOperator = OpLT | OpGT | OpGTE | OpEQ | OpNE | OpLTE
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant ComparisonOperator

data QuantifiedComparisonPredicate = QCAny | QCSome | QCAll
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant QuantifiedComparisonPredicate

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
  deriving (Show, Eq, Generic, NFData)

data BoolOp = AndOp | OrOp
  deriving (Eq, Show, Generic, NFData)
  deriving Serialise via WineryVariant BoolOp

data InPredicateValue = InList [ScalarExpr] | InQueryExpr Select | InScalarExpr ScalarExpr
  deriving (Eq, Show, Generic, NFData)
  deriving Serialise via WineryVariant InPredicateValue

data GroupByExpr = Group ScalarExpr
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant GroupByExpr

data HavingExpr = Having ScalarExpr
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant HavingExpr

data SortExpr = SortExpr ScalarExpr (Maybe Direction) (Maybe NullsOrder)
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant SortExpr  

data Direction = Ascending | Descending
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant Direction  

data NullsOrder = NullsFirst | NullsLast
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant NullsOrder  

data JoinType = InnerJoin | RightOuterJoin | LeftOuterJoin | FullOuterJoin | CrossJoin | NaturalJoin
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant JoinType  

data JoinCondition = JoinOn JoinOnCondition | JoinUsing [UnqualifiedColumnName]
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant JoinCondition

newtype JoinOnCondition = JoinOnCondition ScalarExpr
  deriving (Show, Eq, Generic)
  deriving Serialise via WineryVariant JoinOnCondition
  deriving newtype NFData

data ColumnProjectionName = ColumnProjectionName [ProjectionName] --dot-delimited reference
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant ColumnProjectionName

data ProjectionName = ProjectionName Text | Asterisk
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant ProjectionName

data ColumnName = ColumnName [Text]
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant ColumnName

data UnqualifiedColumnName = UnqualifiedColumnName Text
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant UnqualifiedColumnName

data TableName = TableName [Text]
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant TableName

data OperatorName = OperatorName [Text]
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving Serialise via WineryVariant OperatorName

newtype ColumnAlias = ColumnAlias { unColumnAlias :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving Serialise via WineryVariant ColumnAlias
  deriving newtype NFData

newtype TableAlias = TableAlias { unTableAlias :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving Serialise via WineryVariant TableAlias
  deriving newtype (Monoid, Semigroup, NFData)

newtype FuncName = FuncName [Text]
  deriving (Show, Eq, Generic)
  deriving Serialise via WineryVariant FuncName
  deriving newtype NFData

data Distinctness = Distinct | All
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant Distinctness

newtype RestrictionExpr = RestrictionExpr ScalarExpr
  deriving (Show, Eq, Generic)
  deriving Serialise via WineryVariant RestrictionExpr
  deriving newtype NFData

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

emptyTableExpr :: TableExpr
emptyTableExpr = TableExpr { fromClause = [],
                             whereClause = Nothing,
                             groupByClause = [],
                             havingClause = Nothing,
                             orderByClause = [],
                             limitClause = Nothing,
                             offsetClause = Nothing }

makeBaseFunctor ''ScalarExprBase

