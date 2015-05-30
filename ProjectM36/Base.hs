{-# LANGUAGE GADTs,DeriveGeneric #-}
module ProjectM36.Base where
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.Hashable as Hash
import qualified Data.Set as S
import Control.Monad.State hiding (join)
import Data.UUID (UUID)
import Control.DeepSeq (NFData, rnf)
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Text as T

type StringType = T.Text

data Atom = StringAtom StringType |
            IntAtom Int |
            RelationAtom Relation 
          deriving (Show, Eq, Generic)
                                           
instance NFData Atom where rnf = genericRnf
                           
instance Hash.Hashable Atom                           

data AtomType = StringAtomType |
                IntAtomType |
                RelationAtomType Attributes |
                AnyAtomType --AnyAtomType is used as a wildcard
              deriving (Eq, Show, Generic)
                                                     
instance NFData AtomType where rnf = genericRnf

type AttributeName = T.Text

data Attribute = Attribute AttributeName AtomType deriving (Eq, Show, Generic)

instance Hash.Hashable Attribute where
  hashWithSalt salt (Attribute attrName _) = Hash.hashWithSalt salt attrName

instance NFData Attribute where rnf = genericRnf

type Attributes = V.Vector Attribute

attributesEqual :: Attributes -> Attributes -> Bool
attributesEqual attrs1 attrs2 = attrsAsSet attrs1 == attrsAsSet attrs2
  where
    attrsAsSet = HS.fromList . V.toList
    
sortedAttributesIndices :: Attributes -> [(Int, Attribute)]    
sortedAttributesIndices attrs = L.sortBy (\(_, (Attribute name1 _)) (_,(Attribute name2 _)) -> compare name1 name2) $ V.toList (V.indexed attrs)

type RelationTupleSet = HS.HashSet RelationTuple 

--the same hash must be generated for equal tuples so that the hashset equality works
instance Hash.Hashable RelationTuple where
  hashWithSalt salt (RelationTuple attrs tupVec) = salt `Hash.hashWithSalt` 
                                                   sortedAttrs `Hash.hashWithSalt`
                                                   (V.toList sortedTupVec)
    where
      sortedAttrsIndices = sortedAttributesIndices attrs
      sortedAttrs = map snd sortedAttrsIndices
      sortedTupVec = V.map (\(index, _) -> tupVec V.! index) $ V.fromList sortedAttrsIndices
  
--maybe the attribute->int mapping should be stored once in the relation and then passed down when needed  
data RelationTuple = RelationTuple Attributes (V.Vector Atom) deriving (Show, Generic)

instance Eq RelationTuple where
  (==) tuple1@(RelationTuple attrs1 _) tuple2@(RelationTuple attrs2 _) = attributesEqual attrs1 attrs2 && atomsEqual
    where
    atomForAttribute attr (RelationTuple attrs tupVec) = case V.findIndex (== attr) attrs of
      Nothing -> Nothing
      Just index -> tupVec V.!? index
    atomsEqual = V.all (== True) $ V.map (\attr -> atomForAttribute attr tuple1 == atomForAttribute attr tuple2) attrs1

instance NFData RelationTuple where rnf = genericRnf

data Relation = Relation Attributes RelationTupleSet deriving (Show, Generic)

instance Eq Relation where
  Relation attrs1 tupSet1 == Relation attrs2 tupSet2 = attributesEqual attrs1 attrs2 && tupSet1 == tupSet2

instance NFData Relation where rnf = genericRnf
                               
instance Hash.Hashable Relation where                               
  hashWithSalt salt (Relation attrs tupSet) = salt `Hash.hashWithSalt` 
                                              sortedAttrs `Hash.hashWithSalt`
                                              HS.toList tupSet
    where
      sortedAttrs = map snd (sortedAttributesIndices attrs)
  
data RelationCardinality = Uncountable | Countable Int deriving (Eq, Show, Generic)
data RelationSizeInfinite = RelationSizeInfinite

type RelVarName = StringType

data RelationalExpr where
  MakeStaticRelation :: Attributes -> RelationTupleSet -> RelationalExpr
  ExistingRelation :: Relation -> RelationalExpr
  --MakeFunctionalRelation (creates a relation from a tuple-generating function, potentially infinite)
  --in Tutorial D, relational variables pick up the type of the first relation assigned to them
  --relational variables should also be able to be explicitly-typed like in Haskell
  RelationVariable :: RelVarName -> RelationalExpr
  Project :: S.Set AttributeName -> RelationalExpr -> RelationalExpr
  Union :: RelationalExpr -> RelationalExpr -> RelationalExpr
  Join :: RelationalExpr -> RelationalExpr -> RelationalExpr
  Rename :: AttributeName -> AttributeName -> RelationalExpr -> RelationalExpr
  Group :: S.Set AttributeName -> AttributeName -> RelationalExpr -> RelationalExpr
  Ungroup :: AttributeName -> RelationalExpr -> RelationalExpr
  Restrict :: RestrictionPredicateExpr -> RelationalExpr -> RelationalExpr  
  Equals :: RelationalExpr -> RelationalExpr -> RelationalExpr
  Extend :: TupleExpr -> RelationalExpr -> RelationalExpr
  --Summarize :: AtomExpr -> AttributeName -> RelationalExpr -> RelationalExpr -> RelationalExpr -- a special case of Extend
  deriving (Show,Eq)

data DatabaseContext = DatabaseContext { 
  inclusionDependencies :: HS.HashSet InclusionDependency,
  relationVariables :: M.Map RelVarName  Relation,
  atomFunctions :: AtomFunctions
  } deriving (Show)
             
type IncDepName = StringType             

data InclusionDependency = InclusionDependency IncDepName RelationalExpr RelationalExpr deriving (Show)

instance Hash.Hashable InclusionDependency where
  hashWithSalt salt dep = Hash.hashWithSalt salt (show dep)
  
instance Eq InclusionDependency where
  (==) (InclusionDependency nameA _ _) (InclusionDependency nameB _ _) = nameA == nameB

--Database context expressions modify the database context while relational expressions do not
data DatabaseExpr where
  Define :: RelVarName -> Attributes -> DatabaseExpr
  Undefine :: RelVarName -> DatabaseExpr --forget existence of relvar X
  Assign :: RelVarName -> RelationalExpr -> DatabaseExpr
  Insert :: RelVarName -> RelationalExpr -> DatabaseExpr
  Delete :: RelVarName -> RestrictionPredicateExpr -> DatabaseExpr 
  Update :: RelVarName  -> M.Map AttributeName Atom -> RestrictionPredicateExpr -> DatabaseExpr -- needs restriction support
  AddInclusionDependency :: InclusionDependency -> DatabaseExpr
  MultipleExpr :: [DatabaseExpr] -> DatabaseExpr
  deriving (Show,Eq)

type DatabaseState a = State DatabaseContext a

data RestrictionPredicateExpr where
  TruePredicate :: RestrictionPredicateExpr
  AndPredicate :: RestrictionPredicateExpr -> RestrictionPredicateExpr -> RestrictionPredicateExpr
  OrPredicate :: RestrictionPredicateExpr -> RestrictionPredicateExpr -> RestrictionPredicateExpr
  NotPredicate :: RestrictionPredicateExpr -> RestrictionPredicateExpr
  RelationalExprPredicate :: RelationalExpr -> RestrictionPredicateExpr --type must be same as true and false relations (no attributes)
  AttributeEqualityPredicate :: AttributeName -> AtomExpr -> RestrictionPredicateExpr -- relationalexpr must result in relation with single tuple
  deriving (Show, Eq)

-- child + parent links
-- the string represents the branch name and can be used to find the named HEADs
type HeadName = StringType

type TransactionHeads = M.Map HeadName Transaction

data TransactionGraph = TransactionGraph TransactionHeads (S.Set Transaction)
                        deriving (Show, Eq)

transactionsForGraph :: TransactionGraph -> S.Set Transaction
transactionsForGraph (TransactionGraph _ t) = t

transactionHeadsForGraph :: TransactionGraph -> TransactionHeads
transactionHeadsForGraph (TransactionGraph heads _) = heads

data TransactionInfo = TransactionInfo UUID (S.Set UUID) | -- 1 parent + n children
                       MergeTransactionInfo UUID UUID (S.Set UUID) -- 2 parents, n children
                     deriving(Show)
                             
data Transaction = Transaction UUID TransactionInfo DatabaseContext -- self uuid
                   deriving (Show)
                            
--represents an "in-progress" transaction which has not yet been added to the transaction graph
--one the transaction is "complete", it is committed and no longer can be changed
-- this is similar to the index in git
data DisconnectedTransaction = DisconnectedTransaction UUID DatabaseContext --parentUUID, context
                            
transactionUUID :: Transaction -> UUID
transactionUUID (Transaction uuid _ _) = uuid

transactionContext :: Transaction -> DatabaseContext
transactionContext (Transaction _ _ context) = context
                            
instance Eq Transaction where                            
  (Transaction uuidA _ _) == (Transaction uuidB _ _) = uuidA == uuidB
                   
instance Ord Transaction where                            
  compare (Transaction uuidA _ _) (Transaction uuidB _ _) = compare uuidA uuidB

--used on the right side of attribute assignments
data AtomExpr = AttributeAtomExpr AttributeName |
                NakedAtomExpr Atom |
                FunctionAtomExpr AtomFunctionName [AtomExpr] |
                RelationAtomExpr RelationalExpr
              deriving (Eq,Show)

--used to extend a relation
data TupleExpr = AttributeTupleExpr AttributeName AtomExpr
  deriving (Show, Eq)
           
--enumerates the list of functions available to be run as part of tuple expressions           
type AtomFunctions = HS.HashSet AtomFunction

type AtomFunctionName = StringType

data AtomFunction = AtomFunction {
  atomFuncName :: AtomFunctionName,
  atomFuncType :: [AtomType], 
  atomFunc :: [Atom] -> Atom
  }
           
instance Hash.Hashable AtomFunction where
  hashWithSalt salt func = salt `Hash.hashWithSalt` (atomFuncName func)
                           
instance Eq AtomFunction where                           
  f1 == f2 = (atomFuncName f1) == (atomFuncName f2)
  
instance Show AtomFunction where  
  show aFunc = T.unpack (atomFuncName aFunc) ++ "::" ++ showArgTypes
   where
     showArgTypes = concat (L.intersperse "->" $ map show (atomFuncType aFunc))
