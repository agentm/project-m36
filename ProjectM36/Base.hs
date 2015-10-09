{-# LANGUAGE ExistentialQuantification,BangPatterns,GADTs,DeriveGeneric,DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Base where
import qualified Data.Map as M
import qualified Data.HashSet as HS
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Set as S
import Control.Monad.State (State)
import Data.UUID (UUID)
import Control.DeepSeq (NFData, rnf)
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Text (Text,pack,unpack)
import Data.Binary
import Data.Vector.Binary()
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar (Day,toGregorian,fromGregorian)
import Data.Hashable.Time ()
import Data.Typeable
import Text.Read (readMaybe)
import Data.ByteString (ByteString)
import ProjectM36.ConcreteTypeRep

type StringType = Text

class (Read a, 
       Hashable a, 
       Binary a, 
       Eq a, 
       Show a, 
       Typeable a, 
       NFData a) => Atomable a where
  fromText :: Text -> Either String a
  fromText t = case readMaybe (unpack t) of
    Just v -> Right v
    Nothing -> Left "parse error"
  
  toText :: a -> Text
  toText a = pack $ show a
  
instance Atomable Int 
instance Atomable Double
instance Atomable Text
instance Atomable Day
instance Atomable UTCTime
instance Atomable ByteString
instance Atomable Bool
instance Atomable Relation
instance (Atomable a) => Atomable (Maybe a)
  
data Atom = forall a. Atomable a => Atom a

instance Eq Atom where
  (Atom a) == (Atom b) = Just a == cast b
  
instance Show Atom where
  show (Atom atom) = "Atom " ++ show atom
  
instance Binary Atom where
  put atom = put atom
  get = do
    r <- get :: (Get Atom)
    return r

instance NFData Atom where 
  rnf !(Atom a) = rnf a
  
instance Hashable Atom where  
  hashWithSalt salt (Atom a) = hashWithSalt salt a
                   
--not so great orphan instance                   
instance Binary UTCTime where
  put utc = put $ toRational (utcTimeToPOSIXSeconds utc)
  get = do 
    r <- get :: Get Rational
    return (posixSecondsToUTCTime (fromRational r))
    
instance Binary Day where    
  put day = put $ toGregorian day
  get = do
    (y,m,d) <- get :: Get (Integer, Int, Int)
    return $ fromGregorian y m d
                                           
data AtomType = AtomType ConcreteTypeRep | 
                RelationAtomType Attributes |
                AnyAtomType --wildcard used in Atom Functions
              deriving (Eq,Show,NFData,Generic,Binary)

isRelationAtomType :: AtomType -> Bool
isRelationAtomType (RelationAtomType _) = True
isRelationAtomType _ = False

type AttributeName = StringType

data Attribute = Attribute AttributeName AtomType deriving (Eq, Show, Generic)

instance Hashable Attribute where
  hashWithSalt salt (Attribute attrName _) = hashWithSalt salt attrName

instance NFData Attribute where rnf = genericRnf
                                
instance Binary Attribute

type Attributes = V.Vector Attribute

attributesEqual :: Attributes -> Attributes -> Bool
attributesEqual attrs1 attrs2 = attrsAsSet attrs1 == attrsAsSet attrs2
  where
    attrsAsSet = HS.fromList . V.toList
    
sortedAttributesIndices :: Attributes -> [(Int, Attribute)]    
sortedAttributesIndices attrs = L.sortBy (\(_, (Attribute name1 _)) (_,(Attribute name2 _)) -> compare name1 name2) $ V.toList (V.indexed attrs)

newtype RelationTupleSet = RelationTupleSet { asList :: [RelationTuple] } deriving (Hashable, Show, Generic, Binary)

instance Read Relation where
  readsPrec = error "relation read not supported"

instance Eq RelationTupleSet where
 set1 == set2 = hset set1 == hset set2
   where
     hset = HS.fromList . asList

instance NFData RelationTupleSet where rnf = genericRnf

--the same hash must be generated for equal tuples so that the hashset equality works
instance Hashable RelationTuple where
  hashWithSalt salt (RelationTuple attrs tupVec) = salt `hashWithSalt` 
                                                   sortedAttrs `hashWithSalt`
                                                   (V.toList sortedTupVec)
    where
      sortedAttrsIndices = sortedAttributesIndices attrs
      sortedAttrs = map snd sortedAttrsIndices
      sortedTupVec = V.map (\(index, _) -> tupVec V.! index) $ V.fromList sortedAttrsIndices
  
--maybe the attribute->int mapping should be stored once in the relation and then passed down when needed  
data RelationTuple = RelationTuple Attributes (V.Vector Atom) deriving (Show, Generic)

instance Binary RelationTuple

instance Eq RelationTuple where
  (==) tuple1@(RelationTuple attrs1 _) tuple2@(RelationTuple attrs2 _) = attributesEqual attrs1 attrs2 && atomsEqual
    where
    atomForAttribute attr (RelationTuple attrs tupVec) = case V.findIndex (== attr) attrs of
      Nothing -> Nothing
      Just index -> tupVec V.!? index
    atomsEqual = V.all (== True) $ V.map (\attr -> atomForAttribute attr tuple1 == atomForAttribute attr tuple2) attrs1

instance NFData RelationTuple where rnf = genericRnf

data Relation = Relation Attributes RelationTupleSet deriving (Show, Generic,Typeable)

instance Eq Relation where
  Relation attrs1 tupSet1 == Relation attrs2 tupSet2 = attributesEqual attrs1 attrs2 && tupSet1 == tupSet2

instance NFData Relation where rnf = genericRnf
                               
instance Hashable Relation where                               
  hashWithSalt salt (Relation attrs tupSet) = salt `hashWithSalt` 
                                              sortedAttrs `hashWithSalt`
                                              asList tupSet
    where
      sortedAttrs = map snd (sortedAttributesIndices attrs)
      
instance Binary Relation      
  
data RelationCardinality = Uncountable | Countable Int deriving (Eq, Show, Generic, Ord)
data RelationSizeInfinite = RelationSizeInfinite

type RelVarName = StringType

data RelationalExpr where
  MakeStaticRelation :: Attributes -> RelationTupleSet -> RelationalExpr
  ExistingRelation :: Relation -> RelationalExpr
  --MakeFunctionalRelation (creates a relation from a tuple-generating function, potentially infinite)
  --in Tutorial D, relational variables pick up the type of the first relation assigned to them
  --relational variables should also be able to be explicitly-typed like in Haskell
  RelationVariable :: RelVarName -> RelationalExpr
  Project :: AttributeNames -> RelationalExpr -> RelationalExpr
  Union :: RelationalExpr -> RelationalExpr -> RelationalExpr
  Join :: RelationalExpr -> RelationalExpr -> RelationalExpr
  Rename :: AttributeName -> AttributeName -> RelationalExpr -> RelationalExpr
  Group :: AttributeNames -> AttributeName -> RelationalExpr -> RelationalExpr
  Ungroup :: AttributeName -> RelationalExpr -> RelationalExpr
  Restrict :: RestrictionPredicateExpr -> RelationalExpr -> RelationalExpr  
  Equals :: RelationalExpr -> RelationalExpr -> RelationalExpr
  NotEquals :: RelationalExpr -> RelationalExpr -> RelationalExpr
  Extend :: TupleExpr -> RelationalExpr -> RelationalExpr
  --Summarize :: AtomExpr -> AttributeName -> RelationalExpr -> RelationalExpr -> RelationalExpr -- a special case of Extend
  deriving (Show,Eq, Generic)

instance Binary RelationalExpr

data DatabaseContext = DatabaseContext { 
  inclusionDependencies :: M.Map IncDepName InclusionDependency,
  relationVariables :: M.Map RelVarName Relation,
  atomFunctions :: AtomFunctions
  } deriving (Show, Generic)
             
--instance Binary DatabaseContext             
             
type IncDepName = StringType             

data InclusionDependency = InclusionDependency RelationalExpr RelationalExpr deriving (Show, Eq, Generic)

instance Binary InclusionDependency

--Database context expressions modify the database context while relational expressions do not
data DatabaseExpr where
  Define :: RelVarName -> Attributes -> DatabaseExpr
  Undefine :: RelVarName -> DatabaseExpr --forget existence of relvar X
  Assign :: RelVarName -> RelationalExpr -> DatabaseExpr
  Insert :: RelVarName -> RelationalExpr -> DatabaseExpr
  Delete :: RelVarName -> RestrictionPredicateExpr -> DatabaseExpr 
  Update :: RelVarName  -> M.Map AttributeName Atom -> RestrictionPredicateExpr -> DatabaseExpr -- needs restriction support
  AddInclusionDependency :: IncDepName -> InclusionDependency -> DatabaseExpr
  RemoveInclusionDependency :: IncDepName -> DatabaseExpr
  MultipleExpr :: [DatabaseExpr] -> DatabaseExpr
  deriving (Show,Eq)

type DatabaseState a = State DatabaseContext a

data RestrictionPredicateExpr where
  TruePredicate :: RestrictionPredicateExpr
  AndPredicate :: RestrictionPredicateExpr -> RestrictionPredicateExpr -> RestrictionPredicateExpr
  OrPredicate :: RestrictionPredicateExpr -> RestrictionPredicateExpr -> RestrictionPredicateExpr
  NotPredicate :: RestrictionPredicateExpr -> RestrictionPredicateExpr
  RelationalExprPredicate :: RelationalExpr -> RestrictionPredicateExpr --type must be same as true and false relations (no attributes)
  AtomExprPredicate :: AtomExpr -> RestrictionPredicateExpr --atom must evaluate to boolean
  AttributeEqualityPredicate :: AttributeName -> AtomExpr -> RestrictionPredicateExpr -- relationalexpr must result in relation with single tuple
  deriving (Show, Eq, Generic)

instance Binary RestrictionPredicateExpr

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
                     deriving(Show, Generic)
                             
instance Binary TransactionInfo                             
                             
data Transaction = Transaction UUID TransactionInfo DatabaseContext -- self uuid
                   deriving (Show, Generic)

--instance Binary Transaction
                            
--represents an "in-progress" transaction which has not yet been added to the transaction graph
--one the transaction is "complete", it is committed and no longer can be changed
-- this is similar to the index in git
data DisconnectedTransaction = DisconnectedTransaction UUID DatabaseContext --parentUUID, context
                            
transactionUUID :: Transaction -> UUID
transactionUUID (Transaction uuid _ _) = uuid

transactionContext :: Transaction -> DatabaseContext
transactionContext (Transaction _ _ context) = context

transactionInfo :: Transaction -> TransactionInfo
transactionInfo (Transaction _ info _) = info
                            
instance Eq Transaction where                            
  (Transaction uuidA _ _) == (Transaction uuidB _ _) = uuidA == uuidB
                   
instance Ord Transaction where                            
  compare (Transaction uuidA _ _) (Transaction uuidB _ _) = compare uuidA uuidB

--used on the right side of attribute assignments
data AtomExpr = AttributeAtomExpr AttributeName |
                NakedAtomExpr Atom |
                FunctionAtomExpr AtomFunctionName [AtomExpr] |
                RelationAtomExpr RelationalExpr
              deriving (Eq,Show,Generic)
                       
instance Binary AtomExpr                       

--used to extend a relation
data TupleExpr = AttributeTupleExpr AttributeName AtomExpr
  deriving (Show, Eq, Generic)
           
instance Binary TupleExpr           
           
--enumerates the list of functions available to be run as part of tuple expressions           
type AtomFunctions = HS.HashSet AtomFunction

type AtomFunctionName = StringType

data AtomFunction = AtomFunction {
  atomFuncName :: AtomFunctionName,
  atomFuncType :: [AtomType], 
  atomFunc :: [Atom] -> Atom
  }
           
instance Hashable AtomFunction where
  hashWithSalt salt func = salt `hashWithSalt` (atomFuncName func)
                           
instance Eq AtomFunction where                           
  f1 == f2 = (atomFuncName f1) == (atomFuncName f2)
  
instance Show AtomFunction where  
  show aFunc = unpack (atomFuncName aFunc) ++ "::" ++ showArgTypes
   where
     showArgTypes = concat (L.intersperse "->" $ map show (atomFuncType aFunc))
     
data AttributeNames = AttributeNames (S.Set AttributeName) |
                      InvertedAttributeNames (S.Set AttributeName)
                      deriving (Eq, Show, Generic)
                                
instance Binary AttributeNames 

--used for global dbms configuration- per-transaction flag is elsewhere
data PersistenceStrategy = NoPersistence | --no filesystem persistence/memory-only database
                           MinimalPersistence FilePath  --fsync off
                           --CrashSafePersistence FilePath --full fsync- not yet implemented
                           deriving (Show, Read)
                                    
    
