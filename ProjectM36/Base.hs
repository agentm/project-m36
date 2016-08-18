{-# LANGUAGE ExistentialQuantification,BangPatterns,DeriveGeneric,DeriveAnyClass #-}
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
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as BS64
import Data.Maybe (isJust)

type StringType = Text

-- | Any data type represents as an 'Atom' in the database must adhere to the 'Atomable' typeclass.
class (Read a, 
       Hashable a, 
       Binary a, 
       Eq a, 
       Show a, 
       Typeable a, 
       NFData a) => Atomable a where
  fromText :: (Atomable a) => Text -> Either String a
  fromText t = case readMaybe (unpack t) of
    Just v -> Right v
    Nothing -> Left ("Parse error: " ++ (unpack t))
  
  toText :: a -> Text
  toText a = pack $ show a
  
instance Atomable Int 
instance Atomable Double
instance Atomable Text where
instance Atomable Day
instance Atomable UTCTime
instance Atomable ByteString where
  toText bs = TE.decodeUtf8With (\_ _  -> Just '?') (BS64.encode bs)
instance Atomable Bool
instance Atomable Relation
instance (Atomable a) => Atomable (Maybe a)
  
-- | Database atoms are the smallest, undecomposable units of a tuple. Common examples are integers, text, or unique identity keys.
data Atom = IntAtom Int |
            DoubleAtom Double |
            TextAtom Text |
            DayAtom Day |
            DateTimeAtom UTCTime |
            ByteStringAtom ByteString |
            BoolAtom Bool |
            RelationAtom Relation |
            ConstructedAtom DataConstructorName AtomType [Atom]
            deriving (Eq, Show, Hashable, Binary, Typeable, NFData)

instance Binary UTCTime where
  put utc = put $ toRational (utcTimeToPOSIXSeconds utc)
  get = do 
    r <- get :: Get Rational
    return (posixSecondsToUTCTime (fromRational r))
    
instance Binary Day where    
  put day = put $ toGregorian day
  get = do
    (y,m,d) <- get :: Get (Integer, Int, Int)
    return (fromGregorian y m d)

-- I suspect the definition of ConstructedAtomType with its name alone is insufficient to disambiguate the cases; for example, one could create a type named X, remove a type named X, and re-add it using different constructors. However, as long as requests are served from only one DatabaseContext at-a-time, the type name is unambiguous. This will become a problem for time-travel, however.
-- | The AtomType must uniquely identify the type of a atom.
data AtomType = IntAtomType |
                DoubleAtomType |
                TextAtomType |
                DayAtomType |
                DateTimeAtomType |
                ByteStringAtomType |
                BoolAtomType |
                RelationAtomType Attributes |
                ConstructedAtomType TypeConstructorName TypeVarMap |
                AnyAtomType
                --wildcard used in Atom Functions and tuples for data constructors which don't provide all arguments to the type constructor
              deriving (Eq,NFData,Generic,Binary, Show, Hashable)
                       
type TypeVarMap = M.Map TypeVarName AtomType
                       
-- | Return True iff the atom type argument is relation-valued. If True, this indicates that the Atom contains a relation.
isRelationAtomType :: AtomType -> Bool
isRelationAtomType (RelationAtomType _) = True
isRelationAtomType _ = False

-- | The AttributeName is the name of an attribute in a relation.
type AttributeName = StringType

-- | A relation's type is composed of attribute names and types.
data Attribute = Attribute AttributeName AtomType deriving (Eq, Show, Generic)

instance Hashable Attribute where
  hashWithSalt salt (Attribute attrName _) = hashWithSalt salt attrName

instance NFData Attribute where rnf = genericRnf
                                
instance Binary Attribute

-- | 'Attributes' represent the head of a relation.
type Attributes = V.Vector Attribute

-- | Equality function for a set of attributes.
attributesEqual :: Attributes -> Attributes -> Bool
attributesEqual attrs1 attrs2 = attrsAsSet attrs1 == attrsAsSet attrs2
  where
    attrsAsSet = HS.fromList . V.toList
    
sortedAttributesIndices :: Attributes -> [(Int, Attribute)]    
sortedAttributesIndices attrs = L.sortBy (\(_, (Attribute name1 _)) (_,(Attribute name2 _)) -> compare name1 name2) $ V.toList (V.indexed attrs)

-- | The relation's tuple set is the body of the relation.
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
  --sanity check the tuple for attribute and tuple counts
  --this bit me when tuples were being hashed before being verified
  hashWithSalt salt (RelationTuple attrs tupVec) = if V.length attrs /= V.length tupVec then
                                                     error "invalid tuple: attributes and tuple count mismatch"
                                                   else
                                                     salt `hashWithSalt` 
                                                     sortedAttrs `hashWithSalt`
                                                     (V.toList sortedTupVec)
    where
      sortedAttrsIndices = sortedAttributesIndices attrs
      sortedAttrs = map snd sortedAttrsIndices
      sortedTupVec = V.map (\(index, _) -> tupVec V.! index) $ V.fromList sortedAttrsIndices
  
-- | A tuple is a set of attributes mapped to their 'Atom' values.
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
  
data RelationCardinality = Countable | Finite Int deriving (Eq, Show, Generic, Ord)

-- | Relation variables are identified by their names.
type RelVarName = StringType

-- | A relational expression represents query (read) operations on a database.
data RelationalExpr =
  --- | Create a relation from tuple expressions.
  MakeRelationFromExprs (Maybe [AttributeExpr]) [TupleExpr] |
  --- | Create and reference a relation from attributes and a tuple set.
  MakeStaticRelation Attributes RelationTupleSet |
  --- | Reference an existing relation in Haskell-space.
  ExistingRelation Relation |
  --MakeFunctionalRelation (creates a relation from a tuple-generating function, potentially infinite)
  --in Tutorial D, relational variables pick up the type of the first relation assigned to them
  --relational variables should also be able to be explicitly-typed like in Haskell
  --- | Reference a relation variable by its name.
  RelationVariable RelVarName |
  --- | Create a projection over attribute names. (Note that the 'AttributeNames' structure allows for the names to be inverted.)
  Project AttributeNames RelationalExpr |
  --- | Create a union of two relational expressions. The expressions should have identical attributes.
  Union RelationalExpr RelationalExpr |
  --- | Create a join of two relational expressions. The join occurs on attributes which are identical. If the expressions have no overlapping attributes, the join becomes a cross-product of both tuple sets.
  Join RelationalExpr RelationalExpr |
  --- | Rename an attribute (first argument) to another (second argument).
  Rename AttributeName AttributeName RelationalExpr |
  --- | Return a relation containing all tuples of the first argument which do not appear in the second argument (minus).
  Difference RelationalExpr RelationalExpr |
  --- | Create a sub-relation composed of the first argument's attributes which will become an attribute of the result expression. The unreferenced attributes are not altered in the result but duplicate tuples in the projection of the expression minus the attribute names are compressed into one. For more information, <https://github.com/agentm/project-m36/blob/master/docs/introduction_to_the_relational_algebra.markdown#group read the relational algebra tutorial.>
  Group AttributeNames AttributeName RelationalExpr |
  --- | Create an expression to unwrap a sub-relation contained within at an attribute's name. Note that this is not always an inverse of a group operation.
  Ungroup AttributeName RelationalExpr |
  --- | Filter the tuples of the relational expression to only retain the tuples which evaluate against the restriction predicate to true.
  Restrict RestrictionPredicateExpr RelationalExpr |
  --- | Returns the true relation iff 
  Equals RelationalExpr RelationalExpr |
  NotEquals RelationalExpr RelationalExpr |
  Extend ExtendTupleExpr RelationalExpr 
  --Summarize :: AtomExpr -> AttributeName -> RelationalExpr -> RelationalExpr -> RelationalExpr -- a special case of Extend
  deriving (Show, Eq, Generic, Binary)

type NotificationName = StringType
type Notifications = M.Map NotificationName Notification

-- | When the changeExpr returns a different result in the database context, then the reportExpr is triggered and sent asynchronously to all clients.
data Notification = Notification {
  changeExpr :: RelationalExpr,
  reportExpr :: RelationalExpr
  }
  deriving (Show, Eq, Binary, Generic)

type TypeVarName = StringType
  
-- | "data Either a b"
data TypeConstructorDef = ADTypeConstructorDef TypeConstructorName [TypeVarName] |
                          PrimitiveTypeConstructorDef TypeConstructorName AtomType
                        deriving (Show, Generic, Binary, Eq, NFData)
                                 
-- | Found in data constructors and type declarations: Left (Either Int Text) | Right Int
data TypeConstructor = ADTypeConstructor TypeConstructorName [TypeConstructorArg] |
                       PrimitiveTypeConstructor TypeConstructorName AtomType
                       deriving (Show, Generic, Binary, Eq, NFData)
                                
data TypeConstructorArg = TypeConstructorArg TypeConstructor |                                
                          TypeConstructorTypeVarArg TypeVarName
                          deriving (Show, Generic, Binary, Eq, NFData)
                       
type TypeConstructorMapping = [(TypeConstructorDef, DataConstructorDefs)]

type TypeConstructorName = StringType
type TypeConstructorArgName = StringType
type DataConstructorName = StringType
type AtomTypeName = StringType

-- | Used to define a data constructor in a type constructor context. (Left a | Right b)
data DataConstructorDef = DataConstructorDef DataConstructorName [DataConstructorDefArg] deriving (Eq, Show, Binary, Generic)

type DataConstructorDefs = [DataConstructorDef]

data DataConstructorDefArg = DataConstructorDefTypeConstructorArg TypeConstructor | 
                             DataConstructorDefTypeVarNameArg TypeVarName
                           deriving (Show, Generic, Binary, Eq, NFData)
                                    
type InclusionDependencies = M.Map IncDepName InclusionDependency
type RelationVariables = M.Map RelVarName Relation
                                    
-- | The DatabaseContext is a snapshot of a database's evolving state and contains everything a database client can change over time.
data DatabaseContext = DatabaseContext { 
  inclusionDependencies :: InclusionDependencies,
  relationVariables :: RelationVariables,
  atomFunctions :: AtomFunctions,
  notifications :: Notifications,
  typeConstructorMapping :: TypeConstructorMapping
  } deriving (Show, Generic)
             
type IncDepName = StringType             

-- | Inclusion dependencies represent every possible database constraints. Constraints enforce specific, arbitrarily-complex rules to which the database context must adhere.
data InclusionDependency = InclusionDependency RelationalExpr RelationalExpr deriving (Show, Eq, Generic)

instance Binary InclusionDependency

-- | Database context expressions modify the database context.
data DatabaseContextExpr = 
  NoOperation |
  Define RelVarName [AttributeExpr] |
  Undefine RelVarName | --forget existence of relvar X
  Assign RelVarName RelationalExpr |
  Insert RelVarName RelationalExpr |
  Delete RelVarName RestrictionPredicateExpr |
  Update RelVarName (M.Map AttributeName AtomExpr) RestrictionPredicateExpr |
  
  AddInclusionDependency IncDepName InclusionDependency |
  RemoveInclusionDependency IncDepName |
  
  AddNotification NotificationName RelationalExpr RelationalExpr |
  RemoveNotification NotificationName |

  AddTypeConstructor TypeConstructorDef [DataConstructorDef] |
  RemoveTypeConstructor TypeConstructorName |

  --adding an AtomFunction is not a pure operation (required loading GHC modules)
  RemoveAtomFunction AtomFunctionName |
  
  MultipleExpr [DatabaseContextExpr]
  deriving (Show, Eq, Binary, Generic)

-- | Adding an atom function should be nominally a DatabaseExpr except for the fact that it cannot be performed purely. Thus, we create the DatabaseContextIOExpr.
data DatabaseContextIOExpr = AddAtomFunction AtomFunctionName [TypeConstructor] AtomFunctionBodyScript
                           deriving (Show, Eq, Generic, Binary)

type DatabaseState a = State DatabaseContext a

-- | Restriction predicate are boolean algebra components which, when composed, indicate whether or not a tuple should be retained during a restriction (filtering) operation.
data RestrictionPredicateExpr =
  TruePredicate |
  AndPredicate RestrictionPredicateExpr RestrictionPredicateExpr |
  OrPredicate RestrictionPredicateExpr RestrictionPredicateExpr |
  NotPredicate RestrictionPredicateExpr |
  RelationalExprPredicate RelationalExpr | --type must be same as true and false relations (no attributes)
  AtomExprPredicate AtomExpr | --atom must evaluate to boolean
  AttributeEqualityPredicate AttributeName AtomExpr -- relationalexpr must result in relation with single tuple
  deriving (Show, Eq, Generic)

instance Binary RestrictionPredicateExpr

-- child + parent links
-- | A transaction graph's head name references the leaves of the transaction graph and can be used during session creation to indicate at which point in the graph commits should persist.
type HeadName = StringType

type TransactionHeads = M.Map HeadName Transaction

-- | The transaction graph is the global database's state which references every committed transaction.
data TransactionGraph = TransactionGraph TransactionHeads (S.Set Transaction)
                        deriving (Show, Eq)

transactionsForGraph :: TransactionGraph -> S.Set Transaction
transactionsForGraph (TransactionGraph _ t) = t

transactionHeadsForGraph :: TransactionGraph -> TransactionHeads
transactionHeadsForGraph (TransactionGraph heads _) = heads

-- | Every transaction has context-specific information attached to it.
data TransactionInfo = TransactionInfo TransactionId (S.Set TransactionId) | -- 1 parent + n children
                       MergeTransactionInfo TransactionId TransactionId (S.Set TransactionId) -- 2 parents, n children
                     deriving(Show, Generic)
                             
instance Binary TransactionInfo                             

-- | Every set of modifications made to the database are atomically committed to the transaction graph as a transaction.
type TransactionId = UUID

data Transaction = Transaction TransactionId TransactionInfo DatabaseContext -- self uuid
                   deriving (Show, Generic)

--instance Binary Transaction
                            
-- | The disconnected transaction represents an in-progress workspace used by sessions before changes are committed. This is similar to git's "index". After a transaction is committed, it is "connected" in the transaction graph and can no longer be modified.
data DisconnectedTransaction = DisconnectedTransaction TransactionId DatabaseContext --parentID, context
                            
transactionId :: Transaction -> TransactionId
transactionId (Transaction tid _ _) = tid

transactionContext :: Transaction -> DatabaseContext
transactionContext (Transaction _ _ context) = context

transactionInfo :: Transaction -> TransactionInfo
transactionInfo (Transaction _ info _) = info
                            
instance Eq Transaction where                            
  (Transaction uuidA _ _) == (Transaction uuidB _ _) = uuidA == uuidB
                   
instance Ord Transaction where                            
  compare (Transaction uuidA _ _) (Transaction uuidB _ _) = compare uuidA uuidB

-- | An atom expression represents an action to take when extending a relation or when statically defining a relation or a new tuple.
data AtomExpr = AttributeAtomExpr AttributeName |
                NakedAtomExpr Atom |
                FunctionAtomExpr AtomFunctionName [AtomExpr] |
                RelationAtomExpr RelationalExpr |
                ConstructedAtomExpr DataConstructorName [AtomExpr]
              deriving (Eq,Show,Generic)
                       
instance Binary AtomExpr                       

-- | Used in tuple creation when creating a relation.
data ExtendTupleExpr = AttributeExtendTupleExpr AttributeName AtomExpr
                     deriving (Show, Eq, Binary, Generic)
           
--enumerates the list of functions available to be run as part of tuple expressions           
type AtomFunctions = HS.HashSet AtomFunction

type AtomFunctionName = StringType

type AtomFunctionBodyScript = StringType

type AtomFunctionBodyType = [Atom] -> Atom

data AtomFunctionBody = AtomFunctionBody (Maybe AtomFunctionBodyScript) AtomFunctionBodyType
                        
instance Show AtomFunctionBody where
  show (AtomFunctionBody mScript _) = case mScript of
    Just script -> show (unpack script)
    Nothing -> "<compiled>"

-- | An AtomFunction has a name, a type, and a function body to execute when called.
data AtomFunction = AtomFunction {
  atomFuncName :: AtomFunctionName,
  atomFuncType :: [AtomType], 
  atomFuncBody :: AtomFunctionBody
  }
           
instance Hashable AtomFunction where
  hashWithSalt salt func = salt `hashWithSalt` (atomFuncName func)
                           
instance Eq AtomFunction where                           
  f1 == f2 = let (AtomFunctionBody mScript1 _) = atomFuncBody f1 
                 (AtomFunctionBody mScript2 _) = atomFuncBody f2 in
             if atomFuncName f1 == atomFuncName f2 then
               if isJust mScript1 then
                 if mScript1 == mScript2 then
                   True
                 else 
                   False
               else
                 True
             else
               False
  
instance Show AtomFunction where  
  show aFunc = unpack (atomFuncName aFunc) ++ "::" ++ showArgTypes ++ "; " ++ body
   where
     body = show (atomFuncBody aFunc)
     showArgTypes = concat (L.intersperse "->" $ map show (atomFuncType aFunc))
     
-- | The 'AttributeNames' structure represents a set of attribute names or the same set of names but inverted in the context of a relational expression. For example, if a relational expression has attributes named "a", "b", and "c", the 'InvertedAttributeNames' of ("a","c") is ("b").
data AttributeNames = AttributeNames (S.Set AttributeName) |
                      InvertedAttributeNames (S.Set AttributeName)
                      deriving (Eq, Show, Generic)
                                
instance Binary AttributeNames 

-- | The persistence strategy is a global database option which represents how to persist the database in the filesystem, if at all.
data PersistenceStrategy = NoPersistence | -- ^ no filesystem persistence/memory-only database
                           MinimalPersistence FilePath | -- ^ fsync off, not crash-safe
                           CrashSafePersistence FilePath -- ^ full fsync to disk (flushes kernel and physical drive buffers to ensure that the transaction is on non-volatile storage)
                           deriving (Show, Read)
                                    
data AttributeExpr = AttributeAndTypeNameExpr AttributeName TypeConstructor |
                     NakedAttributeExpr Attribute
                     deriving (Eq, Show, Generic, Binary)
                              
data TupleExpr = TupleExpr (M.Map AttributeName AtomExpr)
                 deriving (Eq, Show, Generic, Binary)

data MergeStrategy = 
  -- | After a union merge, the merge transaction is a result of union'ing relvars of the same name, introducing all uniquely-named relvars, union of constraints, union of atom functions, notifications, and types (unless the names and definitions collide, e.g. two types of the same name with different definitions)
  UnionMergeStrategy |
  -- | Similar to a union merge, but, on conflict, prefer the unmerged section (relvar, function, etc.) from the branch named as the argument.
  UnionPreferMergeStrategy HeadName |
  -- | Similar to the our/theirs merge strategy in git, the merge transaction's context is identical to that of the last transaction in the selected branch.
  SelectedBranchMergeStrategy HeadName
                     deriving (Eq, Show, Binary, Generic, NFData)
