{-# LANGUAGE ExistentialQuantification,DeriveGeneric,DeriveAnyClass,FlexibleInstances,OverloadedStrings, DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectM36.Base where
import ProjectM36.DatabaseContextFunctionError
import ProjectM36.AtomFunctionError
import ProjectM36.MerkleHash

import qualified Data.Map as M
import qualified Data.HashSet as HS
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Set as S
import Data.UUID (UUID)
import Control.DeepSeq (NFData, rnf)
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics (Generic)
import GHC.Stack
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Text (Text,unpack)
import Data.Binary
import Data.Vector.Binary()
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar (Day,toGregorian,fromGregorian)
import Data.Hashable.Time ()
import Data.Typeable
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
type StringType = Text
  
-- | Database atoms are the smallest, undecomposable units of a tuple. Common examples are integers, text, or unique identity keys.
data Atom = IntegerAtom Integer |
            IntAtom Int |
            DoubleAtom Double |
            TextAtom Text |
            DayAtom Day |
            DateTimeAtom UTCTime |
            ByteStringAtom ByteString |
            BoolAtom Bool |
            RelationAtom Relation |
            ConstructedAtom DataConstructorName AtomType [Atom]
            deriving (Eq, Show, Binary, Typeable, NFData, Generic)
                     
instance Hashable Atom where                     
  hashWithSalt salt (ConstructedAtom dConsName _ atoms) = salt `hashWithSalt` atoms
                                                          `hashWithSalt` dConsName --AtomType is not hashable
  hashWithSalt salt (IntAtom i) = salt `hashWithSalt` i
  hashWithSalt salt (IntegerAtom i) = salt `hashWithSalt` i  
  hashWithSalt salt (DoubleAtom d) = salt `hashWithSalt` d
  hashWithSalt salt (TextAtom t) = salt `hashWithSalt` t
  hashWithSalt salt (DayAtom d) = salt `hashWithSalt` d
  hashWithSalt salt (DateTimeAtom dt) = salt `hashWithSalt` dt
  hashWithSalt salt (ByteStringAtom bs) = salt `hashWithSalt` bs
  hashWithSalt salt (BoolAtom b) = salt `hashWithSalt` b
  hashWithSalt salt (RelationAtom r) = salt `hashWithSalt` r

instance Binary UTCTime where
  put utc = put $ toRational (utcTimeToPOSIXSeconds utc)
  get = posixSecondsToUTCTime . fromRational <$> (get :: Get Rational)
    
instance Binary Day where    
  put day = put $ toGregorian day
  get = do
    (y,m,d) <- get :: Get (Integer, Int, Int)
    return (fromGregorian y m d)

-- I suspect the definition of ConstructedAtomType with its name alone is insufficient to disambiguate the cases; for example, one could create a type named X, remove a type named X, and re-add it using different constructors. However, as long as requests are served from only one DatabaseContext at-a-time, the type name is unambiguous. This will become a problem for time-travel, however.
-- | The AtomType uniquely identifies the type of a atom.
data AtomType = IntAtomType |
                IntegerAtomType |
                DoubleAtomType |
                TextAtomType |
                DayAtomType |
                DateTimeAtomType |
                ByteStringAtomType |
                BoolAtomType |
                RelationAtomType Attributes |
                ConstructedAtomType TypeConstructorName TypeVarMap |
                TypeVariableType TypeVarName
                --wildcard used in Atom Functions and tuples for data constructors which don't provide all arguments to the type constructor
              deriving (Eq, NFData, Generic, Binary, Show)

instance Ord AtomType where
  compare = undefined
  
type TypeVarMap = M.Map TypeVarName AtomType

instance Hashable TypeVarMap where 
  hashWithSalt salt tvmap = hashWithSalt salt (M.keys tvmap)
                       
-- | Return True iff the atom type argument is relation-valued. If True, this indicates that the Atom contains a relation.
isRelationAtomType :: AtomType -> Bool
isRelationAtomType (RelationAtomType _) = True
isRelationAtomType _ = False

-- | The AttributeName is the name of an attribute in a relation.
type AttributeName = StringType

-- | A relation's type is composed of attribute names and types.
data Attribute = Attribute AttributeName AtomType deriving (Eq, Show, Generic, NFData, Binary)

instance Hashable Attribute where
  hashWithSalt salt (Attribute attrName _) = hashWithSalt salt attrName

-- | 'Attributes' represent the head of a relation.
type Attributes = V.Vector Attribute

-- | Equality function for a set of attributes.
attributesEqual :: Attributes -> Attributes -> Bool
attributesEqual attrs1 attrs2 = attrsAsSet attrs1 == attrsAsSet attrs2
  where
    attrsAsSet = HS.fromList . V.toList
    
sortedAttributesIndices :: Attributes -> [(Int, Attribute)]    
sortedAttributesIndices attrs = L.sortBy (\(_, Attribute name1 _) (_,Attribute name2 _) -> compare name1 name2) $ V.toList (V.indexed attrs)

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
                                                     V.toList sortedTupVec
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

-- | Used to represent the number of tuples in a relation.         
data RelationCardinality = Countable | Finite Int deriving (Eq, Show, Generic, Ord)

-- | Relation variables are identified by their names.
type RelVarName = StringType

type RelationalExpr = RelationalExprBase ()

-- | A relational expression represents query (read) operations on a database.
data RelationalExprBase a =
  --- | Create a relation from tuple expressions.
  MakeRelationFromExprs (Maybe [AttributeExprBase a]) (TupleExprsBase a) |
  --- | Create and reference a relation from attributes and a tuple set.
  MakeStaticRelation Attributes RelationTupleSet |
  --- | Reference an existing relation in Haskell-space.
  ExistingRelation Relation |
  --MakeFunctionalRelation (creates a relation from a tuple-generating function, potentially infinite)
  --in Tutorial D, relational variables pick up the type of the first relation assigned to them
  --relational variables should also be able to be explicitly-typed like in Haskell
  --- | Reference a relation variable by its name.
  RelationVariable RelVarName a |   
  --- | Create a projection over attribute names. (Note that the 'AttributeNames' structure allows for the names to be inverted.)
  Project (AttributeNamesBase a) (RelationalExprBase a) |
  --- | Create a union of two relational expressions. The expressions should have identical attributes.
  Union (RelationalExprBase a) (RelationalExprBase a) |
  --- | Create a join of two relational expressions. The join occurs on attributes which are identical. If the expressions have no overlapping attributes, the join becomes a cross-product of both tuple sets.
  Join (RelationalExprBase a) (RelationalExprBase a)  |
  --- | Rename an attribute (first argument) to another (second argument).
  Rename AttributeName AttributeName (RelationalExprBase a) |
  --- | Return a relation containing all tuples of the first argument which do not appear in the second argument (minus).
  Difference (RelationalExprBase a) (RelationalExprBase a) |
  --- | Create a sub-relation composed of the first argument's attributes which will become an attribute of the result expression. The unreferenced attributes are not altered in the result but duplicate tuples in the projection of the expression minus the attribute names are compressed into one. For more information, <https://github.com/agentm/project-m36/blob/master/docs/introduction_to_the_relational_algebra.markdown#group read the relational algebra tutorial.>
  Group (AttributeNamesBase a) AttributeName (RelationalExprBase a) |
  --- | Create an expression to unwrap a sub-relation contained within at an attribute's name. Note that this is not always an inverse of a group operation.
  Ungroup AttributeName (RelationalExprBase a) |
  --- | Filter the tuples of the relational expression to only retain the tuples which evaluate against the restriction predicate to true.
  Restrict (RestrictionPredicateExprBase a) (RelationalExprBase a) |
  --- | Returns the true relation iff 
  Equals (RelationalExprBase a) (RelationalExprBase a) |
  NotEquals (RelationalExprBase a) (RelationalExprBase a) |
  Extend (ExtendTupleExprBase a) (RelationalExprBase a) |
  --Summarize :: AtomExpr -> AttributeName -> RelationalExpr -> RelationalExpr -> RelationalExpr -- a special case of Extend
  --Evaluate relationalExpr with scoped views
  With [(WithNameExprBase a, RelationalExprBase a)] (RelationalExprBase a)
  deriving (Show, Eq, Generic, NFData, Foldable, Functor, Traversable)
           
instance Binary RelationalExpr

data WithNameExprBase a = WithNameExpr RelVarName a
  deriving (Show, Eq, Generic, NFData, Foldable, Functor, Traversable)

type WithNameExpr = WithNameExprBase ()

instance Binary WithNameExpr

type GraphRefWithNameExpr = WithNameExprBase GraphRefTransactionMarker

type NotificationName = StringType
type Notifications = M.Map NotificationName Notification

-- | When the changeExpr returns a different result in the database context, then the reportExpr is triggered and sent asynchronously to all clients.
data Notification = Notification {
  changeExpr :: RelationalExpr,
  reportOldExpr :: RelationalExpr, --run the expression in the pre-change context
  reportNewExpr :: RelationalExpr --run the expression in the post-change context
  }
  deriving (Show, Eq, Binary, Generic, NFData)

type TypeVarName = StringType
  
-- | Metadata definition for type constructors such as @data Either a b@.
data TypeConstructorDef = ADTypeConstructorDef TypeConstructorName [TypeVarName] |
                          PrimitiveTypeConstructorDef TypeConstructorName AtomType
                        deriving (Show, Generic, Binary, Eq, NFData)
                                 
-- | Found in data constructors and type declarations: Left (Either Int Text) | Right Int
type TypeConstructor = TypeConstructorBase ()
data TypeConstructorBase a = ADTypeConstructor TypeConstructorName [TypeConstructor] |
                         PrimitiveTypeConstructor TypeConstructorName AtomType |
                         RelationAtomTypeConstructor [AttributeExprBase a] |
                         TypeVariable TypeVarName
                     deriving (Show, Generic, Binary, Eq, NFData)
            
type TypeConstructorMapping = [(TypeConstructorDef, DataConstructorDefs)]

type TypeConstructorName = StringType
type TypeConstructorArgName = StringType
type DataConstructorName = StringType
type AtomTypeName = StringType

-- | Used to define a data constructor in a type constructor context such as @Left a | Right b@
data DataConstructorDef = DataConstructorDef DataConstructorName [DataConstructorDefArg] deriving (Eq, Show, Binary, Generic, NFData)

type DataConstructorDefs = [DataConstructorDef]

data DataConstructorDefArg = DataConstructorDefTypeConstructorArg TypeConstructor | 
                             DataConstructorDefTypeVarNameArg TypeVarName
                           deriving (Show, Generic, Binary, Eq, NFData)
                                    
type InclusionDependencies = M.Map IncDepName InclusionDependency
type RelationVariables = M.Map RelVarName GraphRefRelationalExpr

data GraphRefTransactionMarker = TransactionMarker TransactionId |
                                 UncommittedContextMarker
                                 deriving (Eq, Show, Binary, Generic, NFData, Ord)
  
-- a fundamental relational expr to which other relational expressions compile
type GraphRefRelationalExpr = RelationalExprBase GraphRefTransactionMarker

type SchemaName = StringType                         

type Subschemas = M.Map SchemaName Schema

-- | Every transaction has one concrete database context and any number of isomorphic subschemas.
data Schemas = Schemas DatabaseContext Subschemas

-- | The DatabaseContext is a snapshot of a database's evolving state and contains everything a database client can change over time.
-- I spent some time thinking about whether the VirtualDatabaseContext/Schema and DatabaseContext data constructors should be the same constructor, but that would allow relation variables to be created in a "virtual" context which would appear to defeat the isomorphisms of the contexts. It should be possible to switch to an alternative schema to view the same equivalent information without information loss. However, allowing all contexts to reference another context while maintaining its own relation variables, new types, etc. could be interesting from a security perspective. For example, if a user creates a new relvar in a virtual context, then does it necessarily appear in all linked contexts? After deliberation, I think the relvar should appear in *all* linked contexts to retain the isomorphic properties, even when the isomorphism is for a subset of the context. This hints that the IsoMorphs should allow for "fall-through"; that is, when a relvar is not defined in the virtual context (for morphing), then the lookup should fall through to the underlying context.
newtype Schema = Schema SchemaIsomorphs
              deriving (Generic, Binary)
                              
data SchemaIsomorph = IsoRestrict RelVarName RestrictionPredicateExpr (RelVarName, RelVarName) | 
                      IsoRename RelVarName RelVarName |
                      IsoUnion (RelVarName, RelVarName) RestrictionPredicateExpr RelVarName  --maps two relvars to one relvar
                      -- IsoTypeConstructor in morphAttrExpr
                      deriving (Generic, Binary, Show)
                      
type SchemaIsomorphs = [SchemaIsomorph]
                              
data DatabaseContext = DatabaseContext {
  inclusionDependencies :: InclusionDependencies,
  relationVariables :: RelationVariables,
  atomFunctions :: AtomFunctions,
  dbcFunctions :: DatabaseContextFunctions,
  notifications :: Notifications,
  typeConstructorMapping :: TypeConstructorMapping
  } deriving (NFData, Generic)
             
type IncDepName = StringType             

-- | Inclusion dependencies represent every possible database constraint. Constraints enforce specific, arbitrarily-complex rules to which the database context's relation variables must adhere unconditionally.
data InclusionDependency = InclusionDependency RelationalExpr RelationalExpr deriving (Show, Eq, Generic, NFData)

instance Binary InclusionDependency

type AttributeNameAtomExprMap = M.Map AttributeName AtomExpr

--used for returning information about individual expressions
type DatabaseContextExprName = StringType

type DatabaseContextExpr = DatabaseContextExprBase ()

type GraphRefDatabaseContextExpr = DatabaseContextExprBase GraphRefTransactionMarker

instance Binary GraphRefDatabaseContextExpr

-- | Database context expressions modify the database context.
data DatabaseContextExprBase a = 
  NoOperation |
  Define RelVarName [AttributeExprBase a] |
  Undefine RelVarName | --forget existence of relvar X
  Assign RelVarName (RelationalExprBase a) |
  Insert RelVarName (RelationalExprBase a) |
  Delete RelVarName (RestrictionPredicateExprBase a)  |
  Update RelVarName AttributeNameAtomExprMap (RestrictionPredicateExprBase a) |
  
  AddInclusionDependency IncDepName InclusionDependency |
  RemoveInclusionDependency IncDepName |
  
  AddNotification NotificationName RelationalExpr RelationalExpr RelationalExpr |
  RemoveNotification NotificationName |

  AddTypeConstructor TypeConstructorDef [DataConstructorDef] |
  RemoveTypeConstructor TypeConstructorName |

  --adding an AtomFunction is not a pure operation (required loading GHC modules)
  RemoveAtomFunction AtomFunctionName |
  
  RemoveDatabaseContextFunction DatabaseContextFunctionName |
  
  ExecuteDatabaseContextFunction DatabaseContextFunctionName [AtomExprBase a] |
  
  MultipleExpr [DatabaseContextExprBase a]
  deriving (Show, Eq, Generic)

instance Binary DatabaseContextExpr

type ObjModuleName = StringType
type ObjFunctionName = StringType
type Range = (Int,Int)  
-- | Adding an atom function should be nominally a DatabaseExpr except for the fact that it cannot be performed purely. Thus, we create the DatabaseContextIOExpr.
data DatabaseContextIOExprBase a =
  AddAtomFunction AtomFunctionName [TypeConstructor] AtomFunctionBodyScript |
  LoadAtomFunctions ObjModuleName ObjFunctionName FilePath |
  AddDatabaseContextFunction DatabaseContextFunctionName [TypeConstructor] DatabaseContextFunctionBodyScript |
  LoadDatabaseContextFunctions ObjModuleName ObjFunctionName FilePath |
  CreateArbitraryRelation RelVarName [AttributeExprBase a] Range
                           deriving (Show, Eq, Generic, Binary)

type GraphRefDatabaseContextIOExpr = DatabaseContextIOExprBase GraphRefTransactionMarker

type DatabaseContextIOExpr = DatabaseContextIOExprBase ()

type RestrictionPredicateExpr = RestrictionPredicateExprBase ()

type GraphRefRestrictionPredicateExpr = RestrictionPredicateExprBase GraphRefTransactionMarker

-- | Restriction predicates are boolean algebra components which, when composed, indicate whether or not a tuple should be retained during a restriction (filtering) operation.
data RestrictionPredicateExprBase a =
  TruePredicate |
  AndPredicate (RestrictionPredicateExprBase a) (RestrictionPredicateExprBase a) |
  OrPredicate (RestrictionPredicateExprBase a) (RestrictionPredicateExprBase a) |
  NotPredicate (RestrictionPredicateExprBase a)  |
  RelationalExprPredicate (RelationalExprBase a) | --type must be same as true and false relations (no attributes)
  AtomExprPredicate (AtomExprBase a) | --atom must evaluate to boolean
  AttributeEqualityPredicate AttributeName (AtomExprBase a) -- relationalexpr must result in relation with single tuple
  deriving (Show, Eq, Generic, NFData, Foldable, Functor, Traversable)

instance Binary RestrictionPredicateExpr

-- child + parent links
-- | A transaction graph's head name references the leaves of the transaction graph and can be used during session creation to indicate at which point in the graph commits should persist.
type HeadName = StringType

type TransactionHeads = M.Map HeadName Transaction

-- | The transaction graph is the global database's state which references every committed transaction.
data TransactionGraph = TransactionGraph TransactionHeads (S.Set Transaction)

transactionHeadsForGraph :: TransactionGraph -> TransactionHeads
transactionHeadsForGraph (TransactionGraph hs _) = hs

transactionsForGraph :: TransactionGraph -> S.Set Transaction
transactionsForGraph (TransactionGraph _ ts) = ts

-- | Every transaction has context-specific information attached to it.
-- The `TransactionDiff`s represent child/edge relationships to previous transactions (branches or continuations of the same branch).
data TransactionInfo = TransactionInfo {
  parents :: TransactionParents,
  stamp :: UTCTime,
  merkleHash :: MerkleHash
  } deriving (Show, Generic)

type TransactionParents = NE.NonEmpty TransactionId
{-
data TransactionInfo = TransactionInfo TransactionId TransactionDiffs UTCTime | -- 1 parent + n children
                       MergeTransactionInfo TransactionId TransactionId TransactionDiffs UTCTime -- 2 parents, n children
                     deriving (Show, Generic)
-}

instance Binary TransactionInfo                             

-- | Every set of modifications made to the database are atomically committed to the transaction graph as a transaction.
type TransactionId = UUID

data Transaction = Transaction TransactionId TransactionInfo Schemas
                            
-- | The disconnected transaction represents an in-progress workspace used by sessions before changes are committed. This is similar to git's "index". After a transaction is committed, it is "connected" in the transaction graph and can no longer be modified.
data DisconnectedTransaction = DisconnectedTransaction TransactionId Schemas DirtyFlag
--the database context expression represents a difference between the disconnected transaction and its immutable parent transaction- is this diff expr used at all?

type DirtyFlag = Bool

type TransactionDiffExpr = DatabaseContextExpr
                            
transactionId :: Transaction -> TransactionId
transactionId (Transaction tid _ _) = tid

transactionInfo :: Transaction -> TransactionInfo
transactionInfo (Transaction _ info _) = info

instance Eq Transaction where                            
  (Transaction uuidA _ _) == (Transaction uuidB _ _) = uuidA == uuidB
                   
instance Ord Transaction where                            
  compare (Transaction uuidA _ _) (Transaction uuidB _ _) = compare uuidA uuidB

type AtomExpr = AtomExprBase ()

type GraphRefAtomExpr = AtomExprBase GraphRefTransactionMarker

-- | An atom expression represents an action to take when extending a relation or when statically defining a relation or a new tuple.
data AtomExprBase a = AttributeAtomExpr AttributeName |
                      NakedAtomExpr Atom |
                      FunctionAtomExpr AtomFunctionName [AtomExprBase a] a |
                      RelationAtomExpr (RelationalExprBase a) |
                      ConstructedAtomExpr DataConstructorName [AtomExprBase a] a
                    deriving (Eq,Show,Generic, NFData, Foldable, Functor, Traversable)
                       
instance Binary AtomExpr                       

-- | Used in tuple creation when creating a relation.
data ExtendTupleExprBase a = AttributeExtendTupleExpr AttributeName (AtomExprBase a)
                     deriving (Show, Eq, Generic, NFData, Foldable, Functor, Traversable)
                              
type ExtendTupleExpr = ExtendTupleExprBase ()
type GraphRefExtendTupleExpr = ExtendTupleExprBase GraphRefTransactionMarker

instance Binary ExtendTupleExpr

--enumerates the list of functions available to be run as part of tuple expressions           
type AtomFunctions = HS.HashSet AtomFunction

type AtomFunctionName = StringType

type AtomFunctionBodyScript = StringType

type AtomFunctionBodyType = [Atom] -> Either AtomFunctionError Atom

data AtomFunctionBody = AtomFunctionBody (Maybe AtomFunctionBodyScript) AtomFunctionBodyType

instance NFData AtomFunctionBody where
  rnf (AtomFunctionBody mScript _) = rnf mScript
                        
instance Show AtomFunctionBody where
  show (AtomFunctionBody mScript _) = case mScript of
    Just script -> show (unpack script)
    Nothing -> "<compiled>"

-- | An AtomFunction has a name, a type, and a function body to execute when called.
data AtomFunction = AtomFunction {
  atomFuncName :: AtomFunctionName,
  atomFuncType :: [AtomType], 
  atomFuncBody :: AtomFunctionBody
  } deriving (Generic, NFData)
                          
instance Hashable AtomFunction where
  hashWithSalt salt func = salt `hashWithSalt` atomFuncName func
                           
instance Eq AtomFunction where                           
  f1 == f2 = atomFuncName f1 == atomFuncName f2 
  
instance Show AtomFunction where  
  show aFunc = unpack (atomFuncName aFunc) ++ "::" ++ showArgTypes ++ "; " ++ body
   where
     body = show (atomFuncBody aFunc)
     showArgTypes = L.intercalate "->" (map show (atomFuncType aFunc))
     
-- | The 'AttributeNames' structure represents a set of attribute names or the same set of names but inverted in the context of a relational expression. For example, if a relational expression has attributes named "a", "b", and "c", the 'InvertedAttributeNames' of ("a","c") is ("b").
data AttributeNamesBase a = AttributeNames (S.Set AttributeName) |
                            InvertedAttributeNames (S.Set AttributeName) |
                            UnionAttributeNames (AttributeNamesBase a) (AttributeNamesBase a) |
                            IntersectAttributeNames (AttributeNamesBase a) (AttributeNamesBase a) |
                            RelationalExprAttributeNames (RelationalExprBase a) -- use attribute names from the relational expression's type
                      deriving (Eq, Show, Generic, NFData, Foldable, Functor, Traversable)
                               
type AttributeNames = AttributeNamesBase ()

type GraphRefAttributeNames = AttributeNamesBase GraphRefTransactionMarker

instance Binary AttributeNames
                                
-- | The persistence strategy is a global database option which represents how to persist the database in the filesystem, if at all.
data PersistenceStrategy = NoPersistence | -- ^ no filesystem persistence/memory-only database
                           MinimalPersistence FilePath | -- ^ fsync off, not crash-safe
                           CrashSafePersistence FilePath -- ^ full fsync to disk (flushes kernel and physical drive buffers to ensure that the transaction is on non-volatile storage)
                           deriving (Show, Read)
                                    
type AttributeExpr = AttributeExprBase ()
type GraphRefAttributeExpr = AttributeExprBase GraphRefTransactionMarker

-- | Create attributes dynamically.
data AttributeExprBase a = AttributeAndTypeNameExpr AttributeName TypeConstructor a |
                           NakedAttributeExpr Attribute
                         deriving (Eq, Show, Generic, Binary, NFData, Foldable, Functor, Traversable)
                              
-- | Dynamically create a tuple from attribute names and 'AtomExpr's.
newtype TupleExprBase a = TupleExpr (M.Map AttributeName (AtomExprBase a))
                 deriving (Eq, Show, Generic, NFData, Foldable, Functor, Traversable)
                          
instance Binary TupleExpr

type TupleExpr = TupleExprBase ()

type GraphRefTupleExpr = TupleExprBase GraphRefTransactionMarker

data TupleExprsBase a = TupleExprs a [TupleExprBase a]
  deriving (Eq, Show, Generic, NFData, Foldable, Functor, Traversable)

type GraphRefTupleExprs = TupleExprsBase GraphRefTransactionMarker

type TupleExprs = TupleExprsBase ()

instance Binary TupleExprs

data MergeStrategy = 
  -- | After a union merge, the merge transaction is a result of union'ing relvars of the same name, introducing all uniquely-named relvars, union of constraints, union of atom functions, notifications, and types (unless the names and definitions collide, e.g. two types of the same name with different definitions)
  UnionMergeStrategy |
  -- | Similar to a union merge, but, on conflict, prefer the unmerged section (relvar, function, etc.) from the branch named as the argument.
  UnionPreferMergeStrategy HeadName |
  -- | Similar to the our/theirs merge strategy in git, the merge transaction's context is identical to that of the last transaction in the selected branch.
  SelectedBranchMergeStrategy HeadName
                     deriving (Eq, Show, Binary, Generic, NFData)

type DatabaseContextFunctionName = StringType

type DatabaseContextFunctionBodyScript = StringType

type DatabaseContextFunctionBodyType = [Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext

data DatabaseContextFunctionBody = DatabaseContextFunctionBody (Maybe DatabaseContextFunctionBodyScript) DatabaseContextFunctionBodyType 

instance NFData DatabaseContextFunctionBody where
  rnf (DatabaseContextFunctionBody mScript _) = rnf mScript

data DatabaseContextFunction = DatabaseContextFunction {
  dbcFuncName :: DatabaseContextFunctionName,
  dbcFuncType :: [AtomType],
  dbcFuncBody :: DatabaseContextFunctionBody
  } deriving (Generic, NFData)
                               
type DatabaseContextFunctions = HS.HashSet DatabaseContextFunction

instance Hashable DatabaseContextFunction where
  hashWithSalt salt func = salt `hashWithSalt` dbcFuncName func
                           
instance Eq DatabaseContextFunction where                           
  f1 == f2 = dbcFuncName f1 == dbcFuncName f2 

attrTypeVars :: Attribute -> S.Set TypeVarName
attrTypeVars (Attribute _ aType) = case aType of
  IntAtomType -> S.empty
  IntegerAtomType -> S.empty
  DoubleAtomType -> S.empty
  TextAtomType -> S.empty
  DayAtomType -> S.empty
  DateTimeAtomType -> S.empty
  ByteStringAtomType -> S.empty
  BoolAtomType -> S.empty
  (RelationAtomType attrs) -> S.unions (map attrTypeVars (V.toList attrs))
  (ConstructedAtomType _ tvMap) -> M.keysSet tvMap
  (TypeVariableType nam) -> S.singleton nam
  
typeVars :: TypeConstructor -> S.Set TypeVarName
typeVars (PrimitiveTypeConstructor _ _) = S.empty
typeVars (ADTypeConstructor _ args) = S.unions (map typeVars args)
typeVars (TypeVariable v) = S.singleton v
typeVars (RelationAtomTypeConstructor attrExprs) = S.unions (map attrExprTypeVars attrExprs)
    
attrExprTypeVars :: AttributeExprBase a -> S.Set TypeVarName    
attrExprTypeVars (AttributeAndTypeNameExpr _ tCons _) = typeVars tCons
attrExprTypeVars (NakedAttributeExpr attr) = attrTypeVars attr

atomTypeVars :: AtomType -> S.Set TypeVarName
atomTypeVars IntAtomType = S.empty
atomTypeVars IntegerAtomType = S.empty
atomTypeVars DoubleAtomType = S.empty
atomTypeVars TextAtomType = S.empty
atomTypeVars DayAtomType = S.empty
atomTypeVars DateTimeAtomType = S.empty
atomTypeVars ByteStringAtomType = S.empty
atomTypeVars BoolAtomType = S.empty
atomTypeVars (RelationAtomType attrs) = S.unions (map attrTypeVars (V.toList attrs))
atomTypeVars (ConstructedAtomType _ tvMap) = M.keysSet tvMap
atomTypeVars (TypeVariableType nam) = S.singleton nam

unimplemented :: HasCallStack => a
unimplemented = undefined

--for serializing GraphRefRelationalExpr as part of transaction serialization
instance Binary (TupleExprsBase GraphRefTransactionMarker)

instance Binary (TupleExprBase GraphRefTransactionMarker)

instance Binary GraphRefRelationalExpr 

instance Binary (AtomExprBase GraphRefTransactionMarker)

instance Binary (AttributeNamesBase GraphRefTransactionMarker)

instance Binary (RestrictionPredicateExprBase GraphRefTransactionMarker)

instance Binary (ExtendTupleExprBase GraphRefTransactionMarker)

instance Binary GraphRefWithNameExpr
           

