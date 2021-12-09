{-# LANGUAGE ExistentialQuantification,DeriveGeneric,DeriveAnyClass,FlexibleInstances,OverloadedStrings, DeriveTraversable, DerivingVia, TemplateHaskell, TypeFamilies, DeriveLift, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectM36.Base where
import ProjectM36.DatabaseContextFunctionError
import ProjectM36.AtomFunctionError
import ProjectM36.MerkleHash

import Data.Functor.Foldable.TH
import qualified Data.Map as M
import qualified Data.HashSet as HS
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Set as S
#if MIN_VERSION_uuid_types(1,0,5)
import Data.UUID (UUID)
#else
import Data.UUID.Types.Internal (UUID(..))
#endif
import Control.DeepSeq (NFData, rnf)
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics (Generic)
import GHC.Stack
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Text (Text)
import Data.Time.Clock
import Data.Hashable.Time ()
import Data.Time.Calendar (Day)
import Data.Typeable
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import Data.Vector.Instances ()
import Language.Haskell.TH.Syntax (Lift(..), Exp(..), mkName)
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax (unsafeCodeCoerce)
#elif MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Syntax (unsafeTExpCoerce)
#endif
import Instances.TH.Lift ()
import Data.Void

type StringType = Text

type DatabaseName = String

#if !(MIN_VERSION_hashable(1,3,4))
--support for hashable < 1.3, hashable 1.3+ includes instance for containers
instance Hashable (M.Map TypeVarName AtomType) where 
  hashWithSalt salt tvmap = hashWithSalt salt (M.keys tvmap)

instance Hashable (M.Map AttributeName AtomExpr) where
  hashWithSalt salt m = salt `hashWithSalt` M.toList m

instance Hashable (S.Set AttributeName) where
  hashWithSalt salt s = salt `hashWithSalt` S.toList s
#endif

-- | Database atoms are the smallest, undecomposable units of a tuple. Common examples are integers, text, or unique identity keys.
data AtomBase p = IntegerAtom Integer |
                  IntAtom Int |
                  DoubleAtom Double |
                  TextAtom Text |
                  DayAtom Day |
                  DateTimeAtom UTCTime |
                  ByteStringAtom ByteString |
                  BoolAtom Bool |
                  UUIDAtom UUID |
                  RelationAtom Relation |
                  RelationalExprAtom RelationalExpr | --used for returning inc deps
                  ConstructedAtom DataConstructorName AtomType [Atom] |
                  PlaceholderAtom (AtomBase p)
                deriving (Eq, Show, Typeable, NFData, Generic, Read, Lift)

type Atom = AtomBase Void

-- lift instances missing from th-lift-instances
instance Lift UTCTime where
    lift (UTCTime x y) = do
        x' <- lift x
        y' <- lift y
        return $ AppE (AppE (ConE (mkName "UTCTime")) x') y'
#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = unsafeTExpCoerce . lift
#endif                
        
instance Lift Day where
    lift x = [|toEnum $(lift (fromEnum x))|]
#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = unsafeTExpCoerce . lift
#endif                    

instance Lift DiffTime where
    lift x = [|toEnum $(lift (fromEnum x))|]
#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = unsafeTExpCoerce . lift
#endif

#if MIN_VERSION_uuid_types(1,0,5)
#else
deriving instance Lift UUID
#endif
    
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
  hashWithSalt salt (UUIDAtom u) = salt `hashWithSalt` u
  hashWithSalt salt (RelationAtom r) = salt `hashWithSalt` r
  hashWithSalt salt (RelationalExprAtom re) = salt `hashWithSalt` re

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
                UUIDAtomType |
                RelationAtomType Attributes |
                ConstructedAtomType TypeConstructorName TypeVarMap |
                RelationalExprAtomType |
                TypeVariableType TypeVarName
                --wildcard used in Atom Functions and tuples for data constructors which don't provide all arguments to the type constructor
              deriving (Eq, NFData, Generic, Show, Read, Hashable, Lift)

instance Ord AtomType where
  compare = undefined

-- this should probably be an ordered dictionary in order to be able to round-trip these arguments  
type TypeVarMap = M.Map TypeVarName AtomType

-- | Return True iff the atom type argument is relation-valued. If True, this indicates that the Atom contains a relation.
isRelationAtomType :: AtomType -> Bool
isRelationAtomType (RelationAtomType _) = True
isRelationAtomType _ = False

-- | The AttributeName is the name of an attribute in a relation.
type AttributeName = StringType

-- | A relation's type is composed of attribute names and types.
data Attribute = Attribute AttributeName AtomType deriving (Eq, Show, Read, Generic, NFData, Lift)

instance Hashable Attribute where
  hashWithSalt salt (Attribute attrName _) = hashWithSalt salt attrName

type AttributesHash = Int

-- | 'Attributes' represent the head of a relation.
newtype Attributes = Attributes {
  attributesVec :: V.Vector Attribute
  --,attributesSet :: HS.HashSet Attribute --compare with this generated in heap profile and benchmarks
  }
  deriving (NFData, Read, Hashable, Generic, Lift)

attributesSet :: Attributes -> HS.HashSet Attribute
attributesSet = HS.fromList . V.toList . attributesVec

instance Show Attributes where
  show attrs = "attributesFromList [" <> L.intercalate ", " (map (\attr -> "(" <> show attr <> ")") (V.toList (attributesVec attrs))) <> "]"

--when attribute ordering is irrelevant
instance Eq Attributes where
  attrsA == attrsB =
    attributesVec attrsA == attributesVec attrsB || 
    attributesSet attrsA == attributesSet attrsB

sortedAttributesIndices :: Attributes -> [(Int, Attribute)]    
sortedAttributesIndices attrs = L.sortBy (\(_, Attribute name1 _) (_,Attribute name2 _) -> compare name1 name2) $ V.toList (V.indexed (attributesVec attrs))

-- | The relation's tuple set is the body of the relation.
newtype RelationTupleSet = RelationTupleSet { asList :: [RelationTuple] } deriving (Hashable, Show, Generic, Read, Lift)

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
  hashWithSalt salt (RelationTuple attrs tupVec) = if V.length (attributesVec attrs) /= V.length tupVec then
                                                     error ("invalid tuple: attributes and tuple count mismatch " <> show (attributesVec attrs, tupVec))
                                                   else
                                                     salt `hashWithSalt` 
                                                     sortedAttrs `hashWithSalt`
                                                     V.toList sortedTupVec
    where
      sortedAttrsIndices = sortedAttributesIndices attrs
      sortedAttrs = map snd sortedAttrsIndices
      sortedTupVec = V.map (\(index, _) -> tupVec V.! index) $ V.fromList sortedAttrsIndices
  
-- | A tuple is a set of attributes mapped to their 'Atom' values.
data RelationTuple = RelationTuple Attributes (V.Vector Atom) deriving (Show, Read, Generic, Lift)

instance Eq RelationTuple where
  tuple1@(RelationTuple attrs1 _) == tuple2@(RelationTuple attrs2 _) =
    attrs1 == attrs2 && atomsEqual
    where
      atomForAttribute attr (RelationTuple attrs tupVec) = case V.findIndex (== attr) (attributesVec attrs) of
        Nothing -> Nothing
        Just index -> tupVec V.!? index
      atomsEqual = V.all (== True) $ V.map (\attr -> atomForAttribute attr tuple1 == atomForAttribute attr tuple2) (attributesVec attrs1)

instance NFData RelationTuple where rnf = genericRnf

data Relation = Relation Attributes RelationTupleSet deriving (Show, Generic, Typeable, Lift)

instance Eq Relation where
  Relation attrs1 tupSet1 == Relation attrs2 tupSet2 = attrs1 == attrs2 && tupSet1 == tupSet2

instance NFData Relation where rnf = genericRnf
                               
instance Hashable Relation where                               
  hashWithSalt salt (Relation attrs tupSet) = salt `hashWithSalt` 
                                              sortedAttrs `hashWithSalt`
                                              asList tupSet
    where
      sortedAttrs = map snd (sortedAttributesIndices attrs)
      
-- | Used to represent the number of tuples in a relation.         
data RelationCardinality = Countable | Finite Int deriving (Eq, Show, Generic, Ord)

-- | Relation variables are identified by their names.
type RelVarName = StringType

type RelationalExpr = RelationalExprBase Void Void ()

-- | A relational expression represents query (read) operations on a database.
-- The `marker` represents the position in the transaction graph from whence to evaluate the value (either `()` or `GraphRefTransactionMarker`).
-- The `placeholder` represents expressions which have parameters which can be filled in later.
data RelationalExprBase rvph atph marker =
  --- | Create a relation from tuple expressions.
  MakeRelationFromExprs (Maybe [AttributeExprBase marker]) (TupleExprsBase rvph atph marker) |
  --- | Create and reference a relation from attributes and a tuple set.
  MakeStaticRelation Attributes RelationTupleSet |
  --- | Reference an existing relation in Haskell-space.
  ExistingRelation Relation |
  --MakeFunctionalRelation (creates a relation from a tuple-generating function, potentially infinite)
  --- | Reference a relation variable by its name.
  RelationVariable RelVarName marker |
  --- | Reference a placeholder for use with parameterized queries and anti-quasiquotes.
  RelationVariablePlaceholder rvph marker |
  --- | Create a projection over attribute names. (Note that the 'AttributeNames' structure allows for the names to be inverted.)
  Project (AttributeNamesBase rvph atph marker) (RelationalExprBase rvph atph marker) |
  --- | Create a union of two relational expressions. The expressions should have identical attributes.
  Union (RelationalExprBase rvph atph marker) (RelationalExprBase rvph atph marker) |
  --- | Create a join of two relational expressions. The join occurs on attributes which are identical. If the expressions have no overlapping attributes, the join becomes a cross-product of both tuple sets.
  Join (RelationalExprBase rvph atph marker) (RelationalExprBase rvph atph marker)  |
  --- | Rename an attribute (first argument) to another (second argument).
  Rename AttributeName AttributeName (RelationalExprBase rvph atph marker) |
  --- | Return a relation containing all tuples of the first argument which do not appear in the second argument (minus).
  Difference (RelationalExprBase rvph atph marker) (RelationalExprBase rvph atph marker) |
  --- | Create a sub-relation composed of the first argument's attributes which will become an attribute of the result expression. The unreferenced attributes are not altered in the result but duplicate tuples in the projection of the expression minus the attribute names are compressed into one. For more information, <https://github.com/agentm/project-m36/blob/master/docs/introduction_to_the_relational_algebra.markdown#group read the relational algebra tutorial.>
  Group (AttributeNamesBase rvph atph marker) AttributeName (RelationalExprBase rvph atph marker) |
  --- | Create an expression to unwrap a sub-relation contained within at an attribute's name. Note that this is not always an inverse of a group operation.
  Ungroup AttributeName (RelationalExprBase rvph atph marker) |
  --- | Filter the tuples of the relational expression to only retain the tuples which evaluate against the restriction predicate to true.
  Restrict (RestrictionPredicateExprBase rvph atph marker) (RelationalExprBase rvph atph marker) |
  --- | Returns the true relation iff 
  Equals (RelationalExprBase rvph atph marker) (RelationalExprBase rvph atph marker) |
  NotEquals (RelationalExprBase rvph atph marker) (RelationalExprBase rvph atph marker) |
  Extend (ExtendTupleExprBase rvph atph marker) (RelationalExprBase rvph atph marker) |
  --Summarize :: AtomExpr -> AttributeName -> RelationalExpr -> RelationalExpr -> RelationalExpr -- a special case of Extend
  --Evaluate relationalExpr with scoped views
  With [(WithNameExprBase marker, RelationalExprBase rvph atph marker)] (RelationalExprBase rvph atph marker)
  deriving (Show, Read, Eq, Generic, NFData, Foldable, Functor, Traversable, Lift)

instance Hashable RelationalExpr
    
data WithNameExprBase a = WithNameExpr RelVarName a
  deriving (Show, Read, Eq, Generic, NFData, Foldable, Functor, Traversable, Hashable, Lift)

type WithNameExpr = WithNameExprBase ()

type GraphRefWithNameExpr = WithNameExprBase GraphRefTransactionMarker

type NotificationName = StringType
type Notifications = M.Map NotificationName Notification

-- | When the changeExpr returns a different result in the database context, then the reportExpr is triggered and sent asynchronously to all clients.
data Notification = Notification {
  changeExpr :: RelationalExpr,
  reportOldExpr :: RelationalExpr, --run the expression in the pre-change context
  reportNewExpr :: RelationalExpr --run the expression in the post-change context
  }
  deriving (Show, Eq, Generic, NFData)

type TypeVarName = StringType
  
-- | Metadata definition for type constructors such as @data Either a b@.
data TypeConstructorDef = ADTypeConstructorDef TypeConstructorName [TypeVarName] |
                          PrimitiveTypeConstructorDef TypeConstructorName AtomType
                        deriving (Show, Generic, Eq, NFData, Hashable, Read, Lift)
                                 
-- | Found in data constructors and type declarations: Left (Either Int Text) | Right Int
type TypeConstructor = TypeConstructorBase ()
data TypeConstructorBase a = ADTypeConstructor TypeConstructorName [TypeConstructor] |
                               PrimitiveTypeConstructor TypeConstructorName AtomType |
                               RelationAtomTypeConstructor [AttributeExprBase a] |
                               TypeVariable TypeVarName
                             deriving (Show, Generic, Eq, NFData, Hashable, Read, Lift)
            
type TypeConstructorMapping = [(TypeConstructorDef, DataConstructorDefs)]

type TypeConstructorName = StringType
type TypeConstructorArgName = StringType
type DataConstructorName = StringType
type AtomTypeName = StringType

-- | Used to define a data constructor in a type constructor context such as @Left a | Right b@
data DataConstructorDef = DataConstructorDef DataConstructorName [DataConstructorDefArg] deriving (Eq, Show, Generic, NFData, Hashable, Read, Lift)

type DataConstructorDefs = [DataConstructorDef]

data DataConstructorDefArg = DataConstructorDefTypeConstructorArg TypeConstructor | 
                             DataConstructorDefTypeVarNameArg TypeVarName
                           deriving (Show, Generic, Eq, NFData, Hashable, Read, Lift)
                                    
type InclusionDependencies = M.Map IncDepName InclusionDependency
type RelationVariables = M.Map RelVarName GraphRefRelationalExpr 

data GraphRefTransactionMarker = TransactionMarker TransactionId |
                                 UncommittedContextMarker
                                 deriving (Eq, Show, Generic, NFData, Ord)
  
-- a fundamental relational expr to which other relational expressions compile
type GraphRefRelationalExpr = RelationalExprBase Void Void GraphRefTransactionMarker 

type SchemaName = StringType                         

type Subschemas = M.Map SchemaName Schema

-- | Every transaction has one concrete database context and any number of isomorphic subschemas.
data Schemas = Schemas DatabaseContext Subschemas
  deriving (Generic)

-- | The DatabaseContext is a snapshot of a database's evolving state and contains everything a database client can change over time.
-- I spent some time thinking about whether the VirtualDatabaseContext/Schema and DatabaseContext data constructors should be the same constructor, but that would allow relation variables to be created in a "virtual" context which would appear to defeat the isomorphisms of the contexts. It should be possible to switch to an alternative schema to view the same equivalent information without information loss. However, allowing all contexts to reference another context while maintaining its own relation variables, new types, etc. could be interesting from a security perspective. For example, if a user creates a new relvar in a virtual context, then does it necessarily appear in all linked contexts? After deliberation, I think the relvar should appear in *all* linked contexts to retain the isomorphic properties, even when the isomorphism is for a subset of the context. This hints that the IsoMorphs should allow for "fall-through"; that is, when a relvar is not defined in the virtual context (for morphing), then the lookup should fall through to the underlying context.
newtype Schema = Schema SchemaIsomorphs
              deriving (Generic)
                              
data SchemaIsomorph = IsoRestrict RelVarName RestrictionPredicateExpr (RelVarName, RelVarName) | 
                      IsoRename RelVarName RelVarName |
                      IsoUnion (RelVarName, RelVarName) RestrictionPredicateExpr RelVarName  --maps two relvars to one relvar
                      -- IsoTypeConstructor in morphAttrExpr
                      deriving (Generic, Show)
                      
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
data InclusionDependency = InclusionDependency RelationalExpr RelationalExpr deriving (Show, Eq, Generic, NFData, Hashable, Read, Lift)

type AttributeNameAtomExprMap = M.Map AttributeName AtomExpr

--used for returning information about individual expressions
type DatabaseContextExprName = StringType

type DatabaseContextExpr = DatabaseContextExprBase Void Void ()

instance Hashable DatabaseContextExpr 

type GraphRefDatabaseContextExpr = DatabaseContextExprBase Void GraphRefTransactionMarker 

-- | Database context expressions modify the database context.
data DatabaseContextExprBase rvph atph a = 
  NoOperation |
  Define RelVarName [AttributeExprBase a] |
  Undefine RelVarName | --forget existence of relvar X
  Assign RelVarName (RelationalExprBase rvph atph a) |
  Insert RelVarName (RelationalExprBase rvph atph a) |
  Delete RelVarName (RestrictionPredicateExprBase rvph atph a)  |
  Update RelVarName AttributeNameAtomExprMap (RestrictionPredicateExprBase rvph atph a) |
  
  AddInclusionDependency IncDepName InclusionDependency |
  RemoveInclusionDependency IncDepName |
  
  AddNotification NotificationName RelationalExpr RelationalExpr RelationalExpr |
  RemoveNotification NotificationName |

  AddTypeConstructor TypeConstructorDef [DataConstructorDef] |
  RemoveTypeConstructor TypeConstructorName |

  --adding an AtomFunction is not a pure operation (required loading GHC modules)
  RemoveAtomFunction FunctionName |
  
  RemoveDatabaseContextFunction FunctionName |
  
  ExecuteDatabaseContextFunction FunctionName [AtomExprBase rvph atph a] |
  
  MultipleExpr [DatabaseContextExprBase rvph atph a]
  deriving (Show, Read, Eq, Generic, NFData, Lift, Functor)

type ObjModuleName = StringType
type ObjFunctionName = StringType
type Range = (Int,Int)  
-- | Adding an atom function should be nominally a DatabaseExpr except for the fact that it cannot be performed purely. Thus, we create the DatabaseContextIOExpr.
data DatabaseContextIOExprBase a =
  AddAtomFunction FunctionName [TypeConstructor] FunctionBodyScript |
  LoadAtomFunctions ObjModuleName ObjFunctionName FilePath |
  AddDatabaseContextFunction FunctionName [TypeConstructor] FunctionBodyScript |
  LoadDatabaseContextFunctions ObjModuleName ObjFunctionName FilePath |
  CreateArbitraryRelation RelVarName [AttributeExprBase a] Range
                           deriving (Show, Eq, Generic, Functor)

type GraphRefDatabaseContextIOExpr = DatabaseContextIOExprBase GraphRefTransactionMarker

type DatabaseContextIOExpr = DatabaseContextIOExprBase ()

type RestrictionPredicateExpr = RestrictionPredicateExprBase Void Void ()

instance Hashable RestrictionPredicateExpr

type GraphRefRestrictionPredicateExpr = RestrictionPredicateExprBase Void Void GraphRefTransactionMarker 

-- | Restriction predicates are boolean algebra components which, when composed, indicate whether or not a tuple should be retained during a restriction (filtering) operation.
data RestrictionPredicateExprBase rvph atph a =
  TruePredicate |
  AndPredicate (RestrictionPredicateExprBase rvph atph a) (RestrictionPredicateExprBase rvph atph a) |
  OrPredicate (RestrictionPredicateExprBase rvph atph a) (RestrictionPredicateExprBase rvph atph a) |
  NotPredicate (RestrictionPredicateExprBase rvph atph a)  |
  RelationalExprPredicate (RelationalExprBase rvph atph a) | --type must be same as true and false relations (no attributes)
  AtomExprPredicate (AtomExprBase rvph atph a) | --atom must evaluate to boolean
  AttributeEqualityPredicate AttributeName (AtomExprBase rvph atph a) -- relationalexpr must result in relation with single tuple
  deriving (Show, Read, Eq, Generic, NFData, Foldable, Functor, Traversable, Lift)

-- child + parent links
-- | A transaction graph's head name references the leaves of the transaction graph and can be used during session creation to indicate at which point in the graph commits should persist.
type HeadName = StringType

type TransactionHeads = M.Map HeadName Transaction

-- | The transaction graph is the global database's state which references every committed transaction.
data TransactionGraph = TransactionGraph TransactionHeads (S.Set Transaction)
  deriving Generic

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

-- | Every set of modifications made to the database are atomically committed to the transaction graph as a transaction.
type TransactionId = UUID

data Transaction = Transaction TransactionId TransactionInfo Schemas
  deriving Generic
                            
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

type AtomExpr = AtomExprBase Void Void ()

instance Hashable AtomExpr

type GraphRefAtomExpr = AtomExprBase Void Void GraphRefTransactionMarker 

-- | An atom expression represents an action to take when extending a relation or when statically defining a relation or a new tuple.
data AtomExprBase rvph atph a = AttributeAtomExpr AttributeName |
                        NakedAtomExpr (AtomBase atph) |
                        FunctionAtomExpr FunctionName [AtomExprBase rvph atph a] a |
                        RelationAtomExpr (RelationalExprBase rvph atph a) |
                        ConstructedAtomExpr DataConstructorName [AtomExprBase rvph atph a] a
                      deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable, Lift)
                       
-- | Used in tuple creation when creating a relation.
data ExtendTupleExprBase rvph atph a = AttributeExtendTupleExpr AttributeName (AtomExprBase rvph atph a)
                     deriving (Show, Read, Eq, Generic, NFData, Foldable, Functor, Traversable, Lift)

type ExtendTupleExpr = ExtendTupleExprBase Void Void ()

instance Hashable ExtendTupleExpr
  
type GraphRefExtendTupleExpr = ExtendTupleExprBase Void Void GraphRefTransactionMarker 

--enumerates the list of functions available to be run as part of tuple expressions           
type AtomFunctions = HS.HashSet AtomFunction
type AtomFunctionBodyType = [Atom] -> Either AtomFunctionError Atom
type ObjectFileEntryFunctionName = String

type ObjectFilePath = FilePath

type ObjectModuleName = String

-- | An AtomFunction has a name, a type, and a function body to execute when called.
     
-- | The 'AttributeNames' structure represents a set of attribute names or the same set of names but inverted in the context of a relational expression. For example, if a relational expression has attributes named "a", "b", and "c", the 'InvertedAttributeNames' of ("a","c") is ("b").
data AttributeNamesBase rvph atph a = AttributeNames (S.Set AttributeName) |
                            InvertedAttributeNames (S.Set AttributeName) |
                            UnionAttributeNames (AttributeNamesBase rvph atph a) (AttributeNamesBase rvph atph a) |
                            IntersectAttributeNames (AttributeNamesBase rvph atph a) (AttributeNamesBase rvph atph a) |
                            RelationalExprAttributeNames (RelationalExprBase rvph atph a) -- use attribute names from the relational expression's type
                      deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable, Lift)

type AttributeNames = AttributeNamesBase Void Void ()

instance Hashable AttributeNames

type GraphRefAttributeNames = AttributeNamesBase Void Void GraphRefTransactionMarker 

-- | The persistence strategy is a global database option which represents how to persist the database in the filesystem, if at all.
data PersistenceStrategy = NoPersistence | -- ^ no filesystem persistence/memory-only database
                           MinimalPersistence FilePath | -- ^ fsync off, not crash-safe
                           CrashSafePersistence FilePath -- ^ full fsync to disk (flushes kernel and physical drive buffers to ensure that the transaction is on non-volatile storage)
                           deriving (Show, Read)

persistenceDirectory :: PersistenceStrategy -> Maybe FilePath
persistenceDirectory NoPersistence = Nothing
persistenceDirectory (MinimalPersistence f) = Just f
persistenceDirectory (CrashSafePersistence f) = Just f
                                    
type AttributeExpr = AttributeExprBase ()
type GraphRefAttributeExpr = AttributeExprBase GraphRefTransactionMarker

-- | Create attributes dynamically.
data AttributeExprBase a = AttributeAndTypeNameExpr AttributeName TypeConstructor a |
                           NakedAttributeExpr Attribute
                         deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable, Hashable, Lift)
                              
-- | Dynamically create a tuple from attribute names and 'AtomExpr's.
newtype TupleExprBase rvph atph a = TupleExpr (M.Map AttributeName (AtomExprBase rvph atph a))
                 deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable, Lift)

instance Hashable TupleExpr

type TupleExpr = TupleExprBase Void Void ()

type GraphRefTupleExpr = TupleExprBase Void Void GraphRefTransactionMarker 

data TupleExprsBase rvph atph a = TupleExprs a [TupleExprBase rvph atph a]
  deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable, Lift)

instance Hashable TupleExprs

type GraphRefTupleExprs = TupleExprsBase Void Void GraphRefTransactionMarker 

type TupleExprs = TupleExprsBase Void Void ()

data MergeStrategy = 
  -- | After a union merge, the merge transaction is a result of union'ing relvars of the same name, introducing all uniquely-named relvars, union of constraints, union of atom functions, notifications, and types (unless the names and definitions collide, e.g. two types of the same name with different definitions)
  UnionMergeStrategy |
  -- | Similar to a union merge, but, on conflict, prefer the unmerged section (relvar, function, etc.) from the branch named as the argument.
  UnionPreferMergeStrategy HeadName |
  -- | Similar to the our/theirs merge strategy in git, the merge transaction's context is identical to that of the last transaction in the selected branch.
  SelectedBranchMergeStrategy HeadName
                     deriving (Eq, Show, Generic, NFData)



type DatabaseContextFunctionBodyType = [Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext
type DatabaseContextFunctions = HS.HashSet DatabaseContextFunction

type FunctionName = StringType
type FunctionBodyScript = StringType

-- | Represents stored, user-created or built-in functions which can operates of types such as Atoms or DatabaseContexts.
data Function a = Function {
  funcName :: FunctionName,
  funcType :: [AtomType],
  funcBody :: FunctionBody a
  }
  deriving (Generic, NFData)

instance Eq (Function a) where                           
  f1 == f2 = funcName f1 == funcName f2

instance Hashable (Function a) where
  hashWithSalt salt func = salt `hashWithSalt` funcName func `hashWithSalt` funcType func `hashWithSalt` hashfuncbody 
   where
    hashfuncbody =
      case funcBody func of
        (FunctionScriptBody script _) -> salt `hashWithSalt` script
        (FunctionBuiltInBody _) -> salt
        (FunctionObjectLoadedBody fp modName entryFunc _) -> salt `hashWithSalt` (fp, modName, entryFunc)

data FunctionBody a =
  FunctionScriptBody FunctionBodyScript a |
  FunctionBuiltInBody a |
  FunctionObjectLoadedBody FilePath ObjectModuleName ObjectFileEntryFunctionName a
  deriving Generic

instance NFData a => NFData (FunctionBody a) where
  rnf (FunctionScriptBody script _) = rnf script
  rnf (FunctionBuiltInBody _) = rnf ()
  rnf (FunctionObjectLoadedBody fp mod' entryf _) = rnf (fp, mod', entryf)

type AtomFunction = Function AtomFunctionBodyType
type AtomFunctionBody = FunctionBody AtomFunctionBodyType

type DatabaseContextFunction = Function DatabaseContextFunctionBodyType
type DatabaseContextFunctionBody = FunctionBody DatabaseContextFunctionBodyType

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
  UUIDAtomType -> S.empty
  RelationalExprAtomType -> S.empty
  (RelationAtomType attrs) -> S.unions (map attrTypeVars (V.toList (attributesVec attrs)))
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
atomTypeVars UUIDAtomType = S.empty
atomTypeVars RelationalExprAtomType = S.empty
atomTypeVars (RelationAtomType attrs) = S.unions (map attrTypeVars (V.toList (attributesVec attrs)))
atomTypeVars (ConstructedAtomType _ tvMap) = M.keysSet tvMap
atomTypeVars (TypeVariableType nam) = S.singleton nam

unimplemented :: HasCallStack => a
unimplemented = error "unimplemented"
           
makeBaseFunctor ''RelationalExprBase


data AtomPlaceholder = AtomPlaceholder Atom

type RelationalExprWithPlaceholder = RelationalExprBase AtomPlaceholder ()
