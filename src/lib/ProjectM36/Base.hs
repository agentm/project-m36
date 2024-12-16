{-# LANGUAGE ExistentialQuantification,DeriveGeneric,DeriveAnyClass,FlexibleInstances,OverloadedStrings, DeriveTraversable, DerivingVia, TemplateHaskell, TypeFamilies, BangPatterns, DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectM36.Base where
import ProjectM36.AtomFunctionError
import Data.Functor.Foldable.TH
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
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Calendar (Day(..))
import Data.Typeable
import Data.ByteString (ByteString)
import Data.Vector.Instances ()
import Data.Scientific

type StringType = Text

type DatabaseName = String

-- time-compat includes these instances but time-compat is a dependency that is problematic, so just copy the instances here
instance Hashable Day where
    hashWithSalt salt (ModifiedJulianDay d) = hashWithSalt salt d
    
instance Hashable UTCTime where
    hashWithSalt salt (UTCTime d dt) =
        salt `hashWithSalt` d `hashWithSalt` dt

instance Hashable DiffTime where
    hashWithSalt salt = hashWithSalt salt . toRational        

-- | Database atoms are the smallest, undecomposable units of a tuple. Common examples are integers, text, or unique identity keys.
data Atom = IntegerAtom !Integer |
            IntAtom !Int |
            ScientificAtom !Scientific |
            DoubleAtom !Double |
            TextAtom !Text |
            DayAtom !Day |
            DateTimeAtom !UTCTime |
            ByteStringAtom !ByteString |
            BoolAtom !Bool |
            UUIDAtom !UUID |
            RelationAtom !Relation |
            RelationalExprAtom !RelationalExpr | --used for returning inc deps
            SubrelationFoldAtom !Relation !AttributeName |
            ConstructedAtom !DataConstructorName !AtomType [Atom]
            deriving (Eq, Show, Typeable, NFData, Generic, Read)
                     
instance Hashable Atom where                     
  hashWithSalt salt (ConstructedAtom dConsName _ atoms) = salt `hashWithSalt` atoms
                                                          `hashWithSalt` dConsName --AtomType is not hashable
  hashWithSalt salt (IntAtom i) = salt `hashWithSalt` i
  hashWithSalt salt (IntegerAtom i) = salt `hashWithSalt` i
  hashWithSalt salt (ScientificAtom s) = salt `hashWithSalt` s
  hashWithSalt salt (DoubleAtom d) = salt `hashWithSalt` d
  hashWithSalt salt (TextAtom t) = salt `hashWithSalt` t
  hashWithSalt salt (DayAtom d) = salt `hashWithSalt` d
  hashWithSalt salt (DateTimeAtom dt) = salt `hashWithSalt` dt
  hashWithSalt salt (ByteStringAtom bs) = salt `hashWithSalt` bs
  hashWithSalt salt (BoolAtom b) = salt `hashWithSalt` b
  hashWithSalt salt (UUIDAtom u) = salt `hashWithSalt` u
  hashWithSalt salt (RelationAtom r) = salt `hashWithSalt` r
  hashWithSalt salt (RelationalExprAtom re) = salt `hashWithSalt` re
  hashWithSalt salt (SubrelationFoldAtom rel attrName) = salt `hashWithSalt` rel `hashWithSalt` attrName

-- I suspect the definition of ConstructedAtomType with its name alone is insufficient to disambiguate the cases; for example, one could create a type named X, remove a type named X, and re-add it using different constructors. However, as long as requests are served from only one DatabaseContext at-a-time, the type name is unambiguous. This will become a problem for time-travel, however.
-- | The AtomType uniquely identifies the type of a atom.
data AtomType = IntAtomType |
                IntegerAtomType |
                ScientificAtomType |
                DoubleAtomType |
                TextAtomType |
                DayAtomType |
                DateTimeAtomType |
                ByteStringAtomType |
                BoolAtomType |
                UUIDAtomType |
                RelationAtomType Attributes |
                SubrelationFoldAtomType AtomType |
                ConstructedAtomType TypeConstructorName TypeVarMap |
                RelationalExprAtomType |
                TypeVariableType TypeVarName
                --wildcard used in Atom Functions and tuples for data constructors which don't provide all arguments to the type constructor
              deriving (Eq, NFData, Generic, Show, Read, Hashable)

-- this should probably be an ordered dictionary in order to be able to round-trip these arguments  
type TypeVarMap = M.Map TypeVarName AtomType

-- | Return True iff the atom type argument is relation-valued. If True, this indicates that the Atom contains a relation.
isRelationAtomType :: AtomType -> Bool
isRelationAtomType (RelationAtomType _) = True
isRelationAtomType _ = False

-- subrelations sometimes require special paths
attributesContainRelationAtomType :: Attributes -> Bool
attributesContainRelationAtomType attrs = V.null (V.filter (\(Attribute _ t) -> isRelationAtomType t) (attributesVec attrs))

-- | The AttributeName is the name of an attribute in a relation.
type AttributeName = StringType

-- | A relation's type is composed of attribute names and types.
data Attribute = Attribute AttributeName !AtomType deriving (Eq, Show, Read, Generic, NFData)

instance Hashable Attribute where
  hashWithSalt salt (Attribute attrName _) = hashWithSalt salt attrName

type AttributesHash = Int

-- | 'Attributes' represent the head of a relation.
newtype Attributes = Attributes {
  attributesVec :: V.Vector Attribute
  --,attributesSet :: HS.HashSet Attribute --compare with this generated in heap profile and benchmarks
  }
  deriving (NFData, Read, Hashable, Generic)

attributesSet :: Attributes -> HS.HashSet Attribute
attributesSet = HS.fromList . V.toList . attributesVec

instance Show Attributes where
  showsPrec d attrs = showString $ parens $ "attributesFromList [" <> L.intercalate ", " (map (\attr -> "(" <> show attr <> ")") (V.toList (attributesVec attrs))) <> "]"
    where parens x | d > 0 = "(" <> x <> ")"
          parens x = x

--when attribute ordering is irrelevant
instance Eq Attributes where
  attrsA == attrsB =
    attributesVec attrsA == attributesVec attrsB || 
    attributesSet attrsA == attributesSet attrsB

sortedAttributesIndices :: Attributes -> [(Int, Attribute)]    
sortedAttributesIndices attrs = L.sortBy (\(_, Attribute name1 _) (_,Attribute name2 _) -> compare name1 name2) $ V.toList (V.indexed (attributesVec attrs))

-- | The relation's tuple set is the body of the relation.
newtype RelationTupleSet = RelationTupleSet { asList :: [RelationTuple] } deriving (Show, Generic, Read)

-- we cannot derive Hashable for tuplesets; we need to hash the unordered set of tuples
instance Hashable RelationTupleSet where
  hashWithSalt s tupSet = hashWithSalt s (HS.fromList (asList tupSet))
        
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
data RelationTuple = RelationTuple Attributes (V.Vector Atom) deriving (Show, Read, Generic)

instance Eq RelationTuple where
  tuple1@(RelationTuple attrs1 _) == tuple2@(RelationTuple attrs2 _) =
    attrs1 == attrs2 && atomsEqual
    where
      atomForAttribute attr (RelationTuple attrs tupVec) = case V.findIndex (== attr) (attributesVec attrs) of
        Nothing -> Nothing
        Just index -> tupVec V.!? index
      atomsEqual = V.all id $ V.map (\attr -> atomForAttribute attr tuple1 == atomForAttribute attr tuple2) (attributesVec attrs1)

instance NFData RelationTuple where rnf = genericRnf

data Relation = Relation Attributes RelationTupleSet deriving (Show, Generic,Typeable)

instance Eq Relation where
  Relation attrs1 tupSet1 == Relation attrs2 tupSet2 = attrs1 == attrs2 && tupSet1 == tupSet2

instance NFData Relation where rnf = genericRnf
                               
instance Hashable Relation where                               
  hashWithSalt salt (Relation attrs tupSet) = salt `hashWithSalt` 
                                              sortedAttrs `hashWithSalt`
                                              HS.fromList (asList tupSet)
    where
      sortedAttrs = map snd (sortedAttributesIndices attrs)
      
-- | Used to represent the number of tuples in a relation.         
data RelationCardinality = Countable | Finite Int deriving (Eq, Show, Generic, Ord)

-- | Relation variables are identified by their names.
type RelVarName = StringType

type RelationalExpr = RelationalExprBase ()

type RenameAssocs = S.Set (AttributeName, AttributeName)

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
  -- | Extract a relation from an `Atom` that is a nested relation (a relation within a relation).  
  RelationValuedAttribute AttributeName |
  --- | Create a projection over attribute names. (Note that the 'AttributeNames' structure allows for the names to be inverted.)  
  Project (AttributeNamesBase a) (RelationalExprBase a) |
  --- | Create a union of two relational expressions. The expressions should have identical attributes.
  Union (RelationalExprBase a) (RelationalExprBase a) |
  --- | Create a join of two relational expressions. The join occurs on attributes which are identical. If the expressions have no overlapping attributes, the join becomes a cross-product of both tuple sets.
  Join (RelationalExprBase a) (RelationalExprBase a)  |
  --- | Rename an attribute (first argument) to another (second argument).
  Rename RenameAssocs (RelationalExprBase a) | -- should the rename be a Map?
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
  With (WithNamesAssocsBase a) (RelationalExprBase a)
  deriving (Show, Read, Eq, Generic, NFData, Foldable, Functor, Traversable)

instance Hashable RelationalExpr

-- | Used for fixed relational expressions (useful for caching).
type PinnedRelationalExpr = RelationalExprBase TransactionId

instance Hashable PinnedRelationalExpr
    
type TransactionId = UUID

type WithNamesAssocs = WithNamesAssocsBase ()

type WithNamesAssocsBase a = [(WithNameExprBase a, RelationalExprBase a)]

type GraphRefWithNameAssocs = [(GraphRefWithNameExpr, GraphRefRelationalExpr)]

data WithNameExprBase a = WithNameExpr RelVarName a
                        deriving (Show, Read, Eq, Generic, NFData, Foldable, Functor, Traversable, Hashable)

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
                        deriving (Show, Generic, Eq, NFData, Hashable, Read)
                                 
-- | Found in data constructors and type declarations: Left (Either Int Text) | Right Int
type TypeConstructor = TypeConstructorBase ()
data TypeConstructorBase a = ADTypeConstructor TypeConstructorName [TypeConstructor] |
                             PrimitiveTypeConstructor TypeConstructorName AtomType |
                             RelationAtomTypeConstructor [AttributeExprBase a] |
                             TypeVariable TypeVarName
                           deriving (Show, Generic, Eq, NFData, Hashable, Read)
            
type TypeConstructorMapping = [(TypeConstructorDef, DataConstructorDefs)]

type TypeConstructorName = StringType
type TypeConstructorArgName = StringType
type DataConstructorName = StringType
type AtomTypeName = StringType

-- | Used to define a data constructor in a type constructor context such as @Left a | Right b@
data DataConstructorDef = DataConstructorDef DataConstructorName [DataConstructorDefArg] deriving (Eq, Show, Generic, NFData, Hashable, Read)

type DataConstructorDefs = [DataConstructorDef]

data DataConstructorDefArg = DataConstructorDefTypeConstructorArg TypeConstructor | 
                             DataConstructorDefTypeVarNameArg TypeVarName
                           deriving (Show, Generic, Eq, NFData, Hashable, Read)
                                    
type InclusionDependencies = M.Map IncDepName InclusionDependency
type RelationVariables = M.Map RelVarName GraphRefRelationalExpr

data GraphRefTransactionMarker = TransactionMarker TransactionId |
                                 UncommittedContextMarker
                                 deriving (Eq, Show, Generic, NFData, Ord)
  
-- a fundamental relational expr to which other relational expressions compile
type GraphRefRelationalExpr = RelationalExprBase GraphRefTransactionMarker

type HeadName = StringType

type RegisteredQueryName = StringType

type RegisteredQueries = M.Map RegisteredQueryName RelationalExpr
  
type IncDepName = StringType             

-- | Inclusion dependencies represent every possible database constraint. Constraints enforce specific, arbitrarily-complex rules to which the database context's relation variables must adhere unconditionally.
data InclusionDependency = InclusionDependency RelationalExpr RelationalExpr deriving (Show, Eq, Generic, NFData, Hashable, Read)

type AttributeNameAtomExprMap = M.Map AttributeName AtomExpr

--used for returning information about individual expressions
type DatabaseContextExprName = StringType

type DatabaseContextExpr = DatabaseContextExprBase ()

instance Hashable DatabaseContextExpr 

type GraphRefDatabaseContextExpr = DatabaseContextExprBase GraphRefTransactionMarker

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
  RemoveAtomFunction FunctionName |
  
  RemoveDatabaseContextFunction FunctionName |
  
  ExecuteDatabaseContextFunction FunctionName [AtomExprBase a] |

  AddRegisteredQuery RegisteredQueryName RelationalExpr |
  RemoveRegisteredQuery RegisteredQueryName |
  
  MultipleExpr [DatabaseContextExprBase a]
  deriving (Show, Read, Eq, Generic, NFData)

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
                           deriving (Show, Eq, Generic)

type GraphRefDatabaseContextIOExpr = DatabaseContextIOExprBase GraphRefTransactionMarker

type DatabaseContextIOExpr = DatabaseContextIOExprBase ()

type RestrictionPredicateExpr = RestrictionPredicateExprBase ()

instance Hashable RestrictionPredicateExpr

instance Hashable (RestrictionPredicateExprBase TransactionId)

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
  deriving (Show, Read, Eq, Generic, NFData, Foldable, Functor, Traversable)

type DirtyFlag = Bool

type TransactionDiffExpr = DatabaseContextExpr

type AtomExpr = AtomExprBase ()

instance Hashable AtomExpr

instance Hashable (AtomExprBase TransactionId)

type GraphRefAtomExpr = AtomExprBase GraphRefTransactionMarker

type AggAtomFuncExprInfo = (AttributeName, AttributeName) -- (relvar attribute name, subrel attribute name)

-- | An atom expression represents an action to take when extending a relation or when statically defining a relation or a new tuple.
data AtomExprBase a = AttributeAtomExpr AttributeName |
                      SubrelationAttributeAtomExpr AttributeName AttributeName |
                      NakedAtomExpr !Atom |
                      FunctionAtomExpr !FunctionName [AtomExprBase a] a |
                      -- as a simple, first aggregation case, we can only apply an aggregation to a RelationAtom while "selecting" one attribute
                      RelationAtomExpr (RelationalExprBase a) |
                      IfThenAtomExpr (AtomExprBase a) (AtomExprBase a) (AtomExprBase a) | -- if, then, else
                      ConstructedAtomExpr DataConstructorName [AtomExprBase a] a
                    deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable)
                       
-- | Used in tuple creation when creating a relation.
data ExtendTupleExprBase a = AttributeExtendTupleExpr AttributeName (AtomExprBase a)
                     deriving (Show, Read, Eq, Generic, NFData, Foldable, Functor, Traversable)

type ExtendTupleExpr = ExtendTupleExprBase ()

instance Hashable ExtendTupleExpr

instance Hashable (ExtendTupleExprBase TransactionId)
  
type GraphRefExtendTupleExpr = ExtendTupleExprBase GraphRefTransactionMarker

--enumerates the list of functions available to be run as part of tuple expressions           
type AtomFunctions = HS.HashSet AtomFunction
type AtomFunctionBodyType = [Atom] -> Either AtomFunctionError Atom
type ObjectFileEntryFunctionName = String

type ObjectFilePath = FilePath

type ObjectModuleName = String

-- | An AtomFunction has a name, a type, and a function body to execute when called.
     
-- | The 'AttributeNames' structure represents a set of attribute names or the same set of names but inverted in the context of a relational expression. For example, if a relational expression has attributes named "a", "b", and "c", the 'InvertedAttributeNames' of ("a","c") is ("b").
data AttributeNamesBase a = AttributeNames (S.Set AttributeName) |
                            InvertedAttributeNames (S.Set AttributeName) |
                            UnionAttributeNames (AttributeNamesBase a) (AttributeNamesBase a) |
                            IntersectAttributeNames (AttributeNamesBase a) (AttributeNamesBase a) |
                            RelationalExprAttributeNames (RelationalExprBase a) -- use attribute names from the relational expression's type
                      deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable)

type AttributeNames = AttributeNamesBase ()

instance Hashable AttributeNames

instance Hashable (AttributeNamesBase TransactionId)

type GraphRefAttributeNames = AttributeNamesBase GraphRefTransactionMarker

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
                         deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable, Hashable)
                              
-- | Dynamically create a tuple from attribute names and 'AtomExpr's.
newtype TupleExprBase a = TupleExpr (M.Map AttributeName (AtomExprBase a))
                 deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable)

instance Hashable TupleExpr

instance Hashable (TupleExprBase TransactionId)

type TupleExpr = TupleExprBase ()

type GraphRefTupleExpr = TupleExprBase GraphRefTransactionMarker

data TupleExprsBase a = TupleExprs a [TupleExprBase a]
  deriving (Eq, Show, Read, Generic, NFData, Foldable, Functor, Traversable)

instance Hashable TupleExprs

instance Hashable (TupleExprsBase TransactionId)

type GraphRefTupleExprs = TupleExprsBase GraphRefTransactionMarker

type TupleExprs = TupleExprsBase ()

data MergeStrategy = 
  -- | After a union merge, the merge transaction is a result of union'ing relvars of the same name, introducing all uniquely-named relvars, union of constraints, union of atom functions, notifications, and types (unless the names and definitions collide, e.g. two types of the same name with different definitions)
  UnionMergeStrategy |
  -- | Similar to a union merge, but, on conflict, prefer the unmerged section (relvar, function, etc.) from the branch named as the argument.
  UnionPreferMergeStrategy HeadName |
  -- | Similar to the our/theirs merge strategy in git, the merge transaction's context is identical to that of the last transaction in the selected branch.
  SelectedBranchMergeStrategy HeadName
                     deriving (Eq, Show, Generic, NFData)


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

attrTypeVars :: Attribute -> S.Set TypeVarName
attrTypeVars (Attribute _ aType) = case aType of
  IntAtomType -> S.empty
  IntegerAtomType -> S.empty
  ScientificAtomType -> S.empty
  DoubleAtomType -> S.empty
  TextAtomType -> S.empty
  DayAtomType -> S.empty
  DateTimeAtomType -> S.empty
  ByteStringAtomType -> S.empty
  BoolAtomType -> S.empty
  UUIDAtomType -> S.empty
  RelationalExprAtomType -> S.empty
  SubrelationFoldAtomType{} -> S.empty
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
atomTypeVars ScientificAtomType = S.empty
atomTypeVars DoubleAtomType = S.empty
atomTypeVars TextAtomType = S.empty
atomTypeVars DayAtomType = S.empty
atomTypeVars DateTimeAtomType = S.empty
atomTypeVars ByteStringAtomType = S.empty
atomTypeVars BoolAtomType = S.empty
atomTypeVars UUIDAtomType = S.empty
atomTypeVars RelationalExprAtomType = S.empty
atomTypeVars SubrelationFoldAtomType{} = S.empty
atomTypeVars (RelationAtomType attrs) = S.unions (map attrTypeVars (V.toList (attributesVec attrs)))
atomTypeVars (ConstructedAtomType _ tvMap) = M.keysSet tvMap
atomTypeVars (TypeVariableType nam) = S.singleton nam

unimplemented :: HasCallStack => a
unimplemented = error "unimplemented"
           
makeBaseFunctor ''RelationalExprBase


