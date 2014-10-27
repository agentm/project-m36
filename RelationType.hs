module RelationType where
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.Hashable as Hash

data Atom = StringAtom String |
            IntAtom Int |
            RelationAtom Relation deriving (Show, Eq)

data AtomType = StringAtomType |
                IntAtomType |
                RelationAtomType Attributes deriving (Eq, Show)
                                                     
type AttributeName = String
data Attribute = Attribute AttributeName AtomType deriving (Eq, Show)

type Attributes = M.Map AttributeName Attribute --attributes keys by attribute name for ease of access

type RelationTupleSet = HS.HashSet RelationTuple 

instance Hash.Hashable RelationTuple where
  hash tup = Hash.hash $ show tup
    
data RelationTuple = RelationTuple (M.Map AttributeName Atom) deriving (Eq, Show)

data Relation = Relation Attributes RelationTupleSet deriving (Show, Eq)
data RelationCardinality = Uncountable | Countable Int deriving (Eq, Show)
data RelationSizeInfinite = RelationSizeInfinite

data RelationalError = RelationalError Int String deriving (Show,Eq) 
