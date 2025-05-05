{-# LANGUAGE DeriveGeneric #-}
module ProjectM36.IsomorphicSchema.Types where
import GHC.Generics
import ProjectM36.Base
import ProjectM36.ValueMarker
import qualified Data.Map as M

-- | Every transaction has one concrete database context and any number of isomorphic subschemas.
data Schemas ctx = Schemas {
  concreteDatabaseContext :: ctx,
  subschemas :: ValueMarker Subschemas
  }
  deriving (Generic)

type Subschemas = M.Map SchemaName Schema

type SchemaName = StringType                         

newtype Schema = Schema SchemaIsomorphs
              deriving (Generic, Eq)
                              
data SchemaIsomorph = IsoRestrict RelVarName RestrictionPredicateExpr (RelVarName, RelVarName) | 
                      IsoRename RelVarName RelVarName |
                      IsoUnion (RelVarName, RelVarName) RestrictionPredicateExpr RelVarName  --maps two relvars to one relvar
                      -- IsoTypeConstructor in morphAttrExpr
                      deriving (Generic, Show, Eq)
                      
type SchemaIsomorphs = [SchemaIsomorph]
