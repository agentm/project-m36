{-# LANGUAGE DeriveGeneric #-}
module ProjectM36.IsomorphicSchema.Types where
import GHC.Generics
import ProjectM36.Base
import qualified Data.Map as M

-- | Every transaction has one concrete database context and any number of isomorphic subschemas.
data Schemas ctx = Schemas ctx Subschemas
  deriving (Generic)

type Subschemas = M.Map SchemaName Schema

type SchemaName = StringType                         

newtype Schema = Schema SchemaIsomorphs
              deriving (Generic)
                              
data SchemaIsomorph = IsoRestrict RelVarName RestrictionPredicateExpr (RelVarName, RelVarName) | 
                      IsoRename RelVarName RelVarName |
                      IsoUnion (RelVarName, RelVarName) RestrictionPredicateExpr RelVarName  --maps two relvars to one relvar
                      -- IsoTypeConstructor in morphAttrExpr
                      deriving (Generic, Show)
                      
type SchemaIsomorphs = [SchemaIsomorph]
