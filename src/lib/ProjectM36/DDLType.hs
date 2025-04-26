module ProjectM36.DDLType where
import ProjectM36.HashSecurely
import ProjectM36.Base
import ProjectM36.RelationalExpression
import ProjectM36.Error
import ProjectM36.Attribute
import qualified Data.Map as M
import ProjectM36.Relation
import ProjectM36.InclusionDependency
import ProjectM36.AtomFunction
import ProjectM36.DatabaseContextFunction
import ProjectM36.IsomorphicSchema
import ProjectM36.DatabaseContext.Types
import ProjectM36.TransactionGraph.Types
import ProjectM36.IsomorphicSchema.Types

-- | Return a hash of just DDL-specific (schema) attributes. This is useful for determining if a client has the appropriate updates needed to work with the current schema. The database context is fully resolved and materialized so that no references to previous transactions exist in the hash calculation.
ddlHash :: DatabaseContext -> TransactionGraph -> Either RelationalError SecureHash
ddlHash ctx tgraph = do
  -- we cannot merely hash the relational representation of the type because the order of items matters when hashing
  -- registered queries are not included here because a client could be compatible with a schema even if the queries are not registered. The client should validate registered query state up-front. Perhaps there should be another hash for registered queries.
  rvtypemap <- typesForRelationVariables ctx tgraph
  resDBContext <- toResolvedDatabaseContext ctx tgraph
  pure $ mkDDLHash resDBContext rvtypemap

-- | Process all relations within the context of the transaction graph to extract the relation variables types.
typesForRelationVariables :: DatabaseContext -> TransactionGraph -> Either RelationalError (M.Map RelVarName Relation)
typesForRelationVariables ctx tgraph = do
  rvs <- resolveDBC' tgraph ctx relationVariables
  let gfEnv = freshGraphRefRelationalExprEnv (Just ctx) tgraph
  M.fromList <$> mapM (\(rvname, rvexpr) -> do
           rvtype <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr rvexpr)
           pure (rvname, rvtype)
                      ) (M.toList rvs)


-- | Return a Relation which represents the database context's current DDL schema.
ddlType :: Schema -> DatabaseContext -> TransactionGraph -> Either RelationalError Relation
ddlType schema ctx tgraph = do
  incDeps <- resolveDBC' tgraph ctx inclusionDependencies
  incDepsRel <- inclusionDependenciesInSchema schema incDeps >>= inclusionDependenciesAsRelation

  atomFuncs <- resolveDBC' tgraph ctx atomFunctions
  atomFuncsRel <- atomFunctionsAsRelation atomFuncs

  dbcFuncs <- resolveDBC' tgraph ctx dbcFunctions
  dbcFuncsRel <- databaseContextFunctionsAsRelation dbcFuncs

  tConsMap <- resolveDBC' tgraph ctx typeConstructorMapping
  typesRel <- typesAsRelation tConsMap
  
  relvarTypesRel <- relationVariablesAsRelationInSchema ctx schema tgraph
  let attrsAssocs = [("inclusion_dependencies", incDepsRel),
                     ("atom_functions", atomFuncsRel),
                     ("database_context_functions", dbcFuncsRel),
                     ("types", typesRel),
                     ("relation_variables", relvarTypesRel)]
      attrs = attributesFromList $ map (\(n, rv) -> Attribute n (RelationAtomType (attributes rv))) attrsAssocs
      tuples = [[RelationAtom incDepsRel,
                 RelationAtom atomFuncsRel,
                 RelationAtom dbcFuncsRel,
                 RelationAtom typesRel,
                 RelationAtom relvarTypesRel]]
  mkRelationFromList attrs tuples

