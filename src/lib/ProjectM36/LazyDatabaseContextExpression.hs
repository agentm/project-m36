module ProjectM36.LazyDatabaseContextExpression where
import ProjectM36.Base
import ProjecM36.DatabaseState
import qualified Data.Set as S

-- this function's goal is to retain maximum laziness (thunks)
-- returns True iff the expression was added to the lazy state, False if the expression requires further evaluation
processThunks :: DatabaseContextExpr -> DatabaseState (Either RelationalError Bool)

processThunks NoOperation = pure True

--Define can be thunked as long as the name isn't in the thunks
processThunks (Define rvName _) = do
  thunks' <- getThunks
  pure (not (S.member rvName (assignedRelVars thunks')))
-- minor optimization opportunity if exprs are Define x, Undefine x, ... Define x, we could successfully retain laziness
  
processThunks (Undefine rvName) = do
  --validate that the relvar name does appear previously defined
              
  
--returns all relvar names which are being assigned in the expr  
assignedRelVars :: DatabaseContextExpr -> S.Set RelVarName
assignedRelVars expr = case expr of
  NoOperation -> S.empty
  Define rv _ -> S.singleton rv
  Undefine rv -> S.singleton rv
  Assign rv _ -> S.singleton rv
  Insert rv _ -> S.singleton rv
  Delete rv _ -> S.singleton rv
  Update rv _ _ -> S.singleton rv
  AddInclusionDependency _ _ -> S.empty
  RemoveInclusionDependency _ -> S.empty
  AddTypeConstructor _ _ -> S.empty
  RemoveTypeConstructor _ -> S.empty
  RemoveAtomFunction _ _ -> S.empty
  MultipleExpr exprs = foldr (\e acc -> S.union acc (assignedRelVars e)) S.empty exprs
  
  
  
  
  
  