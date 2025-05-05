{-# LANGUAGE CPP #-}
--Transaction Merge Engines
module ProjectM36.TransactionGraph.Merge where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.ValueMarker
import ProjectM36.TransactionGraph.Types
import ProjectM36.RelationalExpression
import ProjectM36.DatabaseContext.Types
#if MIN_VERSION_base(4,18,0)
import Control.Monad (foldM)
import Control.Monad.Except
#else
import Control.Monad.Except hiding (join)
#endif
import qualified Data.Set as S
import qualified Data.Map as M
import qualified ProjectM36.TypeConstructorDef as TCD
import qualified Data.HashSet as HS
import qualified Data.Text as T

data MergePreference = PreferFirst | PreferSecond | PreferNeither

-- Check for overlapping keys. If the values differ, try a preference resolution
unionMergeMaps :: (Ord k, Eq a) => MergePreference -> TransactionGraph -> (DatabaseContext -> ValueMarker (M.Map k a)) -> ValueMarker (M.Map k a) -> ValueMarker (M.Map k a) -> Either RelationalError (ValueMarker (M.Map k a))
unionMergeMaps _prefer _graph _f a@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
      pure a
unionMergeMaps prefer graph f v_mapA v_mapB = do
  mapA <- resolveValueMarker graph f v_mapA
  mapB <- resolveValueMarker graph f v_mapB
  case prefer of
    PreferFirst -> pure (ValueMarker $ M.union mapA mapB)
    PreferSecond -> pure $ ValueMarker $ M.union mapB mapA
    PreferNeither -> if M.intersection mapA mapB == M.intersection mapA mapB then
                       pure $ ValueMarker $ M.union mapA mapB
                     else
                       Left (MergeTransactionError StrategyViolatesComponentMergeError)
                     
-- perform the merge even if the attributes are different- is this what we want? Obviously, we need finer-grained merge options.
unionMergeRelation :: MergePreference -> GraphRefRelationalExpr -> GraphRefRelationalExpr -> GraphRefRelationalExprM GraphRefRelationalExpr
unionMergeRelation prefer relA relB = do
  let unioned = Union relA relB
      mergeErr e = MergeTransactionError (StrategyViolatesRelationVariableMergeError e)
      preferredRelVar =
        case prefer of
          PreferFirst -> pure relA
          PreferSecond -> pure relB
          PreferNeither -> throwError (MergeTransactionError StrategyWithoutPreferredBranchResolutionMergeError)
      handler AttributeNamesMismatchError{} = preferredRelVar
      handler err' = throwError (mergeErr err')
  --typecheck first?
  (evalGraphRefRelationalExpr unioned >> pure (Union relA relB)) `catchError` handler

--try to execute unions against the relvars contents -- if a relvar only appears in one context, include it
unionMergeRelVars ::
  MergePreference ->
  TransactionGraph ->
  ValueMarker RelationVariables ->
  ValueMarker RelationVariables ->
  GraphRefRelationalExprM (ValueMarker RelationVariables)
unionMergeRelVars _prefer _graph v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
      pure v
unionMergeRelVars prefer graph v_relvarsA v_relvarsB = do
  relvarsA <- case resolveValueMarker graph relationVariables v_relvarsA of
       Left err -> throwError err
       Right val -> pure val
  relvarsB <- case resolveValueMarker graph relationVariables v_relvarsB of
       Left err -> throwError err
       Right val -> pure val
  let allNames = S.toList (S.union (M.keysSet relvarsA) (M.keysSet relvarsB))
  ValueMarker <$> foldM (\acc name -> do
            mergedRel <- do
              let findRel = M.lookup name
                  lookupA = findRel relvarsA
                  lookupB = findRel relvarsB
              case (lookupA, lookupB) of
                (Just relA, Just relB) -> do
                  unionMergeRelation prefer relA relB
                (Nothing, Just relB) -> pure relB 
                (Just relA, Nothing) -> pure relA 
                (Nothing, Nothing) -> error "impossible relvar naming lookup"
            pure $ M.insert name mergedRel acc
            ) M.empty allNames

-- if two functions have the same name, ensure that the functions are identical, otherwise, conflict or prefer
--because we don't have a bytecode, there is no way to verify that function bodies are equal, so if the types match up, just choose the first function. This is a serious bug, but intractable until we have a function bytecode.
unionMergeAtomFunctions :: MergePreference -> TransactionGraph -> ValueMarker AtomFunctions -> ValueMarker AtomFunctions -> Either RelationalError (ValueMarker AtomFunctions)
unionMergeAtomFunctions _prefer _g v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
    pure v
unionMergeAtomFunctions prefer graph v_funcsA v_funcsB = do
  funcsA <- resolveValueMarker graph atomFunctions v_funcsA
  funcsB <- resolveValueMarker graph atomFunctions v_funcsB  
  pure $ ValueMarker $
    case prefer of
      PreferFirst -> HS.union funcsA funcsB
      PreferSecond -> HS.union funcsB funcsA
      PreferNeither -> HS.union funcsA funcsB

unionMergeTypeConstructorMapping :: MergePreference -> TransactionGraph -> ValueMarker TypeConstructorMapping -> ValueMarker TypeConstructorMapping -> Either RelationalError (ValueMarker TypeConstructorMapping)
unionMergeTypeConstructorMapping _prefer _graph v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
      pure v
unionMergeTypeConstructorMapping prefer graph v_typesA v_typesB = do
  typesA <- resolveValueMarker graph typeConstructorMapping v_typesA
  typesB <- resolveValueMarker graph typeConstructorMapping v_typesB  
  
  let allFuncNames = S.fromList $ map (\(tc,_) -> TCD.name tc) (typesA ++ typesB)
  val <- foldM (\acc name -> do
            let findType tcm = case filter (\(t,_) -> TCD.name t == name) tcm of
                  [] -> Nothing
                  [x] -> Just x
                  _ -> error $ "multiple names matching in TypeConstructorMapping for " <> T.unpack name
                lookupA = findType typesA
                lookupB = findType typesB
                cat t = pure (t : acc)
            case (lookupA, lookupB) of
               (Nothing, Nothing) -> error "type name lookup failure"
               (Just typeA, Nothing) -> cat typeA
               (Nothing, Just typeB) -> cat typeB
               (Just typeA, Just typeB) -> if typeA == typeB then
                                             cat typeA
                                           else --merge conflict
                                             case prefer of 
                                               PreferFirst -> cat typeA
                                               PreferSecond -> cat typeB
                                               PreferNeither -> Left (MergeTransactionError StrategyViolatesTypeConstructorMergeError)
            ) [] (S.toList allFuncNames)
  pure (ValueMarker val)

unionMergeDatabaseContextFunctions :: MergePreference -> TransactionGraph -> ValueMarker DatabaseContextFunctions -> ValueMarker DatabaseContextFunctions -> Either RelationalError (ValueMarker DatabaseContextFunctions)
unionMergeDatabaseContextFunctions _prefer _graph v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
    pure v
unionMergeDatabaseContextFunctions prefer graph v_funcsA v_funcsB = do
  funcsA <- resolveValueMarker graph dbcFunctions v_funcsA
  funcsB <- resolveValueMarker graph dbcFunctions v_funcsB
  pure $ ValueMarker $
    case prefer of
      PreferFirst -> HS.union funcsA funcsB
      PreferSecond -> HS.union funcsB funcsA
      PreferNeither -> HS.union funcsA funcsB
  
unionMergeRegisteredQueries :: MergePreference -> TransactionGraph -> ValueMarker RegisteredQueries -> ValueMarker RegisteredQueries -> Either RelationalError (ValueMarker RegisteredQueries)
unionMergeRegisteredQueries _prefer _g v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB = pure v
unionMergeRegisteredQueries prefer graph v_regQsA v_regQsB = do
  regQsA <- resolveValueMarker graph registeredQueries v_regQsA
  regQsB <- resolveValueMarker graph registeredQueries v_regQsB
  
  v <- case prefer of
         PreferFirst -> pure (M.union regQsA regQsB)
         PreferSecond -> pure (M.union regQsB regQsA)
         PreferNeither -> do
           let isect = M.filter id $ M.mapWithKey (\qname val -> M.lookup qname regQsB /= Just val) (M.intersection regQsA regQsB)
           --if the values in the intersection are the same, we can merge them      
           if M.null isect then
             pure (M.union regQsA regQsB)
             else
             Left (MergeTransactionError (StrategyViolatesRegisteredQueryMergeError (M.keys isect)))
  pure (ValueMarker v)
        
