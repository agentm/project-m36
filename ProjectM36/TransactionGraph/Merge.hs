--Transaction Merge Engines
module ProjectM36.TransactionGraph.Merge where
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Set as S
import qualified Data.Map as M
import qualified ProjectM36.TypeConstructorDef as TCD
import ProjectM36.Relation
import Control.Monad (foldM)
import qualified Data.HashSet as HS
import Debug.Trace

data MergePreference = PreferFirst | PreferSecond | PreferNeither

-- Check for overlapping keys. If the values differ, try a preference resolution
unionMergeMaps :: (Ord k, Eq k, Eq a) => MergePreference -> M.Map k a -> M.Map k a -> Either MergeError (M.Map k a)
unionMergeMaps prefer mapA mapB = case prefer of
  PreferFirst -> pure $ M.union mapA mapB
  PreferSecond -> pure $ M.union mapB mapA
  PreferNeither -> if M.intersection mapA mapB == M.intersection mapA mapB then
                     pure $ M.union mapA mapB
                   else
                     Left $ StrategyViolatesComponentMergeError
                     
-- perform the merge even if the attributes are different- is this what we want? Obviously, we need finer-grained merge options.
unionMergeRelation :: MergePreference -> Relation -> Relation -> Either MergeError Relation
unionMergeRelation prefer relA relB = case union relA relB of
    Right unionRel -> pure unionRel
    Left (AttributeNamesMismatchError _) -> preferredRelVar
    Left _ -> Left StrategyViolatesRelationVariableMergeError
    where
      preferredRelVar = case prefer of
        PreferFirst -> Right relA
        PreferSecond -> Right relB
        PreferNeither -> Left StrategyViolatesRelationVariableMergeError

--try to execute unions against the relvars contents -- if a relvar only appears in one context, include it
unionMergeRelVars :: MergePreference -> RelationVariables -> RelationVariables -> Either MergeError RelationVariables
unionMergeRelVars prefer relvarsA relvarsB = do
  let allNames = S.toList (S.union (M.keysSet relvarsA) (M.keysSet relvarsB))
  foldM (\acc name -> do
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
unionMergeAtomFunctions :: MergePreference -> AtomFunctions -> AtomFunctions -> Either MergeError AtomFunctions  
unionMergeAtomFunctions prefer funcsA funcsB = case prefer of
  PreferFirst -> pure $ HS.union funcsA funcsB
  PreferSecond -> pure $ HS.union funcsB funcsA
  PreferNeither -> pure $ HS.union funcsA funcsB
  
unionMergeTypeConstructorMapping :: MergePreference -> TypeConstructorMapping -> TypeConstructorMapping -> Either MergeError TypeConstructorMapping  
unionMergeTypeConstructorMapping prefer typesA typesB = do
  let allFuncNames = S.fromList $ map (\(tc,_) -> TCD.name tc) (typesA ++ typesB)
  foldM (\acc name -> do
            let findType tcm = case filter (\(t,_) -> TCD.name t == name) tcm of
                  [] -> Nothing
                  x:[] -> Just x
                  _ -> error "multiple names matching in TypeConstructorMapping"
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
                                               PreferNeither -> Left StrategyViolatesTypeConstructorMergeError
            ) [] (S.toList allFuncNames)

