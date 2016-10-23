{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.IsomorphicSchema where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import Data.Maybe
import Control.Monad
import Control.Monad.State
import GHC.Generics
import Data.Binary
import qualified Data.Map as M
import qualified Data.Set as S
--import Debug.Trace
-- isomorphic schemas offer bi-directional functors between two schemas

--TODO: note that renaming a relvar should alter any stored isomorphisms as well
--TODO: rel attrs rename or transform (needs bidirectional atom functions)
-- TODO: IsoRestrict should include requirement that union'd relations should retain the same tuple count (no tuples are lost or ambiguous between the two relations)
--TODO: allow morphs to stack (morph a schema to a new schema)
 -- this could be accomplished by morphing the morphs or by chain linking schemas so that they need not directly reference the underlying concrete schema

-- the isomorphic building blocks should not be arbitrarily combined; for example, combing restrict and union on the same target relvar does not make sense as that would create effects at a distance in the secondary schema

data SchemaExpr = AddSubschema SchemaName |
                  AddSubschemaIsomorph SchemaName SchemaIsomorph |
                  RemoveSubschema SchemaName
                  deriving (Generic, Binary)

-- | Return True iff the SchemaIsomorph is fully isomorphic in the sense that it does not represent an isomorphic sub-schema.
isFullyIsomorphic :: SchemaIsomorph -> Bool
isFullyIsomorphic (IsoRestrict _ _ (a, b)) = isJust a && isJust b
isFullyIsomorphic (IsoUnion (_, a) _ _) = isJust a

isomorphs :: Schema -> SchemaIsomorphs
isomorphs (Schema i) = i

-- | Convenience function which renames a relvar in schemaA to another name in schemaB.
isomorphRename :: RelVarName -> RelVarName -> SchemaIsomorph
isomorphRename nameA nameB = IsoRestrict nameA TruePredicate (Just nameB, Nothing)

{- -- useful for transforming a concrete context into a virtual schema and vice versa
invert :: SchemaIsomorph -> SchemaIsomorph
invert (IsoRelVarName nameA nameB) = IsoRelVarName nameB nameA
-}

isomorphInRelVarNames :: SchemaIsomorph -> [RelVarName]
isomorphInRelVarNames (IsoRestrict rv _ _) = [rv]
isomorphInRelVarNames (IsoUnion (rvA, mRvB) _ _) = rvA : maybe [] (\x -> [x]) mRvB

-- | Relation variables names represented in the virtual schema space. Useful for determining if a relvar name is valid in the schema.
isomorphsInRelVarNames :: SchemaIsomorphs -> S.Set RelVarName
isomorphsInRelVarNames morphs = S.fromList (foldr rvnames [] morphs)
  where
    rvnames morph acc = acc ++ isomorphInRelVarNames morph
    
isomorphOutRelVarNames :: SchemaIsomorph -> [RelVarName]    
isomorphOutRelVarNames (IsoRestrict _ _ (mRvA, mRvB)) = catMaybes [mRvA, mRvB]
isomorphOutRelVarNames (IsoUnion _ _ rvA) = [rvA]

isomorphsOutRelVarNames :: SchemaIsomorphs -> S.Set RelVarName
isomorphsOutRelVarNames morphs = S.fromList (foldr rvnames [] morphs)
  where
    rvnames morph acc = acc ++ isomorphOutRelVarNames morph

processRelationalExprInSchema :: Schema -> RelationalExpr -> Either RelationalError RelationalExpr
processRelationalExprInSchema (Schema morphs) relExprIn = do
  let validRelVarNames = isomorphsInRelVarNames morphs
  --validate that all rvs are present in the virtual schema- this prevents relation variables being referenced in the underlying schema (falling through the transformation)
  let processRelExpr rexpr morph = relExprMogrify (\expr -> case expr of
                                                      RelationVariable rv () | S.notMember rv validRelVarNames -> Left (RelVarNotDefinedError rv)
                                                      ex -> relExprMorph morph ex) rexpr
        
  foldM processRelExpr relExprIn morphs
  
processDatabaseContextExprInSchema :: Schema -> DatabaseContextExpr -> Either RelationalError DatabaseContextExpr  
processDatabaseContextExprInSchema schema@(Schema morphs) dbExpr = do
  let processDbExpr expr morph = case expr of
        Assign rv _ | notMember rv -> badRv rv
        Insert rv _ | notMember rv -> badRv rv
        Update rv _ _ | notMember rv -> badRv rv
        MultipleExpr exprs -> MultipleExpr <$> mapM (processDatabaseContextExprInSchema schema) exprs
        e -> databaseContextExprMorph morph relExprMogrifier e
      relExprMogrifier = processRelationalExprInSchema schema
      badRv rv = Left (RelVarNotDefinedError rv)
      notMember rv = S.notMember rv validRelVarNames
      validRelVarNames = isomorphsInRelVarNames morphs
  foldM processDbExpr dbExpr morphs
  
-- re-evaluate- it's not possible to display an incdep that may be for a foreign key to a relvar which is not available in the subschema! 
-- weird compromise: allow inclusion dependencies failures not in the subschema to be propagated- in the worst case, only the inclusion dependency's name is leaked.
  {-
-- | Convert inclusion dependencies for display in a specific schema.
applySchemaToInclusionDependencies :: Schema -> InclusionDependencies -> Either RelationalError InclusionDependencies
applySchemaToInclusionDependencies (Schema morphs) incDeps = 
  let incDepMorph incDep = --check that the mentioned relvars are in fact in the current schema
  M.update incDepMorph incDeps        
  -}
  

-- | Morph a relational expression in one schema to another isomorphic schema.
relExprMorph :: SchemaIsomorph -> (RelationalExpr -> Either RelationalError RelationalExpr)
relExprMorph (IsoRestrict relIn predi split) = \expr -> case expr of 
  RelationVariable rv () | rv == relIn -> case split of
    -- hide the relvarname
    (Nothing, Nothing) -> Left (RelVarNotDefinedError rv)
    --simple relvar rename
    (Just relOutTrue, Nothing) -> Right (Restrict predi (RelationVariable relOutTrue ()))
    --rename but always empty relation- is this useful?
    (Nothing, Just relOutFalse) -> Right (Restrict (NotPredicate predi) (RelationVariable relOutFalse ()))
    (Just relOutTrue, Just relOutFalse) -> Right (Union (RelationVariable relOutTrue ()) (RelationVariable relOutFalse ()))
  orig -> Right orig
relExprMorph (IsoUnion (relInT, mRelInF) predi relTarget) = \expr -> case expr of
  --only the true predicate portion appears in the virtual schema  
  RelationVariable rv () | rv == relInT -> Right (Restrict predi (RelationVariable relTarget ()))

  RelationVariable rv () | Just rv == mRelInF -> Right (Restrict (NotPredicate predi) (RelationVariable relTarget ()))
  orig -> Right orig
  
relExprMogrify :: (RelationalExpr -> Either RelationalError RelationalExpr) -> RelationalExpr -> Either RelationalError RelationalExpr
relExprMogrify func (Project attrs expr) = func expr >>= \ex -> func (Project attrs ex)
relExprMogrify func (Union exprA exprB) = do
  exA <- func exprA
  exB <- func exprB
  func (Union exA exB)
relExprMogrify func (Join exprA exprB) = do
  exA <- func exprA
  exB <- func exprB
  func (Join exA exB)
relExprMogrify func (Rename n1 n2 expr) = func expr >>= \ex -> func (Rename n1 n2 ex)
relExprMogrify func (Difference exprA exprB) = do
  exA <- func exprA
  exB <- func exprB
  func (Difference exA exB)
relExprMogrify func (Group ns n expr) = func expr >>= \ex -> func (Group ns n ex)
relExprMogrify func (Ungroup n expr) = func expr >>= \ex -> func (Ungroup n ex)
relExprMogrify func (Restrict predi expr) = func expr >>= \ex -> func (Restrict predi ex)
relExprMogrify func (Equals exprA exprB) = do
  exA <- func exprA
  exB <- func exprB
  func (Equals exA exB)
relExprMogrify func (NotEquals exprA exprB) = do
  exA <- func exprA
  exB <- func exprB
  func (NotEquals exA exB)
relExprMogrify func (Extend ext expr) = func expr >>= \ex -> func (Extend ext ex)
relExprMogrify func other = func other

{-
spam :: Either RelationalError RelationalExpr
spam = relExprMogrify (relExprMorph (IsoRestrict "emp" TruePredicate (Just "nonboss", Just "boss"))) (RelationVariable "emp" ())

spam2 :: Either RelationalError RelationalExpr
spam2 = relExprMogrify (relExprMorph (IsoUnion ("boss", Just "nonboss") TruePredicate "emp")) (RelationVariable "boss" ()) 
-}

databaseContextExprMorph :: SchemaIsomorph  -> (RelationalExpr -> Either RelationalError RelationalExpr) -> DatabaseContextExpr -> Either RelationalError DatabaseContextExpr
databaseContextExprMorph iso@(IsoRestrict rvIn filt split) relExprFunc expr = case expr of
  Assign rv relExpr | rv == rvIn -> do
    ex <- relExprFunc relExpr
    let trueExpr n = Assign n (Restrict filt ex)
        falseExpr n = Assign n (Restrict (NotPredicate filt) ex)
    case split of
      (Just rvTrue, Just rvFalse) -> pure $ MultipleExpr [trueExpr rvTrue, falseExpr rvFalse]
      (Just rvTrue, Nothing) -> pure (trueExpr rvTrue)
      (Nothing, Just rvFalse) -> pure (falseExpr rvFalse)
      (Nothing, Nothing) -> Left (RelVarNotDefinedError rvIn)
  Insert rv relExpr | rv == rvIn -> do
    ex <- relExprFunc relExpr
    let trueExpr n = Insert n (Restrict filt ex)
        falseExpr n = Insert n (Restrict (NotPredicate filt) ex)
    case split of
      (Just rvTrue, Just rvFalse) -> pure $ MultipleExpr [trueExpr rvTrue, falseExpr rvFalse]
      (Just rvTrue, Nothing) -> pure (trueExpr rvTrue)
      (Nothing, Just rvFalse) -> pure (falseExpr rvFalse)
      (Nothing, Nothing) -> Left (RelVarNotDefinedError rvIn)
  Update rv attrMap predi | rv == rvIn -> do
    -- if the update would "shift" a tuple from the true->false relvar or vice versa, that would be a constraint violation in the virtual schema
    let trueExpr n = Update n attrMap (AndPredicate predi filt)
        falseExpr n = Update n attrMap (AndPredicate predi (NotPredicate filt))
    case split of
      (Just rvTrue, Just rvFalse) -> pure (MultipleExpr [trueExpr rvTrue, falseExpr rvFalse])
      (Just rvTrue, Nothing) -> pure (trueExpr rvTrue)
      (Nothing, Just rvFalse) -> pure (falseExpr rvFalse)
      (Nothing, Nothing) -> Left (RelVarNotDefinedError rvIn)
  MultipleExpr exprs -> MultipleExpr <$> mapM (databaseContextExprMorph iso relExprFunc) exprs
  orig -> pure orig                                    
databaseContextExprMorph iso@(IsoUnion (rvTrue, mRvFalse) filt rvOut) relExprFunc expr = case expr of   
  --assign: replace all instances in the portion of the target relvar with the new tuples from the relExpr
  --problem: between the delete->insert, constraints could be violated which would not otherwise be violated in the "in" schema. This implies that there should be a combo operator which can insert/update/delete in a single pass based on relexpr queries, or perhaps MultipleExpr should be the infamous "comma" operator from TutorialD?
  Assign rv relExpr | rv == rvTrue -> relExprFunc relExpr >>= \ex -> pure $ MultipleExpr [Delete rvOut filt,
                                                                                      Insert rvOut (Restrict filt ex)]
  Assign rv relExpr | Just rv == mRvFalse -> relExprFunc relExpr >>= \ex -> pure $ MultipleExpr [Delete rvOut (NotPredicate filt),            
                                                                  Insert rvOut (Restrict (NotPredicate filt) ex)]
  Insert rv relExpr | rv == rvTrue || Just rv == mRvFalse -> relExprFunc relExpr >>= \ex -> pure $ Insert rvOut ex
  Delete rv delPred | rv == rvTrue -> pure $ Delete rvOut (AndPredicate delPred filt)
  Delete rv delPred | Just rv == mRvFalse -> pure $ Delete rvOut (AndPredicate delPred (NotPredicate filt))
  Update rv attrMap predi | rv == rvTrue -> pure $ Update rvOut attrMap (AndPredicate predi filt)
  Update rv attrMap predi | Just rv == mRvFalse -> pure $ Update rvOut attrMap (AndPredicate (NotPredicate filt) predi)
  orig -> pure orig
  
-- | Apply the isomorphism transformations to the relational expression to convert the relational expression from operating on one schema to a disparate, isomorphic schema.
applyRelationalExprSchemaIsomorphs :: SchemaIsomorphs -> RelationalExpr -> Either RelationalError RelationalExpr
applyRelationalExprSchemaIsomorphs morphs expr = foldM (\expr' morph -> relExprMogrify (relExprMorph morph) expr') expr morphs

inclusionDependenciesMorph :: SchemaIsomorph -> (InclusionDependency -> Either RelationalError InclusionDependency)
inclusionDependenciesMorph iso = \(InclusionDependency subExpr expr) -> InclusionDependency <$> relExprMorph iso subExpr <*> relExprMorph iso expr

applyInclusionDependenciesSchemaIsoMorphs :: SchemaIsomorphs -> InclusionDependencies -> Either RelationalError InclusionDependencies
--add inc deps for restriction with some automatic name
applyInclusionDependenciesSchemaIsoMorphs morphs incDeps = undefined

relationVariablesInSchema :: Schema -> DatabaseContext -> Either RelationalError RelationVariables
relationVariablesInSchema schema@(Schema morphs) context = if null morphs then --main schema
                                                                     pure (relationVariables context)
                                                                   else
                                                                      foldM transform M.empty morphs
  where
    transform newRvMap morph = do
      let rvNames = isomorphInRelVarNames morph
      rvAssocs <- mapM (\rv -> do
                           expr' <- processRelationalExprInSchema schema (RelationVariable rv ())
                           rel <- evalState (evalRelationalExpr expr') context
                           pure (rv, rel)) rvNames
      pure (M.union newRvMap (M.fromList rvAssocs))



{-
proposal
data DatabaseContext = 
Concrete ...|
Virtual Isomorphs
-}
  
applyRelationVariablesSchemaIsomorphs :: SchemaIsomorphs -> RelationVariables -> Either RelationalError RelationVariables                                                                 
applyRelationVariablesSchemaIsomorphs = undefined
  

applySchemaIsomorphsToDatabaseContext :: SchemaIsomorphs -> DatabaseContext -> Either RelationalError DatabaseContext
applySchemaIsomorphsToDatabaseContext morphs context = do
  incdeps <- applyInclusionDependenciesSchemaIsoMorphs morphs (inclusionDependencies context)
  relvars <- applyRelationVariablesSchemaIsomorphs morphs (relationVariables context)
  pure (context { inclusionDependencies = incdeps,
                  relationVariables = relvars
                  --atomFunctions = atomfuncs,
                  --notifications = notifs,
                  --typeConstructorMapping = tconsmapping
                })
    
validate :: SchemaIsomorph -> S.Set RelVarName -> Either RelationalError SchemaIsomorph
validate morph underlyingRvNames = if S.size invalidRvNames > 0 then 
                          Left (MultipleErrors (map RelVarNotDefinedError (S.toList invalidRvNames)))
                         else
                           Right morph
  where
    morphRvNames = S.fromList (isomorphOutRelVarNames morph)
    invalidRvNames = S.difference morphRvNames underlyingRvNames

-- the second argument really should be a database context in the future so we can morph other context items
evalSchemaExpr :: SchemaExpr -> S.Set RelVarName -> Subschemas -> Either RelationalError Subschemas
evalSchemaExpr (AddSubschema sname) _ sschemas = if M.member sname sschemas then
                                                 Left (SubschemaNameInUseError sname)
                                               else 
                                                 pure (M.insert sname (Schema []) sschemas)
evalSchemaExpr (AddSubschemaIsomorph sname morph) underlyingRvNames sschemas = case M.lookup sname sschemas of
  Nothing -> Left (SubschemaNameNotInUseError sname)
  Just (Schema oldMorphs) -> do
    morph' <- validate morph underlyingRvNames
    pure (M.insert sname (Schema (oldMorphs ++ [morph'])) sschemas)
evalSchemaExpr (RemoveSubschema sname) _ sschemas = if M.member sname sschemas then
                                           pure (M.delete sname sschemas)
                                         else
                                           Left (SubschemaNameNotInUseError sname)

