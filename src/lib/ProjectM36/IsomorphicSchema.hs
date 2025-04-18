{-# LANGUAGE DeriveGeneric, LambdaCase, DerivingVia, FlexibleInstances #-}
module ProjectM36.IsomorphicSchema where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.MiscUtils
import ProjectM36.Relation
import ProjectM36.NormalizeExpr
import ProjectM36.DatabaseContext.Types
import ProjectM36.TransactionGraph.Types
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.RelationalExpression
import qualified ProjectM36.AttributeNames as AN
import Control.Monad
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector as V
import qualified ProjectM36.Attribute as A
import ProjectM36.AtomType
import Data.Text (Text)
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif

-- isomorphic schemas offer bi-directional functors between two schemas

--TODO: note that renaming a relvar should alter any stored isomorphisms as well
--TODO: rel attrs rename or transform (needs bidirectional atom functions)
-- TODO: IsoRestrict should include requirement that union'd relations should retain the same tuple count (no tuples are lost or ambiguous between the two relations)
--TODO: allow morphs to stack (morph a schema to a new schema)
 -- this could be accomplished by morphing the morphs or by chain linking schemas so that they need not directly reference the underlying concrete schema

-- the isomorphic building blocks should not be arbitrarily combined; for example, combing restrict and union on the same target relvar does not make sense as that would create effects at a distance in the secondary schema

data SchemaExpr = AddSubschema SchemaName SchemaIsomorphs |
                  RemoveSubschema SchemaName
                  deriving (Generic, Show)

isomorphs :: Schema -> SchemaIsomorphs
isomorphs (Schema i) = i

-- | Return an error if the schema is not isomorphic to the base database context.
-- A schema is fully isomorphic iff all relvars in the base context are in the "out" relvars, but only once.
--TODO: add relvar must appear exactly once constraint
validateSchema :: Schema -> S.Set RelVarName -> Maybe SchemaError
validateSchema potentialSchema expectedRelVars
  | not (S.null rvDiff) = Just (RelVarReferencesMissing rvDiff)
  | otherwise = case outDupes of
      x : _ -> Just $ RelVarOutReferencedMoreThanOnce x
      _ -> case inDupes of
             [] -> Nothing
             x : _ -> Just $ RelVarInReferencedMoreThanOnce x
  where
    --check that the predicate for IsoUnion and IsoRestrict holds right now
    outDupes = duplicateNames (namesList isomorphOutRelVarNames)
    inDupes = duplicateNames (namesList isomorphInRelVarNames)
    duplicateNames = dupes . L.sort
    namesList isoFunc = concatMap isoFunc (isomorphs potentialSchema)
    schemaRelVars = isomorphsOutRelVarNames (isomorphs potentialSchema)
    rvDiff = S.difference expectedRelVars schemaRelVars

-- useful for transforming a concrete context into a virtual schema and vice versa
invert :: SchemaIsomorph -> SchemaIsomorph
invert (IsoRename rvIn rvOut) = IsoRename rvOut rvIn
invert (IsoRestrict rvIn predi (rvAOut, rvBOut)) = IsoUnion (rvAOut, rvBOut) predi rvIn
invert (IsoUnion (rvAIn, rvBIn) predi rvOut) = IsoRestrict rvOut predi (rvAIn, rvBIn)

isomorphInRelVarNames :: SchemaIsomorph -> [RelVarName]
isomorphInRelVarNames (IsoRestrict rv _ _) = [rv]
isomorphInRelVarNames (IsoUnion (rvA, rvB) _ _) = [rvA, rvB]
isomorphInRelVarNames (IsoRename rv _) = [rv]

-- | Relation variables names represented in the virtual schema space. Useful for determining if a relvar name is valid in the schema.
isomorphsInRelVarNames :: SchemaIsomorphs -> S.Set RelVarName
isomorphsInRelVarNames morphs = S.fromList (foldr rvnames [] morphs)
  where
    rvnames morph acc = acc ++ isomorphInRelVarNames morph

isomorphOutRelVarNames :: SchemaIsomorph -> [RelVarName]
isomorphOutRelVarNames (IsoRestrict _ _ (rvA, rvB)) = [rvA, rvB]
isomorphOutRelVarNames (IsoUnion _ _ rv) = [rv]
isomorphOutRelVarNames (IsoRename _ rv) = [rv]

isomorphsOutRelVarNames :: SchemaIsomorphs -> S.Set RelVarName
isomorphsOutRelVarNames morphs = S.fromList (foldr rvnames [] morphs)
  where
    rvnames morph acc = acc ++ isomorphOutRelVarNames morph

-- | Check that all mentioned relvars are actually present in the current schema.
validateRelationalExprInSchema :: Schema -> RelationalExpr -> Either RelationalError ()
validateRelationalExprInSchema schema relExprIn =
  void $ relExprMogrify (\case
                            RelationVariable rv () | S.notMember rv validRelVarNames -> Left (RelVarNotDefinedError rv)
                            ex -> Right ex) relExprIn
  where
    validRelVarNames = isomorphsInRelVarNames (isomorphs schema)

processRelationalExprInSchema :: Schema -> RelationalExpr -> Either RelationalError RelationalExpr
processRelationalExprInSchema (Schema []) expr = pure expr
processRelationalExprInSchema schema relExprIn = do
  --validate that all rvs are present in the virtual schema- this prevents relation variables being referenced in the underlying schema (falling through the transformation)
  let processRelExpr rexpr morph = relExprMogrify (relExprMorph morph) rexpr
  validateRelationalExprInSchema schema relExprIn
  foldM processRelExpr relExprIn (isomorphs schema)

validateDatabaseContextExprInSchema :: Schema -> DatabaseContextExpr -> Either RelationalError ()
validateDatabaseContextExprInSchema schema dbExpr = mapM_ (\morph -> databaseContextExprMorph morph (\e -> validateRelationalExprInSchema schema e >> pure e) dbExpr) (isomorphs schema)

processDatabaseContextExprInSchema :: Schema -> DatabaseContextExpr -> Either RelationalError DatabaseContextExpr
processDatabaseContextExprInSchema schema@(Schema morphs) dbExpr = do
  let relExprMogrifier = processRelationalExprInSchema schema
  --validate that all mentioned relvars are in the valid set
  _ <- validateDatabaseContextExprInSchema schema dbExpr
  --perform the morph
  foldM (\ex morph -> databaseContextExprMorph morph relExprMogrifier ex) dbExpr morphs

-- | If the database context expression adds or removes a relvar, we need to update the isomorphs to create a passthrough Isomorph.
processDatabaseContextExprSchemaUpdate :: Schema -> DatabaseContextExpr -> Schema
processDatabaseContextExprSchemaUpdate schema@(Schema morphs) expr = case expr of
  Define rv _ | S.notMember rv validSchemaName -> passthru rv
  Assign rv _ | S.notMember rv validSchemaName -> passthru rv
  Undefine rv | S.member rv validSchemaName -> Schema (filter (elem rv . isomorphInRelVarNames) morphs)
  MultipleExpr exprs -> foldr (flip processDatabaseContextExprSchemaUpdate) schema exprs
  _ -> schema
  where
    validSchemaName = isomorphsInRelVarNames morphs
    passthru rvname = Schema (morphs ++ [IsoRename rvname rvname])

processDatabaseContextExprSchemasUpdate :: Subschemas -> DatabaseContextExpr -> Subschemas
processDatabaseContextExprSchemasUpdate subschemas expr = M.map (`processDatabaseContextExprSchemaUpdate` expr) subschemas

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
-- Returns a function which can be used to morph a 'GraphRefRelationalExpr'. Here, we naively apply the morphs in the current context ignoring past contexts because:
-- * the current schema may not exist in past
-- * this function should only be used for showing DDL, not for expression evaluation.
-- * if a schema were renamed, then the path to past isomorphisms in the transaction graph tree would be lost.
relExprMorph :: SchemaIsomorph -> RelationalExpr -> Either RelationalError RelationalExpr
relExprMorph (IsoRestrict relIn _ (relOutTrue, relOutFalse)) = \case
  RelationVariable rv m | rv == relIn -> Right (Union (RelationVariable relOutTrue m) (RelationVariable relOutFalse m))
  orig -> Right orig
relExprMorph (IsoUnion (relInT, relInF) predi relTarget) = \case
  --only the true predicate portion appears in the virtual schema
  RelationVariable rv m | rv == relInT -> Right (Restrict predi (RelationVariable relTarget m))

  RelationVariable rv m | rv == relInF -> Right (Restrict (NotPredicate predi) (RelationVariable relTarget m))
  orig -> Right orig
relExprMorph (IsoRename relIn relOut) = \case
  RelationVariable rv m | rv == relIn -> Right (RelationVariable relOut m)
  orig -> Right orig

relExprMogrify :: (RelationalExprBase a -> Either RelationalError (RelationalExprBase a)) -> RelationalExprBase a -> Either RelationalError (RelationalExprBase a)
relExprMogrify func (Project attrs expr) = func expr >>= \ex -> func (Project attrs ex)
relExprMogrify func (Union exprA exprB) = do
  exA <- func exprA
  exB <- func exprB
  func (Union exA exB)
relExprMogrify func (Join exprA exprB) = do
  exA <- func exprA
  exB <- func exprB
  func (Join exA exB)
relExprMogrify func (Rename attrs expr) = func expr >>= \ex -> func (Rename attrs ex)
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
databaseContextExprMorph iso'@(IsoRestrict rvIn filt (rvTrue, rvFalse)) relExprFunc expr = case expr of
  Assign rv relExpr | rv == rvIn -> do
    ex <- relExprFunc relExpr
    let trueExpr n = Assign n (Restrict filt ex)
        falseExpr n = Assign n (Restrict (NotPredicate filt) ex)
    pure $ MultipleExpr [trueExpr rvTrue, falseExpr rvFalse]
  Insert rv relExpr | rv == rvIn -> do
    ex <- relExprFunc relExpr
    let trueExpr n = Insert n (Restrict filt ex)
        falseExpr n = Insert n (Restrict (NotPredicate filt) ex)
    pure $ MultipleExpr [trueExpr rvTrue, falseExpr rvFalse]
  Update rv attrMap predi | rv == rvIn -> do
    -- if the update would "shift" a tuple from the true->false relvar or vice versa, that would be a constraint violation in the virtual schema
    let trueExpr n = Update n attrMap (AndPredicate predi filt)
        falseExpr n = Update n attrMap (AndPredicate predi (NotPredicate filt))
    pure (MultipleExpr [trueExpr rvTrue, falseExpr rvFalse])
  MultipleExpr exprs -> MultipleExpr <$> mapM (databaseContextExprMorph iso' relExprFunc) exprs
  orig -> pure orig
databaseContextExprMorph iso'@(IsoUnion (rvTrue, rvFalse) filt rvOut) relExprFunc expr = case expr of
  --assign: replace all instances in the portion of the target relvar with the new tuples from the relExpr
  --problem: between the delete->insert, constraints could be violated which would not otherwise be violated in the "in" schema. This implies that there should be a combo operator which can insert/update/delete in a single pass based on relexpr queries, or perhaps MultipleExpr should be the infamous "comma" operator from TutorialD?
  -- if any tuples are filtered out of the insert/assign, we need to simulate a constraint violation
  Assign rv relExpr | rv == rvTrue -> relExprFunc relExpr >>= \ex -> pure $ MultipleExpr [Delete rvOut filt,
                                                                                      Insert rvOut (Restrict filt ex)]
  Assign rv relExpr | rv == rvFalse -> relExprFunc relExpr >>= \ex -> pure $ MultipleExpr [Delete rvOut (NotPredicate filt),
                                                                                           Insert rvOut (Restrict (NotPredicate filt) ex)]
  Insert rv relExpr | rv == rvTrue || rv == rvFalse -> relExprFunc relExpr >>= \ex -> pure $ Insert rvOut ex
  Delete rv delPred | rv == rvTrue -> pure $ Delete rvOut (AndPredicate delPred filt)
  Delete rv delPred | rv == rvFalse -> pure $ Delete rvOut (AndPredicate delPred (NotPredicate filt))
  Update rv attrMap predi | rv == rvTrue -> pure $ Update rvOut attrMap (AndPredicate predi filt)
  Update rv attrMap predi | rv == rvFalse -> pure $ Update rvOut attrMap (AndPredicate (NotPredicate filt) predi)
  MultipleExpr exprs -> MultipleExpr <$> mapM (databaseContextExprMorph iso' relExprFunc) exprs
  orig -> pure orig
databaseContextExprMorph iso'@(IsoRename relIn relOut) relExprFunc expr = case expr of
  Assign rv relExpr | rv == relIn -> relExprFunc relExpr >>= \ex -> pure (Assign relOut ex)
  Insert rv relExpr | rv == relIn -> relExprFunc relExpr >>= \ex -> pure $ Insert relOut ex
  Delete rv delPred | rv == relIn -> pure $ Delete relOut delPred
  Update rv attrMap predi | rv == relIn -> pure $ Update relOut attrMap predi
  MultipleExpr exprs -> MultipleExpr <$> mapM (databaseContextExprMorph iso' relExprFunc) exprs
  orig -> pure orig

-- | Apply the isomorphism transformations to the relational expression to convert the relational expression from operating on one schema to a disparate, isomorphic schema.
applyRelationalExprSchemaIsomorphs :: SchemaIsomorphs -> RelationalExpr -> Either RelationalError RelationalExpr
applyRelationalExprSchemaIsomorphs morphs expr = foldM (\expr' morph -> relExprMogrify (relExprMorph morph) expr') expr morphs

-- the morph must be applied in the opposite direction
--algorithm: create a relexpr for each relvar in the schema, then replace those rel exprs wherever they appear in the inc dep relexprs
-- x = x1 union x2
inclusionDependencyInSchema :: Schema -> InclusionDependency -> Either RelationalError InclusionDependency
inclusionDependencyInSchema schema (InclusionDependency rexprA rexprB) = do
  --collect all relvars which appear in the schema
  let schemaRelVars = isomorphsInRelVarNames (isomorphs schema)
  rvAssoc <- mapM (\rvIn -> do
                      rvOut <- processRelationalExprInSchema schema (RelationVariable rvIn ())
                      pure (rvOut, RelationVariable rvIn ())
                  )
             (S.toList schemaRelVars)
  let replacer exprOrig = foldM (\expr (find, replace) -> if expr == find then
                                                            pure replace
                                                          else
                                                            pure expr) exprOrig rvAssoc
  rexprA' <- relExprMogrify replacer rexprA
  rexprB' <- relExprMogrify replacer rexprB
  pure (InclusionDependency rexprA' rexprB')

-- #55 add two virtual constraints for IsoUnion and enforce them before the tuples disappear
-- this is needed to
-- also, it's inverse to IsoRestrict which adds two constraints at the base level
-- for IsoRestrict, consider hiding the two, generated constraints since they can never be thrown in the isomorphic schema
inclusionDependenciesInSchema :: Schema -> InclusionDependencies -> Either RelationalError InclusionDependencies
inclusionDependenciesInSchema schema incDeps = M.fromList <$> mapM (\(depName, dep) -> inclusionDependencyInSchema schema dep >>= \newDep -> pure (depName, newDep)) (M.toList incDeps)

relationVariablesInSchema :: Schema -> Either RelationalError RelationVariables
relationVariablesInSchema schema@(Schema morphs) = foldM transform M.empty morphs
  where
    transform newRvMap morph = do
      let rvNames = isomorphInRelVarNames morph
      rvAssocs <- mapM (\rv -> do
                           expr' <- processRelationalExprInSchema schema (RelationVariable rv ())
                           let gfExpr = runProcessExprM UncommittedContextMarker (processRelationalExpr expr')
                           pure (rv, gfExpr)) rvNames
      pure (M.union newRvMap (M.fromList rvAssocs))


-- | Show metadata about the relation variables in the isomorphic schema.
relationVariablesAsRelationInSchema :: DatabaseContext -> Schema -> TransactionGraph -> Either RelationalError Relation
relationVariablesAsRelationInSchema ctx (Schema []) graph = relationVariablesAsRelation ctx graph -- no schema morphism
relationVariablesAsRelationInSchema concreteDbContext schema graph = do
  rvDefsInConcreteSchema <- relationVariablesInSchema schema
  let gfEnv = freshGraphRefRelationalExprEnv (Just concreteDbContext) graph
  typAssocs <- forM (M.toList rvDefsInConcreteSchema) $ \(rv, gfExpr) -> do
    typ <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr)
    pure (rv, typ)
  let tups = map relVarToAtomList typAssocs
      subrelAttrs = A.attributesFromList [Attribute "attribute" TextAtomType, Attribute "type" TextAtomType]
      attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                  Attribute "attributes" (RelationAtomType subrelAttrs)]
      relVarToAtomList (rvName, rel) = [TextAtom rvName, attributesToRel (attributesVec (attributes rel))]
      attrAtoms a = [TextAtom (A.attributeName a), TextAtom (prettyAtomType (A.atomType a))]
      attributesToRel attrl = case mkRelationFromList subrelAttrs (map attrAtoms (V.toList attrl)) of
        Left err -> error ("relationVariablesAsRelation pooped " ++ show err)
        Right rel -> RelationAtom rel
  mkRelationFromList attrs tups

{-
proposal
data DatabaseContext =
Concrete ...|
Virtual Isomorphs
-}
{-
applyRelationVariablesSchemaIsomorphs :: SchemaIsomorphs -> RelationVariables -> Either RelationalError RelationVariables
applyRelationVariablesSchemaIsomorphs {-morphs rvs -}= undefined
-}
{-  M.fromList <$> mapM (\(rvname, rvexpr) -> do
                          morphed <- applyRelationalExprSchemaIsomorphs morphs rvexpr
                          pure (rvname, morphed)
                      ) (M.toList rvs)
  -}
{-
applySchemaIsomorphsToDatabaseContext :: SchemaIsomorphs -> DatabaseContext -> Either RelationalError DatabaseContext
applySchemaIsomorphsToDatabaseContext morphs context = do
--  incdeps <- inclusionDependen morphs (inclusionDependencies context)
  relvars <- applyRelationVariablesSchemaIsomorphs morphs (relationVariables context)
  pure (context { --inclusionDependencies = incdeps,
                  relationVariables = relvars
                  --atomFunctions = atomfuncs,
                  --notifications = notifs,
                  --typeConstructorMapping = tconsmapping
                })
  -}
{-
validate :: SchemaIsomorph -> S.Set RelVarName -> Either RelationalError SchemaIsomorph
validate morph underlyingRvNames = if S.size invalidRvNames > 0 then
                          Left (MultipleErrors (map RelVarNotDefinedError (S.toList invalidRvNames)))
                         else
                           Right morph
  where
    morphRvNames = S.fromList (isomorphOutRelVarNames morph)
    invalidRvNames = S.difference morphRvNames underlyingRvNames
-}

-- | Create inclusion dependencies mainly for IsoRestrict because the predicate should hold in the base schema.
createIncDepsForIsomorph :: SchemaName -> SchemaIsomorph -> InclusionDependencies
createIncDepsForIsomorph sname (IsoRestrict origRv predi (rvTrue, rvFalse)) = let
  newIncDep predicate rv = InclusionDependency (Project AN.empty (Restrict predicate (RelationVariable rv ()))) (ExistingRelation relationTrue)
  incDepName b = "schema" <> "_" <> sname <> "_" <> b in
  M.fromList [(incDepName (origRv <> "_true"), newIncDep predi rvTrue),
              (incDepName (origRv <> "_false"), newIncDep (NotPredicate predi) rvFalse)]
createIncDepsForIsomorph _ _ = M.empty

-- in the case of IsoRestrict, the database context should be updated with the restriction so that if the restriction does not hold, then the schema cannot be created
evalSchemaExpr :: SchemaExpr -> DatabaseContext -> TransactionId -> TransactionGraph -> Subschemas -> Either RelationalError (Subschemas, DatabaseContext)
evalSchemaExpr (AddSubschema sname morphs) context transId graph sschemas =
  if M.member sname sschemas then
    Left (SubschemaNameInUseError sname)
    else do
    relVars <- resolveDBC' graph context relationVariables
    case validateSchema (Schema morphs) (M.keysSet relVars) of
      Just err -> Left (SchemaCreationError err)
      Nothing -> do
        let newSchemas = M.insert sname newSchema sschemas
            newSchema = Schema morphs
            moreIncDeps = foldr (\morph acc -> M.union acc (createIncDepsForIsomorph sname morph)) M.empty morphs
            incDepExprs = MultipleExpr (map (uncurry AddInclusionDependency) (M.toList moreIncDeps))
            dbenv = mkDatabaseContextEvalEnv transId graph
        dbstate <- runDatabaseContextEvalMonad context dbenv (evalGraphRefDatabaseContextExpr incDepExprs)
        pure (newSchemas, dbc_context dbstate)
--need to propagate dirty flag here

evalSchemaExpr (RemoveSubschema sname) context _ _ sschemas = if M.member sname sschemas then
                                           pure (M.delete sname sschemas, context)
                                         else
                                           Left (SubschemaNameNotInUseError sname)


-- | Apply SchemaIsomorphs to database context data.
class Morph a where
  morphToSchema :: Schema -> TransactionGraph -> a -> Either RelationalError a

instance Morph RelationalExpr where
  morphToSchema schema _ relExprIn = do
      let processRelExpr rexpr morph = relExprMogrify (relExprMorph morph) rexpr
      validateRelationalExprInSchema schema relExprIn
      foldM processRelExpr relExprIn (isomorphs schema)

-- | The names of inclusion dependencies might leak context about a different schema, but that's arbitrary and cannot be altered without having the user provide a renaming function or a new set of incDep names- seems extraneous.
instance Morph InclusionDependency where
  morphToSchema schema _ (InclusionDependency rexprA rexprB) = do
    let schemaRelVars = isomorphsInRelVarNames (isomorphs schema)
    rvAssoc <- mapM (\rvIn -> do
                      rvOut <- processRelationalExprInSchema schema (RelationVariable rvIn ())
                      pure (rvOut, RelationVariable rvIn ())
                  )
             (S.toList schemaRelVars)
    let replacer exprOrig = foldM (\expr (find, replace) -> if expr == find then
                                                              pure replace
                                                            else
                                                              pure expr) exprOrig rvAssoc
    rexprA' <- relExprMogrify replacer rexprA
    rexprB' <- relExprMogrify replacer rexprB
    pure (InclusionDependency rexprA' rexprB')

instance Morph InclusionDependencies where
  morphToSchema schema tg incDeps = M.fromList <$> mapM (\(n,incdep) -> (,) n <$> morphToSchema schema tg incdep) (M.toList incDeps)

{-
-- cannot be implemented because relvars map to transaction-graph-traversing expressions and we do not track schema changes over time
instance Morph RelationVariables where
  morphToSchema schema tg relVars = do
    let folder acc (IsoRename rvBase rvSchema) =
          case M.lookup rvBase relVars of
            Nothing -> Left (RelVarNotDefinedError rvBase)
            Just gfExpr -> do
              gfExprSchema <- morphToSchema schema tg gfExpr
              pure (acc <> [(rvSchema, gfExprSchema)])
    M.fromList <$> foldM folder mempty (isomorphs schema)
-}
{-
instance Morph GraphRefRelationalExpr where
-- cannot be supported because we don't track how the schema changes over the lifetime of a transaction graph
-}

notificationsAsRelationInSchema :: Notifications -> Schema -> Either RelationalError Relation
notificationsAsRelationInSchema notifs schema  = do
  let attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                    Attribute "changeExpr" RelationalExprAtomType,
                                    Attribute "reportOldExpr" RelationalExprAtomType,
                                    Attribute "reportNewExpr" RelationalExprAtomType]
      relExprT = processRelationalExprInSchema schema
      transform (name, e1, e2, e3) = do
        e1' <- relExprT e1
        e2' <- relExprT e2
        e3' <- relExprT e3
        pure (name, e1', e2', e3')
  notifsData <- mapM transform (notificationsAsData notifs)
  let mkRow (name, changeE, oldE, newE) = [TextAtom name,
                                           RelationalExprAtom changeE,
                                           RelationalExprAtom oldE,
                                           RelationalExprAtom newE]
  mkRelationFromList attrs (map mkRow notifsData)

notificationsAsData :: Notifications -> [(Text, RelationalExpr, RelationalExpr, RelationalExpr)]
notificationsAsData notifs =
    map mkRow (M.toList notifs)
  where
    mkRow (name, notif) = (name,
                           changeExpr notif,
                           reportOldExpr notif,
                           reportNewExpr notif)
