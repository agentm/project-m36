module ProjectM36.IsomorphicSchema where
import ProjectM36.Base
import ProjectM36.Error
import Data.Maybe
import Control.Monad
-- isomorphic schemas offer bi-directional functors between two schemas

--TODO: note that renaming a relvar should alter any stored isomorphisms as well
--TODO: rel attrs rename or transform (needs bidirectional atom functions)

-- the isomorphic building blocks should not be arbitrarily combined; for example, combing restrict and union on the same target relvar does not make sense as that would create effects at a distance in the secondary schema
--this should create a new schema
data SchemaIsomorph = IsoRestrict RelVarName RestrictionPredicateExpr (Maybe RelVarName, Maybe RelVarName) | --maps one relvar into two relvars (true, false), if the false name is Nothing, no relvar in schemaB is created. This is useful for renaming relation variables between schemas or hiding relvars altogether.
                      IsoUnion (RelVarName, Maybe RelVarName) RestrictionPredicateExpr RelVarName  --maps two relvars to one relvar
                      -- IsoTypeConstructor in morphAttrExpr
                      
type SchemaIsomorphs = [SchemaIsomorph]

-- | Return True iff the SchemaIsomorph is fully isomorphic in the sense that it does not represent an isomorphic sub-schema.
isFullyIsomorphic :: SchemaIsomorph -> Bool
isFullyIsomorphic (IsoRestrict _ _ (a, b)) = isJust a && isJust b
isFullyIsomorphic (IsoUnion (_, a) _ _) = isJust a

-- | Convenience function which renames a relvar in schemaA to another name in schemaB.
isomorphRename :: RelVarName -> RelVarName -> SchemaIsomorph
isomorphRename nameA nameB = IsoRestrict nameA TruePredicate (Just nameB, Nothing)

isomorphHide :: RelVarName -> SchemaIsomorph
isomorphHide nameA = IsoRestrict nameA (NotPredicate TruePredicate) (Nothing, Nothing)

{-
invert :: SchemaIsomorph -> SchemaIsomorph
invert (IsoRelVarName nameA nameB) = IsoRelVarName nameB nameA
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

spam :: Either RelationalError RelationalExpr
spam = relExprMogrify (relExprMorph (IsoRestrict "emp" TruePredicate (Just "nonboss", Just "boss"))) (RelationVariable "emp" ())

spam2 :: Either RelationalError RelationalExpr
spam2 = relExprMogrify (relExprMorph (IsoUnion ("boss", Just "nonboss") TruePredicate "emp")) (RelationVariable "boss" ()) 

databaseContextExprMorph :: SchemaIsomorph -> (DatabaseContextExpr -> Either RelationalError DatabaseContextExpr)
databaseContextExprMorph iso@(IsoRestrict rvIn filt split) = \expr -> case expr of
  Assign rv relExpr | rv == rvIn -> do
    ex <- relExprF relExpr
    let trueExpr n = Assign n (Restrict filt ex)
        falseExpr n = Assign n (Restrict (NotPredicate filt) ex)
    case split of
      (Just rvTrue, Just rvFalse) -> pure $ MultipleExpr [trueExpr rvTrue, falseExpr rvFalse]
      (Just rvTrue, Nothing) -> pure (trueExpr rvTrue)
      (Nothing, Just rvFalse) -> pure (falseExpr rvFalse)
      (Nothing, Nothing) -> Left (RelVarNotDefinedError rvIn)
  Insert rv relExpr | rv == rvIn -> do
    ex <- relExprF relExpr
    let trueExpr n = Insert n (Restrict filt ex)
        falseExpr n = Insert n (Restrict (NotPredicate filt) ex)
    case split of
      (Just rvTrue, Just rvFalse) -> pure $ MultipleExpr [trueExpr rvTrue, falseExpr rvFalse]
      (Just rvTrue, Nothing) -> pure (trueExpr rvTrue)
      (Nothing, Just rvFalse) -> pure (falseExpr rvFalse)
      (Nothing, Nothing) -> Left (RelVarNotDefinedError rvIn)
  Update rv attrMap predi | rv == rvIn -> do
--something missing here, create a function to conver the attr->atom map to a relexpr to insert based on what matches in the existing relation variable    
    let trueExpr n = MultipleExpr [Update n attrMap (AndPredicate predi filt)]
                                   --Insert rvTrue ...
        falseExpr n = MultipleExpr [Update n attrMap (AndPredicate predi (NotPredicate filt))]
                      --Insert rvFalse, retrieve matching tuples based on attrMap
    case split of
      (Just rvTrue, Just rvFalse) -> pure (MultipleExpr [trueExpr rvTrue, falseExpr rvFalse])
      (Just rvTrue, Nothing) -> pure (trueExpr rvTrue)
      (Nothing, Just rvFalse) -> pure (falseExpr rvFalse)
      (Nothing, Nothing) -> Left (RelVarNotDefinedError rvIn)
  MultipleExpr exprs -> MultipleExpr <$> mapM (databaseContextExprMorph iso) exprs
  orig -> pure orig                                    
 where
   relExprF = relExprMogrify (relExprMorph iso)
databaseContextExprMorph iso@(IsoUnion (rvTrue, mRvFalse) filt rvOut) = \expr -> case expr of   
  --assign: replace all instances in the portion of the target relvar with the new tuples from the relExpr
  --problem: between the delete->insert, constraints could be violated which would not otherwise be violated in the "in" schema. This implies that there should be a combo operator which can insert/update/delete in a single pass based on relexpr queries, or perhaps MultipleExpr should be the infamous "comma" operator from TutorialD?
  Assign rv relExpr | rv == rvTrue -> relExprF relExpr >>= \ex -> pure $ MultipleExpr [Delete rvOut filt,
                                                                                      Insert rvOut (Restrict filt ex)]
  Assign rv relExpr | Just rv == mRvFalse -> relExprF relExpr >>= \ex -> pure $ MultipleExpr [Delete rvOut (NotPredicate filt),            
                                                                  Insert rvOut (Restrict (NotPredicate filt) ex)]
  Insert rv relExpr | rv == rvTrue || Just rv == mRvFalse -> relExprF relExpr >>= \ex -> pure $ Insert rvOut ex
  Delete rv delPred | rv == rvTrue -> pure $ Delete rvOut (AndPredicate delPred filt)
  Delete rv delPred | Just rv == mRvFalse -> pure $ Delete rvOut (AndPredicate delPred (NotPredicate filt))
  Update rv attrMap predi | rv == rvTrue -> pure $ Update rvOut attrMap (AndPredicate predi filt)
  Update rv attrMap predi | Just rv == mRvFalse -> pure $ Update rvOut attrMap (AndPredicate (NotPredicate filt) predi)
  orig -> pure orig
 where
   relExprF = relExprMogrify (relExprMorph iso)
  
databaseContextExprMogrify :: (DatabaseContextExpr -> Either RelationalError DatabaseContextExpr) -> DatabaseContextExpr -> Either RelationalError DatabaseContextExpr
databaseContextExprMogrify func (MultipleExpr exprs) = do 
  exprs' <- mapM func exprs
  func (MultipleExpr exprs')
databaseContextExprMogrify func orig = func orig

-- | Apply the isomorphism transformations to the relational expression to convert the relational expression from operating on one schema to a disparate, isomorphic schema.
applyRelationalExprSchemaIsomorphs :: SchemaIsomorphs -> RelationalExpr -> Either RelationalError RelationalExpr
applyRelationalExprSchemaIsomorphs morphs expr = foldM (\expr' morph -> relExprMogrify (relExprMorph morph) expr') expr morphs

-- | Apply the isomorphism transformations to the database context expression to convert the expression from operating on one schema to a disparate, isomorphic schema.
applyDatabaseCotextExprSchemaIsomorphs :: SchemaIsomorphs -> DatabaseContextExpr -> Either RelationalError DatabaseContextExpr
applyDatabaseCotextExprSchemaIsomorphs morphs expr = foldM (\expr' morph -> databaseContextExprMogrify (databaseContextExprMorph morph) expr') expr morphs