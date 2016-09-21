module ProjectM36.IsomorphicSchema where
import ProjectM36.Base
-- isomorphic schemas offer bi-directional functors between two schemas

--TODO: rel attrs rename or transform (needs bidirectional atom functions)

-- the isomorphic building blocks should not be arbitrarily combined; for example, combing restrict and union on the same target relvar does not make sense as that would create effects at a distance in the secondary schema
--this should create a new schema
data SchemaIsomorph = IsoRelVarName RelVarName RelVarName | --maps relvar names between schemas
                      IsoRestrict RelVarName RestrictionPredicateExpr (RelVarName,RelVarName) | --maps one relvar into two relvars (true, false)
                      IsoUnion (RelVarName, RelVarName) RestrictionPredicateExpr RelVarName  --maps two relvars to one relvar
                      -- IsoTypeConstructor in morphAttrExpr
                      
type SchemaIsomorphs = [SchemaIsomorph]

{-
invert :: SchemaIsomorph -> SchemaIsomorph
invert (IsoRelVarName nameA nameB) = IsoRelVarName nameB nameA
-}
  
-- | Morph a relational expression in one schema to another isomorphic schema.
relExprMorph :: SchemaIsomorph -> (RelationalExpr -> RelationalExpr)
-- wherever nameA is found in the expr, it must be replaced by nameB
relExprMorph (IsoRelVarName nameA nameB) = \expr -> case expr of
  RelationVariable rv () | rv == nameA -> RelationVariable nameB ()
  orig -> orig  
relExprMorph (IsoRestrict relIn _ (relOutTrue, relOutFalse)) = \expr -> case expr of 
  RelationVariable rv () | rv == relIn -> Union (RelationVariable relOutTrue ()) (RelationVariable relOutFalse ())
  orig -> orig
relExprMorph (IsoUnion (relInT, relInF) _ relTarget) = \expr -> case expr of
  RelationVariable rv () | rv == relInT || rv == relInF -> RelationVariable relTarget ()
  orig -> orig
  
relExprMogrify :: (RelationalExpr -> RelationalExpr) -> RelationalExpr -> RelationalExpr
relExprMogrify func (Project attrs expr) = func (Project attrs (func expr))
relExprMogrify func (Union exprA exprB) = func (Union (func exprA) (func exprB))
relExprMogrify func (Join exprA exprB) = func (Join (func exprA) (func exprB))
relExprMogrify func (Rename n1 n2 expr) = func (Rename n1 n2 (func expr))
relExprMogrify func (Difference exprA exprB) = func (Difference (func exprA) (func exprB))
relExprMogrify func (Group ns n expr) = func (Group ns n (func expr))
relExprMogrify func (Ungroup n expr) = func (Ungroup n (func expr))
relExprMogrify func (Restrict predi expr) = func (Restrict predi (func expr))
relExprMogrify func (Equals exprA exprB) = func (Equals (func exprA) (func exprB))
relExprMogrify func (NotEquals exprA exprB) = func (NotEquals (func exprA) (func exprB))
relExprMogrify func (Extend ext expr) = func (Extend ext (func expr))
relExprMogrify func other = func other

spam :: RelationalExpr
spam = relExprMogrify (relExprMorph (IsoRestrict "emp" TruePredicate ("nonboss", "boss"))) (RelationVariable "emp" ())

spam2 :: RelationalExpr
spam2 = relExprMogrify (relExprMorph (IsoUnion ("boss", "nonboss") TruePredicate "emp")) (RelationVariable "boss" ()) 

databaseContextExprMorph :: SchemaIsomorph -> (DatabaseContextExpr -> DatabaseContextExpr)
databaseContextExprMorph iso@(IsoRelVarName nameA nameB) = \expr -> case expr of
  Define rv attrExprs | rv == nameA -> Define nameB attrExprs
  Undefine rv | rv == nameA -> Undefine nameB
  Assign rv relExpr | rv == nameA -> Assign nameB (relExprF relExpr)
  Insert rv relExpr | rv == nameA -> Insert nameB (relExprF relExpr)
  Delete rv predi | rv == nameA -> Delete nameB predi -- needs predi mogrifier
  Update rv (attrMap) predi | rv == nameA -> Update nameB attrMap predi
  orig -> orig
 where
   relExprF = relExprMogrify (relExprMorph iso)
databaseContextExprMorph iso@(IsoRestrict rvIn filt (rvTrue, rvFalse)) = \expr -> case expr of
  Assign rv relExpr | rv == rvIn -> MultipleExpr [
    Assign rvTrue (Restrict filt (relExprF relExpr)),
    Assign rvFalse (Restrict (NotPredicate filt) (relExprF relExpr))]   
  Insert rv relExpr | rv == rvIn -> MultipleExpr [
    Insert rvTrue (Restrict filt (relExprF relExpr)),
    Insert rvFalse (Restrict (NotPredicate filt) (relExprF relExpr))]   
  Update rv attrMap predi | rv == rvIn -> MultipleExpr [
    Update rvTrue attrMap (AndPredicate predi filt),
    --Insert rvTrue (Restrict predi (relExprF relExpr)), --something missing here, create a function to conver the attr->atom map to a relexpr to insert based on what matches in the existing relation variable
    Update rvFalse attrMap (AndPredicate predi (NotPredicate filt))
    --Insert rvFalse (Restrict (NotPredicate filt) (relExprF relExpr))
    ]   
  MultipleExpr exprs -> MultipleExpr (map (databaseContextExprMorph iso) exprs)
  orig -> orig                                    
 where
   relExprF = relExprMogrify (relExprMorph iso)
databaseContextExprMorph iso@(IsoUnion (rvTrue, rvFalse) filt rvOut) = \expr -> case expr of   
  --assign: replace all instances in the portion of the target relvar with the new tuples from the relExpr
  --problem: between the delete->insert, constraints could be violated which would not otherwise be violated in the "in" schema. This implies that there should be a combo operator which can insert/update/delete in a single pass based on relexpr queries, or perhaps MultipleExpr should be the infamous "comma" operator from TutorialD?
  Assign rv relExpr | rv == rvTrue -> MultipleExpr [Delete rvOut filt,
                                                    Insert rvOut (Restrict filt (relExprF relExpr))]
  Assign rv relExpr | rv == rvFalse -> MultipleExpr [Delete rvOut (NotPredicate filt),            
                                                     Insert rvOut (Restrict (NotPredicate filt) (relExprF relExpr))]
  Insert rv relExpr | rv == rvTrue || rv == rvFalse -> Insert rvOut (relExprF relExpr)
  Delete rv delPred | rv == rvTrue -> Delete rvOut (AndPredicate delPred filt)
  Delete rv delPred | rv == rvFalse -> Delete rvOut (AndPredicate delPred (NotPredicate filt))
  Update rv attrMap predi | rv == rvTrue -> Update rvOut attrMap (AndPredicate predi filt)
  Update rv attrMap predi | rv == rvFalse -> Update rvOut attrMap (AndPredicate (NotPredicate filt) predi)
  orig -> orig
 where
   relExprF = relExprMogrify (relExprMorph iso)
  
databaseContextExprMogrify :: (DatabaseContextExpr -> DatabaseContextExpr) -> DatabaseContextExpr -> DatabaseContextExpr
databaseContextExprMogrify func (MultipleExpr exprs) = func (MultipleExpr (map func exprs))
databaseContextExprMogrify func orig = func orig

-- | Apply the isomorphism transformations to the relational expression to convert the relational expression from operating on one schema to a disparate, isomorphic schema.
applyRelationalExprSchemaIsomorphs :: SchemaIsomorphs -> RelationalExpr -> RelationalExpr
applyRelationalExprSchemaIsomorphs morphs expr = foldl (\expr' morph -> relExprMogrify (relExprMorph morph) expr') expr morphs

-- | Apply the isomorphism transformations to the database context expression to convert the expression from operating on one schema to a disparate, isomorphic schema.
applyDatabaseCotextExprSchemaIsomorphs :: SchemaIsomorphs -> DatabaseContextExpr -> DatabaseContextExpr
applyDatabaseCotextExprSchemaIsomorphs morphs expr = foldl (\expr' morph -> databaseContextExprMogrify (databaseContextExprMorph morph) expr') expr morphs