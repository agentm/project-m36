{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.RelationalExpression where
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Base
import ProjectM36.Error
import qualified ProjectM36.Attribute as A
import qualified Data.Map as M
import qualified Data.HashSet as HS
import Control.Monad.State hiding (join)
import Data.Maybe

--relvar state is needed in evaluation of relational expression but only as read-only in order to extract current relvar values
evalRelationalExpr :: RelationalExpr -> DatabaseState (Either RelationalError Relation)
evalRelationalExpr (RelationVariable name) = do
  relvarTable <- liftM relationVariables get
  return $ case M.lookup name relvarTable of
    Just res -> Right res
    Nothing -> Left $ RelVarNotDefinedError name 

evalRelationalExpr (Project attrNameSet expr) = do
    rel <- evalRelationalExpr expr
    case rel of 
      Right rel2 -> return $ project attrNameSet rel2
      Left err -> return $ Left err

evalRelationalExpr (Union exprA exprB) = do
  relA <- evalRelationalExpr exprA
  relB <- evalRelationalExpr exprB
  case relA of
    Left err -> return $ Left err
    Right relA2 -> case relB of
      Left err -> return $ Left err
      Right relB2 -> return $ union relA2 relB2

evalRelationalExpr (Join exprA exprB) = do
  relA <- evalRelationalExpr exprA
  relB <- evalRelationalExpr exprB
  case relA of
    Left err -> return $ Left err
    Right relA2 -> case relB of
      Left err -> return $ Left err
      Right relB2 -> return $ join relA2 relB2
      
evalRelationalExpr (MakeStaticRelation attributeSet tupleSet) = do
  case mkRelation attributeSet tupleSet of
    Right rel -> return $ Right rel
    Left err -> return $ Left err
    
evalRelationalExpr (ExistingRelation rel) = return (Right rel)
    
evalRelationalExpr (Rename oldAttrName newAttrName relExpr) = do
  evald <- evalRelationalExpr relExpr
  case evald of
    Right rel -> return $ rename oldAttrName newAttrName rel
    Left err -> return $ Left err
    
evalRelationalExpr (Group oldAttrNameSet newAttrName relExpr) = do
  evald <- evalRelationalExpr relExpr
  case evald of 
    Right rel -> return $ group oldAttrNameSet newAttrName rel
    Left err -> return $ Left err
    
evalRelationalExpr (Ungroup attrName relExpr) = do
  evald <- evalRelationalExpr relExpr
  case evald of
    Right rel -> return $ ungroup attrName rel
    Left err -> return $ Left err
    
evalRelationalExpr (Restrict predicateExpr relExpr) = do
  evald <- evalRelationalExpr relExpr
  context <- get
  case evald of 
    Left err -> return $ Left err
    Right rel -> case predicateRestrictionFilter context predicateExpr of
      Left err -> return $ Left err
      Right filterfunc -> return $ restrict filterfunc rel
      
evalRelationalExpr (Equals relExprA relExprB) = do
  evaldA <- evalRelationalExpr relExprA
  evaldB <- evalRelationalExpr relExprB
  case evaldA of
    Left err -> return $ Left err
    Right relA -> case evaldB of 
      Left err -> return $ Left err
      Right relB -> return $ Right $ if relA == relB then relationTrue else relationFalse

emptyDatabaseContext :: DatabaseContext
emptyDatabaseContext = DatabaseContext { inclusionDependencies = HS.empty,
                                         relationVariables = M.empty}

basicDatabaseContext :: DatabaseContext
basicDatabaseContext = DatabaseContext { inclusionDependencies = HS.empty,
                                         relationVariables = M.fromList [("true", relationTrue),
                                                                         ("false", relationFalse)]}

dateExamples :: DatabaseContext
dateExamples = DatabaseContext { inclusionDependencies = HS.empty, -- add foreign key relationships
                                 relationVariables = M.union (relationVariables basicDatabaseContext) dateRelVars }
  where
    dateRelVars = M.fromList [("S", suppliers),
                              ("P", products),
                              ("SP", supplierProducts)]
    suppliers = suppliersRel
    products = productsRel
    supplierProducts = supplierProductsRel
    
suppliersRel :: Relation    
suppliersRel = case mkRelationFromList attrs atomMatrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "S#" StringAtomType, 
                                  Attribute "SNAME" StringAtomType, 
                                  Attribute "STATUS" IntAtomType, 
                                  Attribute "CITY" StringAtomType]
    atomMatrix = [
      [StringAtom "S1", StringAtom "Smith", IntAtom 20, StringAtom "London"],
      [StringAtom "S2", StringAtom "Jones", IntAtom 10, StringAtom "Paris"],
      [StringAtom "S3", StringAtom "Blake", IntAtom 30, StringAtom "Paris"],
      [StringAtom "S4", StringAtom "Clark", IntAtom 20, StringAtom "London"],
      [StringAtom "S5", StringAtom "Adams", IntAtom 30, StringAtom "Athens"]]
    
supplierProductsRel :: Relation
supplierProductsRel = case mkRelationFromList attrs matrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "S#" StringAtomType,
                                  Attribute "P#" StringAtomType, 
                                  Attribute "QTY" StringAtomType]                 
    matrix = [
      [StringAtom "S1", StringAtom "P1", IntAtom 300],
      [StringAtom "S1", StringAtom "P2", IntAtom 200],
      [StringAtom "S1", StringAtom "P3", IntAtom 400],
      [StringAtom "S1", StringAtom "P4", IntAtom 200],
      [StringAtom "S1", StringAtom "P5", IntAtom 100],
      [StringAtom "S1", StringAtom "P6", IntAtom 100],
      [StringAtom "S2", StringAtom "P1", IntAtom 300],
      [StringAtom "S2", StringAtom "P2", IntAtom 400],
      [StringAtom "S3", StringAtom "P2", IntAtom 200],
      [StringAtom "S4", StringAtom "P2", IntAtom 200],
      [StringAtom "S4", StringAtom "P4", IntAtom 300],
      [StringAtom "S4", StringAtom "P5", IntAtom 400]
      ]
             
productsRel :: Relation             
productsRel = case mkRelationFromList attrs matrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "P#" StringAtomType,
                                  Attribute "PNAME" StringAtomType,
                                  Attribute "COLOR" StringAtomType, 
                                  Attribute "WEIGHT" StringAtomType, 
                                  Attribute "CITY" StringAtomType]
    matrix = [      
      [StringAtom "P1", StringAtom "Nut", StringAtom "Red", IntAtom 12, StringAtom "London"],
      [StringAtom "P2", StringAtom "Bolt", StringAtom "Green", IntAtom 17, StringAtom "Paris"],
      [StringAtom "P3", StringAtom "Screw", StringAtom "Blue", IntAtom 17, StringAtom "Oslo"],
      [StringAtom "P4", StringAtom "Screw", StringAtom "Red", IntAtom 14, StringAtom "London"],
      [StringAtom "P5", StringAtom "Cam", StringAtom "Blue", IntAtom 12, StringAtom "Paris"],
      [StringAtom "P6", StringAtom "Cog", StringAtom "Red", IntAtom 19, StringAtom "London"]
      ]
             
--helper function to process relation variable creation/assignment          
setRelVar :: RelVarName -> Relation -> DatabaseState (Maybe RelationalError)
setRelVar relVarName rel = do 
  currstate <- get
  let newRelVars = M.insert relVarName rel $ relationVariables currstate
  case checkConstraints currstate of
    Just err -> return $ Just err
    Nothing -> do 
      put $ DatabaseContext (inclusionDependencies currstate) newRelVars
      return Nothing
      
-- it is not an error to delete a relvar which does not exist, just like it is not an error to insert a pre-existing tuple into a relation
deleteRelVar :: RelVarName -> DatabaseState (Maybe RelationalError)
deleteRelVar relVarName = do
  currstate <- get
  let newRelVars = M.delete relVarName (relationVariables currstate)
  put $ DatabaseContext (inclusionDependencies currstate) newRelVars
  return Nothing

evalContextExpr :: DatabaseExpr -> DatabaseState (Maybe RelationalError)
evalContextExpr (Define relVarName attrs) = do
  relvars <- liftM relationVariables get
  case M.member relVarName relvars of 
    True -> return (Just (RelVarAlreadyDefinedError relVarName))
    False -> setRelVar relVarName emptyRelation
      where
        emptyRelation = Relation attrs HS.empty
        
evalContextExpr (Undefine relVarName) = do
  deleteRelVar relVarName
  
evalContextExpr (Assign relVarName expr) = do
  -- in the future, it would be nice to get types from the RelationalExpr instead of needing to evaluate it
  relVarTable <- liftM relationVariables get
  let existingRelVar = M.lookup relVarName relVarTable
  value <- evalRelationalExpr expr 
  case value of 
    Left err -> return $ Just err
    Right rel -> case existingRelVar of 
      Nothing -> setRelVar relVarName rel
      Just existingRel -> if attributes existingRel == attributes rel then 
                            setRelVar relVarName rel
                          else
                            return $ Just RelVarAssignmentTypeMismatchError
                            
evalContextExpr (Insert relVarName relExpr) = evalContextExpr $ Assign relVarName (Union relExpr (RelationVariable relVarName))

--assign empty rel until restriction is implemented
evalContextExpr (Delete relVarName predicate) = do
  updatedRel <- evalRelationalExpr (Restrict (NotPredicate predicate) (RelationVariable relVarName))
  case updatedRel of
    Left err -> return $ Just err
    Right rel -> setRelVar relVarName rel
                                 
--union of restricted+updated portion and the unrestricted+unupdated portion
evalContextExpr (Update relVarName attrAssignments restrictionPredicateExpr) = do
  currstate <- get
  let relVarTable = relationVariables currstate 
  case predicateRestrictionFilter currstate restrictionPredicateExpr of
    Left err -> return $ Just err
    Right predicateFunc -> do 
                           case M.lookup relVarName relVarTable of
                             Nothing -> return $ Just (RelVarNotDefinedError relVarName)
                             Just rel -> case makeUpdatedRel rel of
                               Left err -> return $ Just err
                               Right updatedRel -> setRelVar relVarName updatedRel
                               where
                                 makeUpdatedRel relin = do
                                   restrictedPortion <- restrict predicateFunc relin
                                   unrestrictedPortion <- restrict (not . predicateFunc) relin
                                   updatedPortion <- relMap (updateTuple attrAssignments) restrictedPortion
                                   union updatedPortion unrestrictedPortion

evalContextExpr (AddInclusionDependency dep) = do
  currstate <- get
  let newDeps = HS.insert dep (inclusionDependencies currstate)
  put $ DatabaseContext newDeps (relationVariables currstate)
  return Nothing

evalContextExpr (MultipleExpr exprs) = do
  --the multiple expressions must pass the same context around- not the old unmodified context
  evald <- forM exprs evalContextExpr
  --some lifting magic needed here
  case catMaybes evald of
    [] -> return $ Nothing
    err:_ -> return $ Just err
  
-- restrict relvar to get affected tuples, update tuples, delete restriction from relvar, relvar = relvar union updated tuples  
--evalRelVarExpr (Update relVarName updateMap) = do

--run verification on all constraints
checkConstraints :: DatabaseContext -> Maybe RelationalError
checkConstraints context = case failures of 
  [] -> Nothing
  l:_ -> Just l
  where
    failures = catMaybes $ map checkIncDep (HS.toList deps)
    deps = inclusionDependencies context
    eval expr = runState (evalRelationalExpr expr) context
    checkIncDep (InclusionDependency depName subsetExpr supersetExpr) = case evaldSub of
        (Left err, _) -> Just err 
        (Right relSub, _) -> case evaldSuper of
          (Left err, _) -> Just err
          (Right relSuper, _) -> case union relSub relSuper of
            Left err -> Just err
            Right resultRel -> case resultRel == relSuper of
              False -> Just $ InclusionDependencyCheckError depName
              True -> Nothing
       where
         evaldSub = eval subsetExpr
         evaldSuper = eval supersetExpr

{-
checkConstraint :: InclusionDependency -> DatabaseState (Maybe RelationalError)
checkConstraint (InclusionDependency name subDep superDep) = do
  evalSub <- evalRelationalExpr subDep
  evalSuper <- evalRelationalExpr superDep
  case evalSub of 
    Left err -> Just err
    Right relSub 
  result <- liftM2 union evalSub evalSuper
  return $ Nothing
-}
    
-- the type of a relational expression is equal to the relation attribute set returned from executing the relational expression; therefore, the type can be cheaply derived by evaluating a relational expression and ignoring and tuple processing
-- furthermore, the type of a relational expression is the resultant header of the evaluated empty-tupled relation
         
typeForRelationalExpr :: RelationalExpr -> DatabaseState (Either RelationalError Relation)
typeForRelationalExpr expr = do
  currstate <- get
  --replace the relationVariables context element with a cloned set of relation devoid of tuples
  put $ contextWithEmptyTupleSets currstate
  evalRelationalExpr expr
  
--returns a database context with all tuples removed  
--this is useful for type checking and optimization
contextWithEmptyTupleSets :: DatabaseContext -> DatabaseContext
contextWithEmptyTupleSets contextIn = DatabaseContext (inclusionDependencies contextIn) $ M.map (\rel -> Relation (attributes rel) emptyTupleSet) (relationVariables contextIn)

  
{- used for restrictions- take the restrictionpredicate and return the corresponding filter function -}
predicateRestrictionFilter :: DatabaseContext -> RestrictionPredicateExpr -> Either RelationalError (RelationTuple -> Bool)
predicateRestrictionFilter context (AndPredicate expr1 expr2) = do
  expr1v <- predicateRestrictionFilter context expr1
  expr2v <- predicateRestrictionFilter context expr2
  return $ \x -> expr1v x && expr2v x

predicateRestrictionFilter context (OrPredicate expr1 expr2) = do
  expr1v <- predicateRestrictionFilter context expr1
  expr2v <- predicateRestrictionFilter context expr2
  return $ \x -> expr1v x || expr2v x
  
predicateRestrictionFilter _ TruePredicate = Right $ \_ -> True
  
predicateRestrictionFilter context (NotPredicate expr) = do
  exprv <- predicateRestrictionFilter context expr
  return $ \x -> not (exprv x)

predicateRestrictionFilter context (RelationalExprPredicate relExpr) = case runState (evalRelationalExpr relExpr) context of
  (Left err, _) -> Left err
  (Right rel, _) -> if rel == relationTrue then
                      Right $ \_ -> True  
                    else if rel == relationFalse then
                     Right $ \_ -> False      
                         else
                           Left $ PredicateExpressionError "Relational restriction filter must evaluate to 'true' or 'false'"
    
predicateRestrictionFilter _ (AttributeEqualityPredicate attrName atom) = Right $ \tupleIn -> case atomForAttributeName attrName tupleIn of
  Left _ -> False
  Right atom2 -> atom2 == atom





