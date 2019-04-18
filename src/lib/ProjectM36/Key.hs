module ProjectM36.Key where
import ProjectM36.Base
import ProjectM36.Relation
import qualified Data.Set as S
--import Data.Monoid

{-
keys can be implemented using inclusion dependencies as well: the count of the projection of the keys' attributes must be equal to the count of the tuples- p. 120 Database in Depth

example: 
:showexpr ((relation{tuple{}}:{a:=S}):{b:=count(@a)}){b}
┌─┐
│b│
├─┤
│5│
└─┘
((relation{tuple{}}:{a:=S{S#}}):{b:=count(@a)}){b}
┌─┐
│b│
├─┤
│5│
└─┘
-}

-- | Create a uniqueness constraint for the attribute names and relational expression. Note that constraint can span multiple relation variables.
inclusionDependencyForKey :: AttributeNames -> RelationalExpr -> InclusionDependency
inclusionDependencyForKey attrNames relExpr = --InclusionDependency name (exprCount relExpr) (exprCount (projectedOnKeys relExpr))
 InclusionDependency equalityExpr (ExistingRelation relationFalse)
  where 
    projectedOnKeys = Project attrNames
    exprAsSubRelation expr = Extend (AttributeExtendTupleExpr "a" (RelationAtomExpr expr)) (ExistingRelation relationTrue)
    exprCount expr = projectionForCount (Extend (AttributeExtendTupleExpr "b" (FunctionAtomExpr "count" [AttributeAtomExpr "a"] () )) (exprAsSubRelation expr))
    projectionForCount = Project (AttributeNames $ S.fromList ["b"])
    equalityExpr = NotEquals (exprCount relExpr) (exprCount (projectedOnKeys relExpr))

-- | Create a 'DatabaseContextExpr' which can be used to add a uniqueness constraint to attributes on one relation variable.
databaseContextExprForUniqueKey :: RelVarName -> [AttributeName] -> DatabaseContextExpr
databaseContextExprForUniqueKey rvName attrNames = AddInclusionDependency (rvName <> "_key") $ inclusionDependencyForKey (AttributeNames (S.fromList attrNames)) (RelationVariable rvName ())

-- | Create a foreign key constraint from the first relation variable and attributes to the second.
databaseContextExprForForeignKey :: IncDepName -> (RelVarName, [AttributeName]) -> (RelVarName, [AttributeName]) -> DatabaseContextExpr
databaseContextExprForForeignKey fkName infoA infoB =
  AddInclusionDependency fkName (inclusionDependencyForForeignKey infoA infoB)
  
inclusionDependencyForForeignKey :: (RelVarName, [AttributeName]) -> (RelVarName, [AttributeName]) -> InclusionDependency
inclusionDependencyForForeignKey (rvA, attrsA) (rvB, attrsB) = 
  InclusionDependency (
    renameIfNecessary attrsB attrsA (Project (attrsL attrsA)
                                     (RelationVariable rvA ()))) (
    Project (attrsL attrsB) (RelationVariable rvB ()))
  where
    attrsL = AttributeNames . S.fromList    
    renameIfNecessary attrsExpected attrsExisting expr = foldr folder expr (zip attrsExpected attrsExisting)
    folder (attrExpected, attrExisting) expr = if attrExpected == attrExisting then
                                                   expr
                                                 else
                                                   Rename attrExisting attrExpected expr

-- if the constraint is a foreign key constraint, then return the relations and attributes involved - this only detects foreign keys created with `databaseContextExprForForeignKey`
isForeignKeyFor :: InclusionDependency -> (RelVarName, [AttributeName]) -> (RelVarName, [AttributeName]) -> Bool
isForeignKeyFor incDep infoA infoB = incDep == checkIncDep
  where
    checkIncDep = inclusionDependencyForForeignKey infoA infoB




