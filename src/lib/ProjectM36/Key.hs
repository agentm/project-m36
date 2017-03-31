module ProjectM36.Key where
import ProjectM36.Base
import ProjectM36.Relation
import qualified Data.Set as S
import Data.Monoid

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
inclusionDependencyForKey :: AttributeNames -> RelationalExpr -> InclusionDependency
inclusionDependencyForKey attrNames relExpr = --InclusionDependency name (exprCount relExpr) (exprCount (projectedOnKeys relExpr))
 InclusionDependency equalityExpr (ExistingRelation relationFalse)
  where 
    projectedOnKeys expr = Project attrNames expr
    exprAsSubRelation expr = Extend (AttributeExtendTupleExpr "a" (RelationAtomExpr expr)) (ExistingRelation relationTrue)
    exprCount expr = projectionForCount (Extend (AttributeExtendTupleExpr "b" (FunctionAtomExpr "count" [AttributeAtomExpr "a"] () )) (exprAsSubRelation expr))
    projectionForCount expr = Project (AttributeNames $ S.fromList ["b"]) expr
    equalityExpr = NotEquals (exprCount relExpr) (exprCount (projectedOnKeys relExpr))

databaseContextExprForUniqueKey :: RelVarName -> [AttributeName] -> DatabaseContextExpr
databaseContextExprForUniqueKey rvName attrNames = AddInclusionDependency (rvName <> "_key") $ inclusionDependencyForKey (AttributeNames (S.fromList attrNames)) (RelationVariable rvName ())

databaseContextExprForForeignKey :: IncDepName -> (RelVarName, [AttributeName]) -> (RelVarName, [AttributeName]) -> DatabaseContextExpr
databaseContextExprForForeignKey fkName (rvA, attrsA) (rvB, attrsB) = AddInclusionDependency fkName $ InclusionDependency (Project (attrsL attrsA) (RelationVariable rvA ())) (Project (attrsL attrsB) (RelationVariable rvB ()))
  where
    attrsL = AttributeNames . S.fromList    