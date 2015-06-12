{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Key where
import ProjectM36.Base
import ProjectM36.Relation
import qualified Data.Set as S

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
    exprAsSubRelation expr = Extend (AttributeTupleExpr "a" (RelationAtomExpr expr)) (ExistingRelation relationTrue)
    exprCount expr = projectionForCount (Extend (AttributeTupleExpr "b" (FunctionAtomExpr "count" [AttributeAtomExpr "a"])) (exprAsSubRelation expr))
    projectionForCount expr = Project (AttributeNames $ S.fromList ["b"]) expr
    equalityExpr = NotEquals (exprCount relExpr) (exprCount (projectedOnKeys relExpr))

