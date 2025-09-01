module ProjectM36.AttributeExpr where
import ProjectM36.Base
import ProjectM36.Attribute as A

attributeName :: AttributeExprBase a -> AttributeName
attributeName (AttributeAndTypeNameExpr nam _ _) = nam
attributeName (NakedAttributeExpr attr) = A.attributeName attr

-- | Identify all type constructors mentioned in the expression.
typeConstructor :: AttributeExprBase a -> Maybe TypeConstructor
typeConstructor NakedAttributeExpr{} = Nothing
typeConstructor (AttributeAndTypeNameExpr _ tCons _) = Just tCons
