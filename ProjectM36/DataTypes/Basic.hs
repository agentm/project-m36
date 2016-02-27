{-# LANGUAGE OverloadedStrings #-}
-- wraps up primitives plus other basic data types
module ProjectM36.DataTypes.Basic where
import ProjectM36.DataTypes.Primitive
import ProjectM36.Base

basicTypeConstructors :: TypeConstructors
basicTypeConstructors = primitiveTypeConstructors ++ moreTypes
  where
    moreTypes = [(ADTypeConstructor "Day" [],
                  [DataConstructor "Day" [TypeConstructorArg $ PrimitiveTypeConstructor "Int" intAtomType]])]


                
                