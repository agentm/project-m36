{-# LANGUAGE OverloadedStrings #-}
-- wraps up primitives plus other basic data types
module ProjectM36.DataTypes.Basic where
import ProjectM36.DataTypes.Primitive
import ProjectM36.Base

basicTypeConstructorMapping :: TypeConstructorMapping
basicTypeConstructorMapping = primitiveTypeConstructorMapping ++ moreTypes
  where
    moreTypes = [(ADTypeConstructorDef "Day" ["a"],
                  [DataConstructorDef "Day" [DataConstructorDefTypeConstructorArg (PrimitiveTypeConstructor "Int" intAtomType)]]),
                 (ADTypeConstructorDef "Maybe" ["a"],
                  [DataConstructorDef "Nothing" [],
                   DataConstructorDef "Just" [DataConstructorDefTypeVarNameArg "a"]])
                 ]


                
                