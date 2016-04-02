{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.DataTypes.Maybe where
import ProjectM36.Base
import ProjectM36.DataTypes.Primitive
import qualified Data.HashSet as HS
import qualified Data.Map as M

maybeAtomType :: AtomType -> AtomType
maybeAtomType arg = ConstructedAtomType "Maybe" (M.singleton "a" arg)
  
maybeTypeConstructorMapping :: TypeConstructorMapping
maybeTypeConstructorMapping = [(ADTypeConstructorDef "Maybe" ["a"],
                                [DataConstructorDef "Nothing" [],
                                 DataConstructorDef "Just" [DataConstructorDefTypeVarNameArg "a"]])
                              ]

maybeAtomFunctions :: AtomFunctions
maybeAtomFunctions = HS.fromList [
  AtomFunction {
     atomFuncName ="isJust",
     atomFuncType = [maybeAtomType AnyAtomType, boolAtomType],
     atomFunc = \((ConstructedAtom dConsName _ _):_) -> Atom (dConsName == "Nothing")
     }       
  ]