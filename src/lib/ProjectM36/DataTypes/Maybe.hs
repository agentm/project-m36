module ProjectM36.DataTypes.Maybe where
import ProjectM36.Base
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunctionError
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
     atomFuncType = [maybeAtomType (TypeVariableType "a"), BoolAtomType],
     atomFuncBody = AtomFunctionBody Nothing $ \((ConstructedAtom dConsName _ _):_) -> pure $ BoolAtom (dConsName /= "Nothing")
     },
  AtomFunction {
     atomFuncName = "fromMaybe",
     atomFuncType = [TypeVariableType "a", maybeAtomType (TypeVariableType "a"), TypeVariableType "a"],
     atomFuncBody = AtomFunctionBody Nothing $ \(defaultAtom:(ConstructedAtom dConsName _ (atomVal:_)):_) -> if atomTypeForAtom defaultAtom /= atomTypeForAtom atomVal then Left AtomFunctionTypeMismatchError else if dConsName == "Nothing" then pure defaultAtom else pure atomVal
     }
  ]
