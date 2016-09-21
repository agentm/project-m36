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
     atomFuncType = [maybeAtomType AnyAtomType, BoolAtomType],
     atomFuncBody = AtomFunctionBody Nothing $ \((ConstructedAtom dConsName _ _):_) -> BoolAtom (dConsName /= "Nothing")
     },
  AtomFunction {
     atomFuncName = "fromMaybe",
     atomFuncType = [AnyAtomType, maybeAtomType AnyAtomType, AnyAtomType], -- this really should be more specific, but we don't have type checker handling for this yet
     atomFuncBody = AtomFunctionBody Nothing $ \(defaultAtom:(ConstructedAtom dConsName _ (atomVal:_)):_) -> if atomTypeForAtom defaultAtom /= atomTypeForAtom atomVal then error "poop" else if dConsName == "Nothing" then defaultAtom else atomVal
     }
  ]
