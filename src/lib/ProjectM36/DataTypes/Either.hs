module ProjectM36.DataTypes.Either where
import ProjectM36.Base
import ProjectM36.AtomFunction
import ProjectM36.AtomFunctionError
import qualified Data.HashSet as HS
import qualified Data.Map as M
       
eitherAtomType :: AtomType -> AtomType -> AtomType
eitherAtomType tA tB = ConstructedAtomType "Either" (M.fromList [("a", tA), ("b", tB)])
  
eitherTypeConstructorMapping :: TypeConstructorMapping                
eitherTypeConstructorMapping = [(ADTypeConstructorDef "Either" ["a", "b"],
                                 [DataConstructorDef "Left" [DataConstructorDefTypeVarNameArg "a"],
                                  DataConstructorDef "Right" [DataConstructorDefTypeVarNameArg "b"]])]
       
eitherAtomFunctions :: AtomFunctions                               
eitherAtomFunctions = HS.fromList [
  compiledAtomFunction "isLeft" [eitherAtomType (TypeVariableType "a") (TypeVariableType "b"), BoolAtomType] $ \case
        (ConstructedAtom dConsName _ _:_) -> pure (BoolAtom (dConsName == "Left"))
        _ -> Left AtomFunctionTypeMismatchError
  ]
