module ProjectM36.DataTypes.Either where
import ProjectM36.Base
import ProjectM36.AtomFunction
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
  compiledAtomFunction "isLeft" [eitherAtomType AnyAtomType AnyAtomType, BoolAtomType] $ \((ConstructedAtom dConsName _ _):_) -> pure (BoolAtom (dConsName == "Left"))
  ]
