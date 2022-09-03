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
  Function {
     funcName ="isJust",
     funcType = [maybeAtomType (TypeVariableType "a"), BoolAtomType],
     funcBody = FunctionBuiltInBody $
       \case
               ConstructedAtom dConsName _ _:_ -> pure $ BoolAtom (dConsName /= "Nothing")
               _ -> Left AtomFunctionTypeMismatchError
     },
  Function {
     funcName = "fromMaybe",
     funcType = [TypeVariableType "a", maybeAtomType (TypeVariableType "a"), TypeVariableType "a"],
     funcBody = FunctionBuiltInBody $
       \case
         (defaultAtom:ConstructedAtom dConsName _ (atomVal:_):_) -> if atomTypeForAtom defaultAtom /= atomTypeForAtom atomVal then Left AtomFunctionTypeMismatchError else if dConsName == "Nothing" then pure defaultAtom else pure atomVal
         _ ->Left AtomFunctionTypeMismatchError         
     }
  ]

{- To create an inclusion dependency for uniqueness for "Just a" values only 
person := relation{name Text, boss Maybe Text}{tuple{name "Steve",boss Nothing}, tuple{name "Bob", boss Just "Steve"}}
:showexpr ((relation{tuple{}}:{a:=person where ^isJust(@boss)}):{b:=count(@a)}){b}
:showexpr ((relation{tuple{}}:{a:=person{boss} where ^isJust(@boss)}):{b:=count(@a)}){b}
constraint uniqueJust ((relation{tuple{}}:{a:=person where ^isJust(@boss)}):{b:=count(@a)}){b} in ((relation{tuple{}}:{a:=person{boss} where ^isJust(@boss)}):{b:=count(@a)}){b}
person := relation{name Text, boss Maybe Text}{tuple{name "Steve",boss Nothing}, tuple{name "Bob", boss Just "Steve"}, tuple{name "Jim", boss Just "Steve"}} 
ERR: InclusionDependencyCheckError "uniqueJust"
-}
