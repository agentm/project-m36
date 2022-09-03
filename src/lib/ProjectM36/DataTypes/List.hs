module ProjectM36.DataTypes.List where
import ProjectM36.Base
import ProjectM36.DataTypes.Maybe
import ProjectM36.DataTypes.Primitive
import qualified Data.Map as M
import qualified Data.HashSet as HS
import ProjectM36.AtomFunctionError

listAtomType :: AtomType -> AtomType
listAtomType arg = ConstructedAtomType "List" (M.singleton "a" arg)

listTypeConstructorMapping :: TypeConstructorMapping
listTypeConstructorMapping = [(ADTypeConstructorDef "List" ["a"],
                           [DataConstructorDef "Empty" [],
                           DataConstructorDef "Cons" [DataConstructorDefTypeVarNameArg "a",
                                                      DataConstructorDefTypeConstructorArg (ADTypeConstructor "List" [TypeVariable "a"])]])]
                         
listLength :: Atom -> Either AtomFunctionError Int                         
listLength (ConstructedAtom "Cons" _ (_:nextCons:_)) = do
  c <- listLength nextCons
  pure (c + 1)
listLength (ConstructedAtom "Empty" _ _) = pure 0
listLength _ = Left AtomFunctionTypeMismatchError

listMaybeHead :: Atom -> Either AtomFunctionError Atom
listMaybeHead (ConstructedAtom "Cons" _ (val:_)) = pure (ConstructedAtom "Just" aType [val])
  where
    aType = maybeAtomType (atomTypeForAtom val)
listMaybeHead (ConstructedAtom "Empty" (ConstructedAtomType _ tvMap) _) =
  case M.lookup "a" tvMap of
    Nothing -> Left AtomFunctionTypeMismatchError
    Just aType -> pure (ConstructedAtom "Nothing" aType [])
listMaybeHead _ = Left AtomFunctionTypeMismatchError

listAtomFunctions :: AtomFunctions
listAtomFunctions = HS.fromList [
  Function {
     funcName = "length",
     funcType = [listAtomType (TypeVariableType "a"), IntAtomType],
     funcBody = FunctionBuiltInBody $
       \case
               (listAtom:_) ->
                 IntAtom . fromIntegral <$> listLength listAtom
               _ -> Left AtomFunctionTypeMismatchError
     },
  Function {
    funcName = "maybeHead",
    funcType = [listAtomType (TypeVariableType "a"), maybeAtomType (TypeVariableType "a")],
    funcBody = FunctionBuiltInBody $
               \case
                 (listAtom:_) -> listMaybeHead listAtom
                 _ -> Left AtomFunctionTypeMismatchError
    }
  ]
                    
--just a private utility function
listCons :: AtomType -> [Atom] -> Atom
listCons typ [] = ConstructedAtom "Empty" (listAtomType typ) []
listCons typ (a:as) = ConstructedAtom "Cons" (listAtomType typ) [a, listCons typ as]
