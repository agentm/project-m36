module ProjectM36.DataTypes.NonEmptyList where
import ProjectM36.Base
import qualified Data.Map as M
import qualified Data.HashSet as HS
import ProjectM36.AtomFunctionError
import ProjectM36.DataTypes.List

nonEmptyListAtomType :: AtomType -> AtomType
nonEmptyListAtomType arg = ConstructedAtomType "NonEmptyList" (M.singleton "a" arg)

-- data NonEmptyList = NECons a (Cons a)
nonEmptyListTypeConstructorMapping :: TypeConstructorMapping
nonEmptyListTypeConstructorMapping = [(ADTypeConstructorDef "NonEmptyList" ["a"],
                           [DataConstructorDef "NECons" [DataConstructorDefTypeVarNameArg "a",
                            DataConstructorDefTypeConstructorArg (ADTypeConstructor "List" [TypeVariable "a"])]])]
                         
nonEmptyListLength :: Atom -> Either AtomFunctionError Int                         
nonEmptyListLength (ConstructedAtom "NECons" _ (_:nextCons:_)) = do
  c <- listLength nextCons
  pure (c + 1)
nonEmptyListLength (ConstructedAtom "NECons" _ _) = pure 1
nonEmptyListLength _ = Left AtomFunctionTypeMismatchError

nonEmptyListHead :: Atom -> Either AtomFunctionError Atom
nonEmptyListHead (ConstructedAtom "NECons" _ (val:_)) = pure val
nonEmptyListHead _ = Left AtomFunctionTypeMismatchError

{-
listMaybeHead :: Atom -> Either AtomFunctionError Atom
listMaybeHead (ConstructedAtom "Cons" _ (val:_)) = pure (ConstructedAtom "Just" aType [val])
  where
    aType = maybeAtomType (atomTypeForAtom val)
listMaybeHead (ConstructedAtom "Empty" (ConstructedAtomType _ tvMap) _) =
  case M.lookup "a" tvMap of
    Nothing -> Left AtomFunctionTypeMismatchError
    Just aType -> pure (ConstructedAtom "Nothing" aType [])
listMaybeHead _ = Left AtomFunctionTypeMismatchError
-}

nonEmptyListAtomFunctions :: AtomFunctions
nonEmptyListAtomFunctions = HS.fromList [
  Function {
     funcName = "nonEmptyListLength",
     funcType = [nonEmptyListAtomType (TypeVariableType "a"), IntAtomType],
     funcBody = FunctionBuiltInBody $
       \case
         (nonEmptyListAtom:_) ->
           IntAtom . fromIntegral <$> nonEmptyListLength nonEmptyListAtom
         _ -> Left AtomFunctionTypeMismatchError,
     funcACL = ()
     },
  Function {
    funcName = "nonEmptyListHead",
    funcType = [nonEmptyListAtomType (TypeVariableType "a"), TypeVariableType "a"],
    funcBody = FunctionBuiltInBody $
      \case
        (nonEmptyListAtom:_) -> nonEmptyListHead nonEmptyListAtom
        _ -> Left AtomFunctionTypeMismatchError,
    funcACL = ()
    }
  ]
