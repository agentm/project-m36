module ProjectM36.DataTypes.NonEmpty where
import ProjectM36.Base
import ProjectM36.DataTypes.Maybe
import ProjectM36.DataTypes.Primitive
import qualified Data.Map as M
import qualified Data.HashSet as HS
import ProjectM36.AtomFunctionError

nonEmptyAtomType :: AtomType -> AtomType
nonEmptyAtomType arg = ConstructedAtomType "NonEmpty" (M.singleton "a" arg)
{-
listTypeConstructorMapping :: TypeConstructorMapping
listTypeConstructorMapping = [(ADTypeConstructorDef "List" ["a"],
                           [DataConstructorDef "Empty" [],
                           DataConstructorDef "Cons" [DataConstructorDefTypeVarNameArg "a",
                                                      DataConstructorDefTypeConstructorArg (ADTypeConstructor "List" [TypeVariable "a"])]])]

Base.hs:data DataConstructorDef = DataConstructorDef DataConstructorName [DataConstructorDefArg] deriving (Eq, Show, Binary, Generic, NFData)
-}
nonEmptyTypeConstructorMapping :: TypeConstructorMapping
nonEmptyTypeConstructorMapping = [(ADTypeConstructorDef "NonEmpty" ["a"],
                           [DataConstructorDef "NECons" [DataConstructorDefTypeVarNameArg "a",
                            DataConstructorDefTypeConstructorArg (ADTypeConstructor "NonEmpty" [TypeVariable "a"])]])]
                         
nonEmptyLength :: Atom -> Either AtomFunctionError Int                         
nonEmptyLength (ConstructedAtom "NECons" _ (_:nextNECons:_)) = do
  c <- nonEmptyLength nextNECons
  pure (c + 1)
nonEmptyLength (ConstructedAtom "NECons" _ _) = pure 1
nonEmptyLength _ = Left AtomFunctionTypeMismatchError

nonEmptyHead :: Atom -> Either AtomFunctionError Atom
nonEmptyHead (ConstructedAtom "NECons" _ (val:_)) = pure (ConstructedAtom "Just" aType [val])
  where
    aType = maybeAtomType (atomTypeForAtom val)
nonEmptyHead _ = Left AtomFunctionTypeMismatchError

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

listAtomFunctions :: AtomFunctions
listAtomFunctions = HS.fromList [
  AtomFunction {
     atomFuncName = "length",
     atomFuncType = [nonEmptyAtomType (TypeVariableType "a"), IntAtomType],
     atomFuncBody = AtomFunctionBody Nothing (\(nonEmptyAtom:_) ->
                                                 IntAtom . fromIntegral <$> nonEmptyLength nonEmptyAtom)
     },
  AtomFunction {
    atomFuncName = "nonEmptyHead",
    atomFuncType = [nonEmptyAtomType (TypeVariableType "a"), maybeAtomType (TypeVariableType "a")],
    atomFuncBody = AtomFunctionBody Nothing (\(nonEmptyAtom:_) -> nonEmptyHead nonEmptyAtom)
    }
  ]
                    
--just a private utility function
nonEmptyCons :: AtomType -> [Atom] -> Atom
nonEmptyCons typ [a] = ConstructedAtom "NECons" (nonEmptyAtomType typ) []
nonEmptyCons typ (a:as) = ConstructedAtom "NECons" (nonEmptyAtomType typ) [a, nonEmptyCons typ as]
nonEmptyCons _ [] = error "nonEmptyCons error: can't construct an empty nonempty"
