module ProjectM36.DataTypes.SQL.Null where
import ProjectM36.Base
import ProjectM36.AtomFunctionError
import qualified Data.Map as M
import qualified Data.HashSet as HS

-- analogous but not equivalent to a Maybe type due to how NULLs interact with every other value

nullAtomType :: AtomType -> AtomType
nullAtomType arg = ConstructedAtomType "SQLNullable" (M.singleton "a" arg)

nullTypeConstructorMapping :: TypeConstructorMapping
nullTypeConstructorMapping = [(ADTypeConstructorDef "SQLNullable" ["a"],
                                [DataConstructorDef "SQLNull" [],
                                 DataConstructorDef "SQLJust" [DataConstructorDefTypeVarNameArg "a"]])
                              ]

nullAtomFunctions :: AtomFunctions
nullAtomFunctions = HS.fromList [
  Function {
      funcName = "sql_isnull", --this function works on any type variable, not just SQLNullable because removing the isnull function in cases where the type is clearly not SQLNullable is more difficult
      funcType = [TypeVariableType "a", BoolAtomType],
      funcBody = FunctionBuiltInBody $
        \case
           a:[] -> pure $ BoolAtom (isNull a)
           _ -> Left AtomFunctionTypeMismatchError
      },
    Function {
      funcName = "sql_equals",
      funcType = [nullAtomType (TypeVariableType "a"),
                  nullAtomType (TypeVariableType "a"),
                  nullAtomType BoolAtomType],
      funcBody = FunctionBuiltInBody nullEq
      },
    Function {
      funcName = "sql_and",
      funcType = [TypeVariableType "a", TypeVariableType "b", BoolAtomType], -- for a more advanced typechecker, this should be BoolAtomType or SQLNullable BoolAtomType
      funcBody = FunctionBuiltInBody nullAnd
      }
    ]
  where
    sqlNull typ = ConstructedAtom "SQLNull" typ []
    sqlNullable val typ = ConstructedAtom "SQLJust" typ [val]
    isNull (ConstructedAtom dConsName _ _) | dConsName == "SQLNull" = True
    isNull _ = False
    nullEq :: AtomFunctionBodyType
    nullEq (a@(ConstructedAtom _ typA argsA) : b@(ConstructedAtom _ _ argsB) : [])
      | isNull a || isNull b = pure $ sqlNull typA
      | otherwise = pure $ sqlNullable (BoolAtom $ argsA == argsB) BoolAtomType
    nullEq _ = Left AtomFunctionTypeMismatchError

isSQLBool :: Atom -> Bool
isSQLBool (ConstructedAtom dConsName BoolAtomType [_]) | dConsName == "SQLNullable" = True
isSQLBool (BoolAtom _) = True
isSQLBool _ = False

sqlBool :: Atom -> Maybe Bool
sqlBool (ConstructedAtom dConsName BoolAtomType [BoolAtom tf]) | dConsName == "SQLJust" = Just tf
sqlBool (ConstructedAtom dConsName BoolAtomType []) | dConsName == "SQLNull" = Nothing
sqlBool (BoolAtom tf) = Just tf
sqlBool x | isSQLBool x = error "internal sqlBool type error" -- should be caught above
sqlBool _ = error "sqlBool type mismatch"


nullAnd :: [Atom] -> Either AtomFunctionError Atom
nullAnd [a,b] | isSQLBool a && isSQLBool b = do
                  let bNull = nullAtom BoolAtomType Nothing
                      boolF = nullAtom BoolAtomType (Just (BoolAtom False))
                  pure $ case (sqlBool a, sqlBool b) of
                             (Nothing, Nothing) -> bNull
                             (Nothing, Just True) -> bNull
                             (Nothing, Just False) -> boolF
                             (Just True, Nothing) -> bNull
                             (Just False, Nothing) -> boolF
                             (Just a', Just b') ->
                               nullAtom BoolAtomType (Just (BoolAtom (a' && b')))
nullAnd _ = Left AtomFunctionTypeMismatchError
                
nullAtom :: AtomType -> Maybe Atom -> Atom
nullAtom aType mAtom =
  case mAtom of
    Nothing -> ConstructedAtom "SQLNull" (nullAtomType aType) []
    Just atom -> ConstructedAtom "SQLJust" (nullAtomType aType) [atom]
