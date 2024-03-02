module ProjectM36.DataTypes.SQL.Null where
import ProjectM36.Base
import ProjectM36.AtomFunctionError
import qualified Data.Map as M
import qualified Data.HashSet as HS
import ProjectM36.DataTypes.Primitive

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
      funcType = [TypeVariableType "a",
                  TypeVariableType "a",
                  nullAtomType BoolAtomType],
      funcBody = FunctionBuiltInBody nullEq
      },
    Function {
      funcName = "sql_and",
      funcType = [TypeVariableType "a", TypeVariableType "b", BoolAtomType], -- for a more advanced typechecker, this should be BoolAtomType or SQLNullable BoolAtomType
      funcBody = FunctionBuiltInBody nullAnd
      },
    Function {
      funcName = "sql_coalesce_bool", -- used in where clause so that NULLs are filtered out
      funcType = [TypeVariableType "a",
                  BoolAtomType],
      funcBody = FunctionBuiltInBody coalesceBool
      }
    ]
  where
    sqlNull typ = ConstructedAtom "SQLNull" typ []
    sqlNullable val typ = ConstructedAtom "SQLJust" (nullAtomType typ) [val]
    isNull (ConstructedAtom dConsName _ _) | dConsName == "SQLNull" = True
    isNull _ = False
    nullEq :: AtomFunctionBodyType
    nullEq (a@(ConstructedAtom _ typA argsA) : b@(ConstructedAtom _ _ argsB) : [])
      | isNull a || isNull b = pure $ sqlNull typA
      | otherwise = pure $ sqlNullable (BoolAtom $ argsA == argsB) BoolAtomType
    nullEq (a:b:[]) | atomTypeForAtom a == atomTypeForAtom b = pure (sqlNullable (BoolAtom (a == b)) BoolAtomType)
    nullEq _ = Left AtomFunctionTypeMismatchError

coalesceBool :: [Atom] -> Either AtomFunctionError Atom
coalesceBool [arg] = case sqlBool arg of
                       Nothing -> pure (BoolAtom False)
                       Just tf -> pure (BoolAtom tf)
coalesceBool _ = Left AtomFunctionTypeMismatchError                       

isSQLBool :: Atom -> Bool
isSQLBool (ConstructedAtom dConsName BoolAtomType [_]) | dConsName == "SQLNullable" = True
isSQLBool (BoolAtom _) = True
isSQLBool _ = False

sqlBool :: Atom -> Maybe Bool
sqlBool (ConstructedAtom dConsName aType [BoolAtom tf]) |
  dConsName == "SQLJust" &&
  (aType == nullAtomType BoolAtomType ||
   aType == nullAtomType (TypeVariableType "a")) = Just tf
sqlBool (ConstructedAtom dConsName aType []) |
  dConsName == "SQLNull" &&
  (aType == nullAtomType BoolAtomType ||
   aType == nullAtomType (TypeVariableType "a")) = Nothing
sqlBool (BoolAtom tf) = Just tf
sqlBool x | isSQLBool x = error "internal sqlBool type error" -- should be caught above
sqlBool other = error ("sqlBool type mismatch: " <> show other)

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
