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
           _ -> error "isnull" -- $ Left AtomFunctionTypeMismatchError
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
      funcType = [TypeVariableType "a", TypeVariableType "b", nullAtomType BoolAtomType], -- for a more advanced typechecker, this should be BoolAtomType or SQLNullable BoolAtomType
      funcBody = FunctionBuiltInBody nullAnd
      },
    Function {
      funcName = "sql_coalesce_bool", -- used in where clause so that NULLs are filtered out
      funcType = [TypeVariableType "a",
                  BoolAtomType],
      funcBody = FunctionBuiltInBody coalesceBool
      },
    Function {
      funcName = "sql_add",
      funcType = [TypeVariableType "a", 
                  TypeVariableType "b",
                  nullAtomType IntegerAtomType],
      funcBody = FunctionBuiltInBody (sqlIntegerBinaryFunction IntegerAtomType (\a b -> IntegerAtom (a + b)))
      },
    Function {
      funcName = "sql_gt",
      funcType = [TypeVariableType "a",
                  TypeVariableType "b",
                  nullAtomType BoolAtomType],
      funcBody = FunctionBuiltInBody (sqlIntegerBinaryFunction BoolAtomType (\a b -> BoolAtom (a > b)))
      },
    Function {
      funcName = "sql_gte",
      funcType = [TypeVariableType "a",
                  TypeVariableType "b",
                  nullAtomType IntegerAtomType],
      funcBody = FunctionBuiltInBody (sqlIntegerBinaryFunction BoolAtomType (\a b -> BoolAtom (a >= b)))
      }
    
    ]
  where
    sqlNull typ = ConstructedAtom "SQLNull" typ []
    sqlNullable val typ = ConstructedAtom "SQLJust" (nullAtomType typ) [val]
    nullEq :: AtomFunctionBodyType
    nullEq (a@(ConstructedAtom _ typA argsA) : b@(ConstructedAtom _ _ argsB) : [])
      | isNull a || isNull b = pure $ sqlNull typA
      | otherwise = pure $ sqlNullable (BoolAtom $ argsA == argsB) BoolAtomType
    nullEq [a,b] | atomTypeForAtom a == atomTypeForAtom b = pure (sqlNullable (BoolAtom (a == b)) BoolAtomType)
    nullEq _other = Left AtomFunctionTypeMismatchError

coalesceBool :: [Atom] -> Either AtomFunctionError Atom
coalesceBool [arg] = case sqlBool arg of
                       Nothing -> pure (BoolAtom False)
                       Just tf -> pure (BoolAtom tf)
coalesceBool _other = Left AtomFunctionTypeMismatchError                       

isSQLBool :: Atom -> Bool
isSQLBool atom = case atomTypeForAtom atom of
                   ConstructedAtomType "SQLNullable" _ -> True
                   BoolAtomType -> True
                   _ -> False

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
nullAnd _other = Left AtomFunctionTypeMismatchError
                
nullAtom :: AtomType -> Maybe Atom -> Atom
nullAtom aType mAtom =
  case mAtom of
    Nothing -> ConstructedAtom "SQLNull" (nullAtomType aType) []
    Just atom -> ConstructedAtom "SQLJust" (nullAtomType aType) [atom]

isNullOrType :: AtomType -> Atom -> Bool
isNullOrType aType atom = atomTypeForAtom atom == nullAtomType aType || atomTypeForAtom atom == aType

isNull :: Atom -> Bool
isNull (ConstructedAtom "SQLNull" (ConstructedAtomType "SQLNullable" _) []) = True
isNull _ = False

sqlIntegerBinaryFunction :: AtomType -> (Integer -> Integer -> Atom) -> [Atom] -> Either AtomFunctionError Atom
sqlIntegerBinaryFunction expectedAtomType op [a,b] 
  | isNullOrType IntegerAtomType a && isNullOrType IntegerAtomType b =
    case (a,b) of
      (IntegerAtom valA, IntegerAtom valB) -> do
        let res = op valA valB
        pure (nullAtom expectedAtomType (Just res))
      (a',b') | isNull a' || isNull b' -> pure (nullAtom expectedAtomType Nothing)
      _other -> Left AtomFunctionTypeMismatchError
sqlIntegerBinaryFunction _ _ _ = Left AtomFunctionTypeMismatchError 
      
