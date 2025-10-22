module ProjectM36.DataTypes.SQL.Null where
import ProjectM36.Base
import ProjectM36.AtomFunctionError
import qualified Data.Map as M
import qualified Data.HashSet as HS
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunction
import ProjectM36.Tuple
import ProjectM36.Relation
import Data.Maybe (isJust)
import Data.Text (Text)

-- analogous but not equivalent to a Maybe type due to how NULLs interact with every other value

nullAtomType :: AtomType -> AtomType
nullAtomType arg = ConstructedAtomType "SQLNullable" (M.singleton "a" arg)

nullTypeConstructorMapping :: TypeConstructorMapping
nullTypeConstructorMapping = [(ADTypeConstructorDef "SQLNullable" ["a"],
                                [DataConstructorDef "SQLNull" [],
                                 DataConstructorDef "SQLJust" [DataConstructorDefTypeVarNameArg "a"]]),
                               -- used in SQL conversion from in expressions such as INSERT INTO s(city) VALUES (NULL) where the query expression must defer type resolution to SQLNull.
                               (ADTypeConstructorDef "SQLNullOfUnknownType" [],
                                [DataConstructorDef "SQLNullOfUnknownType" []])
                              ]

nullAtomFunctions :: AtomFunctions
nullAtomFunctions = HS.fromList [
    Function {
      funcName = "sql_equals",
      funcType = [TypeVariableType "a",
                  TypeVariableType "b", -- either type could be SQLNullable or a NakedAtom
                  nullAtomType BoolAtomType],
      funcBody = FunctionBuiltInBody sqlEquals,
      funcACL = ()
      },
    Function {
      funcName = "sql_and",
      funcType = [TypeVariableType "a", TypeVariableType "b", nullAtomType BoolAtomType], -- for a more advanced typechecker, this should be BoolAtomType or SQLNullable BoolAtomType
      funcBody = FunctionBuiltInBody nullAnd,
      funcACL = ()
      },
    Function {
      funcName = "sql_or",
      funcType = [TypeVariableType "a", TypeVariableType "b", nullAtomType BoolAtomType], -- for a more advanced typechecker, this should be BoolAtomType or SQLNullable BoolAtomType
      funcBody = FunctionBuiltInBody nullOr,
      funcACL = ()
             },
    Function {
      funcName = "sql_coalesce_bool", -- used in where clause so that NULLs are filtered out
      funcType = [TypeVariableType "a",
                  BoolAtomType],
      funcBody = FunctionBuiltInBody coalesceBool,
      funcACL = ()
      },
    Function {
      funcName = "sql_add",
      funcType = [TypeVariableType "a", 
                  TypeVariableType "b",
                  nullAtomType IntegerAtomType],
      funcBody = FunctionBuiltInBody (sqlIntegerBinaryFunction IntegerAtomType (\a b -> IntegerAtom (a + b))),
      funcACL = ()
      },
    Function {
      funcName = "sql_abs",
      funcType = [TypeVariableType "a",
                  nullAtomType IntegerAtomType],
      funcBody = FunctionBuiltInBody sqlAbs,
      funcACL = ()
      },
    Function {
      funcName = "sql_negate",
      funcType = [TypeVariableType "a",
                  nullAtomType IntegerAtomType],
      funcBody = FunctionBuiltInBody (sqlIntegerUnaryFunction IntegerAtomType (\a -> IntegerAtom (- a))),
      funcACL = ()
      },
    Function {
      funcName = "sql_max",
      funcType = foldAtomFuncType (TypeVariableType "a") (nullAtomType IntegerAtomType),
      funcBody = FunctionBuiltInBody sqlMax,
      funcACL = ()
      },
    Function {
      funcName = "sql_min",
      funcType = foldAtomFuncType (TypeVariableType "a") (nullAtomType IntegerAtomType),
      funcBody = FunctionBuiltInBody sqlMin,
      funcACL = ()
      },
    Function {
      funcName = "sql_count",
      funcType = foldAtomFuncType (TypeVariableType "a") IntegerAtomType,
      funcBody = FunctionBuiltInBody sqlCount,
      funcACL = ()
      },
    Function {
      funcName = "sql_sum",
      funcType = foldAtomFuncType (TypeVariableType "a") (nullAtomType IntegerAtomType),
      funcBody = FunctionBuiltInBody sqlSum,
      funcACL = ()
      },
    Function {
      funcName = "sql_isnull",
      funcType = [TypeVariableType "a", BoolAtomType],
      funcBody = FunctionBuiltInBody sqlIsNull,
      funcACL = ()
      }
    ] <> sqlCompareFunctions


sqlCompareFunctions :: HS.HashSet AtomFunction
sqlCompareFunctions = HS.fromList $
  map mkFunc ops
  where
    mkFunc (sql_func, opi, opt) =
      Function {
      funcName = sql_func,
        funcType = [TypeVariableType "a", TypeVariableType "b", nullAtomType BoolAtomType],
        funcBody = FunctionBuiltInBody (sqlCompareFunc (opi, opt)),
        funcACL = ()
      }
    boolNull = nullAtom BoolAtomType Nothing
    sqlCompareFunc :: (Integer -> Integer -> Bool, Text -> Text -> Bool) -> [Atom] -> Either AtomFunctionError Atom
    sqlCompareFunc (opi, opt) [atomA, atomB] = 
      case (maybeFromAtom atomA, maybeFromAtom atomB) of
        (Nothing, _) -> pure boolNull
        (_, Nothing) -> pure boolNull
        (Just (IntegerAtom a), Just (IntegerAtom b)) -> pure $ nullAtom BoolAtomType (Just (BoolAtom (opi a b)))
        (Just (TextAtom a), Just (TextAtom b)) -> pure (nullAtom BoolAtomType (Just (BoolAtom (opt a b))))
        _ -> Left AtomFunctionTypeMismatchError
    sqlCompareFunc _ _ = Left AtomFunctionTypeMismatchError
    ops :: [(FunctionName,
              Integer -> Integer -> Bool,
              Text -> Text -> Bool)]
    ops = [("sql_gt", (>), (>)),
            ("sql_lt", (<), (<)),
            ("sql_gte", (>=), (>=)),
            ("sql_lte", (<=), (<=))
           ]

maybeFromAtom :: Atom -> Maybe Atom
maybeFromAtom atom | isNull atom = Nothing
maybeFromAtom atom = Just atom           

coalesceBool :: [Atom] -> Either AtomFunctionError Atom
coalesceBool [arg] = case sqlBool arg of
                       Nothing -> pure (BoolAtom False)
                       Just tf -> pure (BoolAtom tf)
coalesceBool _other = Left AtomFunctionTypeMismatchError                       

isSQLBool :: Atom -> Bool
isSQLBool atom = case atomTypeForAtom atom of
                   ConstructedAtomType "SQLNullable" _ -> True
                   ConstructedAtomType "SQLNullOfUnknownType" _ -> True
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
sqlBool (ConstructedAtom "SQLNullOfUnknownType" _ []) = Nothing
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

nullOr :: [Atom] -> Either AtomFunctionError Atom
nullOr [a,b] | isSQLBool a && isSQLBool b = do
                let bNull = nullAtom BoolAtomType Nothing
                    boolTF tf = nullAtom BoolAtomType (Just (BoolAtom tf))
                pure $ case (sqlBool a, sqlBool b) of
                  (Nothing, Nothing) -> bNull
                  (Nothing, Just True) -> boolTF True
                  (Nothing, Just False) -> bNull
                  (Just True, Nothing) -> boolTF True
                  (Just False, Nothing) -> bNull
                  (Just a', Just b') -> boolTF (a' || b')
nullOr _other = Left AtomFunctionTypeMismatchError                  
                
nullAtom :: AtomType -> Maybe Atom -> Atom
nullAtom aType mAtom =
  case mAtom of
    Nothing -> ConstructedAtom "SQLNull" (nullAtomType aType) []
    Just atom -> ConstructedAtom "SQLJust" (nullAtomType aType) [atom]

isNullOrType :: AtomType -> Atom -> Bool
isNullOrType aType atom = atomTypeForAtom atom == nullAtomType aType || atomTypeForAtom atom == aType

isNull :: Atom -> Bool
isNull (ConstructedAtom "SQLNull" (ConstructedAtomType "SQLNullable" _) []) = True
isNull (ConstructedAtom "SQLNullOfUnknownType" (ConstructedAtomType "SQLNullOfUnknownType" _) []) = True
isNull _ = False

isNullAtomType :: AtomType -> Bool
isNullAtomType = isJust . atomTypeFromSQLNull

atomTypeFromSQLNull :: AtomType -> Maybe AtomType
atomTypeFromSQLNull (ConstructedAtomType "SQLNullOfUnknownType" _) = Nothing
atomTypeFromSQLNull (ConstructedAtomType "SQLNullable" vars)
  | M.size vars == 1 =
    case M.elems vars of
      [] -> Nothing
      [t] -> Just t
      _ts -> Nothing
atomTypeFromSQLNull _ = Nothing

sqlIntegerBinaryFunction :: AtomType -> (Integer -> Integer -> Atom) -> [Atom] -> Either AtomFunctionError Atom
sqlIntegerBinaryFunction expectedAtomType op [a,b] 
  | isNullOrType IntegerAtomType a && isNullOrType IntegerAtomType b = do
    let extractVal (ConstructedAtom "SQLJust" _ [IntegerAtom val]) = pure val
        extractVal (IntegerAtom val) = pure val
        extractVal (ConstructedAtom "SQLNull" _ []) = Nothing
        extractVal _ = Nothing
        mValA = extractVal a
        mValB = extractVal b
        inull = nullAtom expectedAtomType Nothing
    case (mValA, mValB) of
      (Nothing, Nothing) -> pure inull
      (Nothing, _) -> pure inull
      (_, Nothing) -> pure inull
      (Just valA, Just valB) -> pure (nullAtom expectedAtomType (Just (op valA valB)))
sqlIntegerBinaryFunction _ _ _ = Left AtomFunctionTypeMismatchError 

sqlIntegerUnaryFunction :: AtomType -> (Integer -> Atom) -> [Atom] -> Either AtomFunctionError Atom
sqlIntegerUnaryFunction expectedAtomType op [x]
  | isNullOrType IntegerAtomType x =
    case x of
      n@(ConstructedAtom "SQLNull" _ []) -> pure n
      ConstructedAtom "SQLJust" _ [IntegerAtom val] -> pure (nullAtom expectedAtomType (Just (op val)))
      IntegerAtom val -> pure (nullAtom expectedAtomType (Just (op val)))
      _other -> Left AtomFunctionTypeMismatchError
sqlIntegerUnaryFunction _ _ _ = Left AtomFunctionTypeMismatchError       

sqlCount :: [Atom] -> Either AtomFunctionError Atom
sqlCount [RelationAtom relIn] =
  case cardinality relIn of
    Finite c -> pure $ IntegerAtom (toInteger c)
    Countable -> Left AtomFunctionTypeMismatchError
sqlCount _ = Left AtomFunctionTypeMismatchError

sqlAbs :: [Atom] -> Either AtomFunctionError Atom
sqlAbs [IntegerAtom val] = pure $ IntegerAtom (abs val)
sqlAbs [arg] | arg == nullAtom IntegerAtomType Nothing =
               pure $ nullAtom IntegerAtomType Nothing
sqlAbs [ConstructedAtom "SQLJust" aType [IntegerAtom val]]
  | aType == nullAtomType IntegerAtomType =
            pure $ nullAtom IntegerAtomType (Just (IntegerAtom (abs val)))
sqlAbs _other = Left AtomFunctionTypeMismatchError         

sqlMax :: [Atom] -> Either AtomFunctionError Atom
sqlMax = sqlIntegerAgg max

sqlMin :: [Atom] -> Either AtomFunctionError Atom
sqlMin = sqlIntegerAgg min

sqlSum :: [Atom] -> Either AtomFunctionError Atom
sqlSum = sqlIntegerAgg (+)

sqlIntegerAgg :: (Integer -> Integer -> Integer) -> [Atom] -> Either AtomFunctionError Atom
sqlIntegerAgg op [SubrelationFoldAtom relIn subAttr] =
  case oneTuple relIn of
    Nothing -> pure $ nullAtom IntegerAtomType Nothing -- SQL max/min of empty table is NULL
    Just oneTup ->
      if atomTypeForAtom (newVal oneTup) /= IntegerAtomType then
        Left AtomFunctionTypeMismatchError
        else
        pure $ relFold (\tupIn acc -> nullMax acc (newVal tupIn)) (newVal oneTup) relIn
 where
   newVal tupIn =
      case atomForAttributeName subAttr tupIn of
        Left err -> error (show err)
        Right atom -> atom
   nullMax acc nextVal =
     let mNextVal = sqlNullableIntegerToMaybe nextVal
         mOldVal = sqlNullableIntegerToMaybe acc
         mResult = op <$> mNextVal <*> mOldVal
         in
       nullAtom IntegerAtomType (case mResult of
                                    Nothing -> Nothing
                                    Just v -> Just (IntegerAtom v))
sqlIntegerAgg _ _ = Left AtomFunctionTypeMismatchError       
       

sqlNullableIntegerToMaybe :: Atom -> Maybe Integer
sqlNullableIntegerToMaybe (IntegerAtom i) = Just i
sqlNullableIntegerToMaybe (ConstructedAtom "SQLJust" aType [IntegerAtom i]) | aType == nullAtomType IntegerAtomType = Just i
sqlNullableIntegerToMaybe (ConstructedAtom "SQLNull" aType []) | aType == nullAtomType IntegerAtomType = Nothing
sqlNullableIntegerToMaybe (ConstructedAtom "SQLNullOfUnknownType" aType []) | aType == nullAtomType IntegerAtomType = Nothing
sqlNullableIntegerToMaybe _ = Nothing
           
-- check that types check out- Int and SQLNullable Int are OK, Int and SQLNullable Text are not OK
sqlEqualsTypes :: Atom -> Atom -> Bool
sqlEqualsTypes a b = underlyingType a == underlyingType b
  where
    underlyingType atom =
      let def = atomTypeForAtom atom 
      in  case atom of
            ConstructedAtom x (ConstructedAtomType "SQLNullable" typmap) y ->
              let getSingle = case M.elems typmap of
                    [z] -> z
                    _ -> def 
              in  case (x, y) of
                    ("SQLNull", []) -> getSingle 
                    ("SQLJust", _) -> getSingle
                    _ -> def
            _ -> def

sqlEquals :: AtomFunctionBodyType
sqlEquals [a,b] | sqlEqualsTypes a b =
  case (maybeNullAtom a, maybeNullAtom b) of
    (Nothing, _) -> pure $ nullAtom BoolAtomType Nothing
    (_, Nothing) -> pure $ nullAtom BoolAtomType Nothing
    (Just a', Just b') -> pure $ nullAtom BoolAtomType (Just (BoolAtom $ a' == b'))
  where
      maybeNullAtom (ConstructedAtom "SQLJust" (ConstructedAtomType "SQLNullable" _) [atom]) = Just atom
      maybeNullAtom (ConstructedAtom "SQLNull" _ []) = Nothing
      maybeNullAtom other = Just other
sqlEquals _other = Left AtomFunctionTypeMismatchError      

sqlIsNull :: AtomFunctionBodyType
sqlIsNull [ConstructedAtom "SQLNull" (ConstructedAtomType "SQLNullable" _) []] = pure (BoolAtom True)
sqlIsNull [_arg] = pure (BoolAtom False)
sqlIsNull _other = Left AtomFunctionTypeMismatchError

isSQLNullableType :: AtomType -> Bool
isSQLNullableType (ConstructedAtomType "SQLNullable" _) = True
isSQLNullableType _ = False

isSQLNullableSpecificType :: AtomType -> AtomType -> Bool
isSQLNullableSpecificType (ConstructedAtomType "SQLNullable" vars) expectedType | M.elems vars == [expectedType] = True
isSQLNullableSpecificType _ _ = False

isSQLNullUnknownType :: AtomType -> Bool
isSQLNullUnknownType t = t == ConstructedAtomType "SQLNullOfUnknownType" mempty
