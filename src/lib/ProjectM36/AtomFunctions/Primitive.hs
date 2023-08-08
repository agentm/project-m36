module ProjectM36.AtomFunctions.Primitive where
import ProjectM36.Base
import ProjectM36.Relation (relFold, oneTuple)
import ProjectM36.Tuple
import ProjectM36.AtomFunctionError
import ProjectM36.AtomFunction
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Control.Monad
import qualified Data.UUID as U
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as APT
import Data.Scientific

primitiveAtomFunctions :: AtomFunctions
primitiveAtomFunctions = HS.fromList [
  --match on any relation type
  Function { funcName = "add",
             funcType = [IntegerAtomType, IntegerAtomType, IntegerAtomType],
             funcBody = body (\case
                                 IntegerAtom i1:IntegerAtom i2:_ -> pure (IntegerAtom (i1 + i2))
                                 _ -> Left AtomFunctionTypeMismatchError)},
    Function { funcName = "id",
               funcType = [TypeVariableType "a", TypeVariableType "a"],
               funcBody = body (\case
                                   x:_ -> pure x
                                   _ -> Left AtomFunctionTypeMismatchError
                               )},
    Function { funcName = "sum",
               funcType = foldAtomFuncType IntegerAtomType IntegerAtomType,
               funcBody = body $ relationAtomFunc relationSum
             },
    Function { funcName = "count",
               funcType = foldAtomFuncType (TypeVariableType "a") IntegerAtomType,
               funcBody = body $ relationAtomFunc relationCount
             },
    Function { funcName = "max",
               funcType = foldAtomFuncType IntegerAtomType IntegerAtomType,
               funcBody = body $ relationAtomFunc relationMax 
             },
    Function { funcName = "min",
               funcType = foldAtomFuncType IntegerAtomType IntegerAtomType,
               funcBody = body $ relationAtomFunc relationMin
             },
    Function { funcName = "eq",
               funcType = [TypeVariableType "a", TypeVariableType "a", BoolAtomType],
               funcBody = body $ \case
                                         [i1,i2] -> pure (BoolAtom (i1 == i2))
                                         _ -> Left AtomFunctionTypeMismatchError
             },
    Function { funcName = "lt",
               funcType = [IntegerAtomType, IntegerAtomType, BoolAtomType],
               funcBody = body $ integerAtomFuncLessThan False},
    Function { funcName = "lte",
               funcType = [IntegerAtomType, IntegerAtomType, BoolAtomType],
               funcBody = body $ integerAtomFuncLessThan True},
    Function { funcName = "gte",
               funcType = [IntegerAtomType, IntegerAtomType, BoolAtomType],
               funcBody = body $ integerAtomFuncLessThan False >=> boolAtomNot},
    Function { funcName = "gt",
               funcType = [IntegerAtomType, IntegerAtomType, BoolAtomType],
               funcBody = body $ integerAtomFuncLessThan True >=> boolAtomNot},
    Function { funcName = "not",
               funcType = [BoolAtomType, BoolAtomType],
               funcBody = body $ \case
                                         [b] -> boolAtomNot b
                                         _ -> Left AtomFunctionTypeMismatchError
             },
    Function { funcName = "int",
               funcType = [IntegerAtomType, IntAtomType],
               funcBody =
                 body $ \case
                                [IntegerAtom v] ->
                                  if v < fromIntegral (maxBound :: Int) then
                                    pure (IntAtom (fromIntegral v))
                                  else
                                    Left InvalidIntBoundError
                                _ -> Left AtomFunctionTypeMismatchError
             },
    Function { funcName = "integer",
               funcType = [IntAtomType, IntegerAtomType],
               funcBody = body $ \case
                 [IntAtom v] -> Right $ IntegerAtom $ fromIntegral v
                 _ -> Left AtomFunctionTypeMismatchError
             },
    Function { funcName = "uuid",
               funcType = [TextAtomType, UUIDAtomType],
               funcBody = body $ \case
                 [TextAtom v] ->
                   let mUUID = U.fromString (T.unpack v) in
                     case mUUID of
                       Just u -> pure $ UUIDAtom u
                       Nothing -> Left $ InvalidUUIDString v
                 _ -> Left AtomFunctionTypeMismatchError
             },
    Function { funcName = "and",
               funcType = [BoolAtomType, BoolAtomType, BoolAtomType],
               funcBody = body $ \case
                 [BoolAtom b1, BoolAtom b2] ->
                   Right $ BoolAtom (b1 && b2)
             },
    Function { funcName = "or",
               funcType = [BoolAtomType, BoolAtomType, BoolAtomType],
               funcBody = body $ \case
                 [BoolAtom b1, BoolAtom b2] ->
                   Right $ BoolAtom (b1 || b2)
             }
    
  ] <> scientificAtomFunctions
  where
    body = FunctionBuiltInBody
    relationAtomFunc f [RelationAtom x] = f x
    relationAtomFunc _ _ = Left AtomFunctionTypeMismatchError
                         
integerAtomFuncLessThan :: Bool -> [Atom] -> Either AtomFunctionError Atom
integerAtomFuncLessThan equality (IntegerAtom i1:IntegerAtom i2:_) = pure (BoolAtom (i1 `op` i2))
  where
    op = if equality then (<=) else (<)
integerAtomFuncLessThan _ _= pure (BoolAtom False)

boolAtomNot :: Atom -> Either AtomFunctionError Atom
boolAtomNot (BoolAtom b) = pure (BoolAtom (not b))
boolAtomNot _ = error "boolAtomNot called on non-Bool atom"

--used by sum atom function
relationSum :: Relation -> Either AtomFunctionError Atom
relationSum relIn = pure (IntegerAtom (relFold (\tupIn acc -> acc + newVal tupIn) 0 relIn))
  where
    --extract Integer from Atom
    newVal tupIn = castInteger (tupleAtoms tupIn V.! 0)
    
relationCount :: Relation -> Either AtomFunctionError Atom
relationCount relIn = pure (IntegerAtom (relFold (\_ acc -> acc + 1) (0::Integer) relIn))

relationMax :: Relation -> Either AtomFunctionError Atom
relationMax relIn = case oneTuple relIn of
    Nothing -> Left AtomFunctionEmptyRelationError
    Just oneTup -> pure (IntegerAtom (relFold (\tupIn acc -> max acc (newVal tupIn)) (newVal oneTup) relIn))
  where
    newVal tupIn = castInteger (tupleAtoms tupIn V.! 0)

relationMin :: Relation -> Either AtomFunctionError Atom
relationMin relIn = case oneTuple relIn of 
  Nothing -> Left AtomFunctionEmptyRelationError
  Just oneTup -> pure (IntegerAtom (relFold (\tupIn acc -> min acc (newVal tupIn)) (newVal oneTup) relIn))
  where
    newVal tupIn = castInteger (tupleAtoms tupIn V.! 0)

castInt :: Atom -> Int
castInt (IntAtom i) = i
castInt _ = error "attempted to cast non-IntAtom to Int"

castInteger :: Atom -> Integer
castInteger (IntegerAtom i) = i 
castInteger _ = error "attempted to cast non-IntegerAtom to Int"


scientificAtomFunctions :: AtomFunctions
scientificAtomFunctions = HS.fromList [
  Function { funcName = "read_scientific",
             funcType = [TextAtomType, ScientificAtomType],
             funcBody = body $ \case
               TextAtom t:_ ->
                 case APT.parseOnly (APT.scientific <* APT.endOfInput) t of
                   Left err -> Left (AtomFunctionParseError err)
                   Right sci -> pure (ScientificAtom sci)
               _ -> Left AtomFunctionTypeMismatchError
           },
  Function { funcName = "scientific",
             funcType = [IntegerAtomType, IntAtomType, ScientificAtomType],
             funcBody = body $ \case
               [IntegerAtom c,IntAtom e] -> pure (ScientificAtom $ scientific c e)
               _ -> Left AtomFunctionTypeMismatchError
           },
  Function { funcName = "scientific_add",
             funcType = binaryFuncType,
             funcBody = binaryFuncBody (+)
           },
  Function { funcName = "scientific_subtract",
             funcType = binaryFuncType,
             funcBody = binaryFuncBody (-)
           },
  Function { funcName = "scientific_multiply",
             funcType = binaryFuncType,
             funcBody = binaryFuncBody (*)
           },
  Function { funcName = "scientific_divide",
             funcType = binaryFuncType,
             funcBody = binaryFuncBody (/)
           }
  ]
  where body = FunctionBuiltInBody
        binaryFuncType = [ScientificAtomType, ScientificAtomType, ScientificAtomType]
        binaryFuncBody op = body $ \case
          [ScientificAtom s1, ScientificAtom s2] -> pure (ScientificAtom (s1 `op` s2))
          _ -> Left AtomFunctionTypeMismatchError
