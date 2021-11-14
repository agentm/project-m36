module ProjectM36.AtomFunctions.Primitive where
import ProjectM36.Base
import ProjectM36.Relation (relFold, oneTuple)
import ProjectM36.Tuple
import ProjectM36.AtomFunctionError
import ProjectM36.AtomFunction
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Control.Monad

primitiveAtomFunctions :: AtomFunctions
primitiveAtomFunctions = HS.fromList [
  --match on any relation type
  Function { funcName = "add",
             funcType = [IntegerAtomType, IntegerAtomType, IntegerAtomType],
             funcBody = body (\(IntegerAtom i1:IntegerAtom i2:_) -> pure (IntegerAtom (i1 + i2)))},
    Function { funcName = "id",
               funcType = [TypeVariableType "a", TypeVariableType "a"],
               funcBody = body (\(x:_) -> pure x)},
    Function { funcName = "sum",
               funcType = foldAtomFuncType IntegerAtomType IntegerAtomType,
               funcBody = body (\(RelationAtom rel:_) -> relationSum rel)},
    Function { funcName = "count",
               funcType = foldAtomFuncType (TypeVariableType "a") IntegerAtomType,
               funcBody = body (\(RelationAtom relIn:_) -> relationCount relIn)},
    Function { funcName = "max",
               funcType = foldAtomFuncType IntegerAtomType IntegerAtomType,
               funcBody = body (\(RelationAtom relIn:_) -> relationMax relIn)},
    Function { funcName = "min",
               funcType = foldAtomFuncType IntegerAtomType IntegerAtomType,
               funcBody = body (\(RelationAtom relIn:_) -> relationMin relIn)},
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
               funcBody = body $ \(b:_) -> boolAtomNot b },
    Function { funcName = "int",
               funcType = [IntegerAtomType, IntAtomType],
               funcBody =
                 body $ \(IntegerAtom v:_) ->
                          if v < fromIntegral (maxBound :: Int) then
                            
                            pure (IntAtom (fromIntegral v))
                          else
                            Left InvalidIntBoundError
             }
  ]
  where
    body = FunctionBuiltInBody
                         
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
