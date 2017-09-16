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
  AtomFunction { atomFuncName = "add",
                 atomFuncType = [IntAtomType, IntAtomType, IntAtomType],
                 atomFuncBody = body (\(IntAtom i1:IntAtom i2:_) -> pure (IntAtom (i1 + i2)))},
  AtomFunction { atomFuncName = "id",
                 atomFuncType = [TypeVariableType "a", TypeVariableType "a"],
                 atomFuncBody = body (\(x:_) -> pure x)},
  AtomFunction { atomFuncName = "sum",
                 atomFuncType = foldAtomFuncType IntAtomType IntAtomType,
                 atomFuncBody = body (\(RelationAtom rel:_) -> relationSum rel)},
  AtomFunction { atomFuncName = "count",
                 atomFuncType = foldAtomFuncType (TypeVariableType "a") IntAtomType,
                 atomFuncBody = body (\(RelationAtom relIn:_) -> relationCount relIn)},
  AtomFunction { atomFuncName = "max",
                 atomFuncType = foldAtomFuncType IntAtomType IntAtomType,
                 atomFuncBody = body (\(RelationAtom relIn:_) -> relationMax relIn)},
  AtomFunction { atomFuncName = "min",
                 atomFuncType = foldAtomFuncType IntAtomType IntAtomType,
                 atomFuncBody = body (\(RelationAtom relIn:_) -> relationMin relIn)},
  AtomFunction { atomFuncName = "lt",
                 atomFuncType = [IntAtomType, IntAtomType, BoolAtomType],
                 atomFuncBody = body $ intAtomFuncLessThan False},
  AtomFunction { atomFuncName = "lte",
                 atomFuncType = [IntAtomType, IntAtomType, BoolAtomType],
                 atomFuncBody = body $ intAtomFuncLessThan True},
  AtomFunction { atomFuncName = "gte",
                 atomFuncType = [IntAtomType, IntAtomType, BoolAtomType],
                 atomFuncBody = body $ intAtomFuncLessThan False >=> boolAtomNot},
  AtomFunction { atomFuncName = "gt",
                 atomFuncType = [IntAtomType, IntAtomType, BoolAtomType],
                 atomFuncBody = body $ intAtomFuncLessThan True >=> boolAtomNot},
  AtomFunction { atomFuncName = "not",
                 atomFuncType = [BoolAtomType, BoolAtomType],
                 atomFuncBody = body $ \(b:_) -> boolAtomNot b }  
  ]
  where
    body = AtomFunctionBody Nothing
                         
intAtomFuncLessThan :: Bool -> [Atom] -> Either AtomFunctionError Atom
intAtomFuncLessThan equality (IntAtom i1:IntAtom i2:_) = pure (BoolAtom (i1 `op` i2))
  where
    op = if equality then (<=) else (<)
intAtomFuncLessThan _ _= pure (BoolAtom False)

boolAtomNot :: Atom -> Either AtomFunctionError Atom
boolAtomNot (BoolAtom b) = pure (BoolAtom (not b))
boolAtomNot _ = error "boolAtomNot called on non-Bool atom"

--used by sum atom function
relationSum :: Relation -> Either AtomFunctionError Atom
relationSum relIn = pure (IntAtom (relFold (\tupIn acc -> acc + newVal tupIn) 0 relIn))
  where
    --extract Int from Atom
    newVal :: RelationTuple -> Integer
    newVal tupIn = castInt (tupleAtoms tupIn V.! 0)
    
relationCount :: Relation -> Either AtomFunctionError Atom
relationCount relIn = pure (IntAtom (relFold (\_ acc -> acc + 1) (0::Integer) relIn))

relationMax :: Relation -> Either AtomFunctionError Atom
relationMax relIn = case oneTuple relIn of
    Nothing -> Left AtomFunctionEmptyRelationError
    Just oneTup -> pure (IntAtom (relFold (\tupIn acc -> max acc (newVal tupIn)) (newVal oneTup) relIn))
  where
    newVal tupIn = castInt (tupleAtoms tupIn V.! 0)

relationMin :: Relation -> Either AtomFunctionError Atom
relationMin relIn = case oneTuple relIn of 
  Nothing -> Left AtomFunctionEmptyRelationError
  Just oneTup -> pure (IntAtom (relFold (\tupIn acc -> min acc (newVal tupIn)) (newVal oneTup) relIn))
  where
    newVal tupIn = castInt (tupleAtoms tupIn V.! 0)

castInt :: Atom -> Integer
castInt (IntAtom i) = i
castInt _ = error "attempted to cast non-IntAtom to Int"