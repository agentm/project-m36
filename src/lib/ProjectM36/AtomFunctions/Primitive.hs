module ProjectM36.AtomFunctions.Primitive where
import ProjectM36.Base
import ProjectM36.Relation (relFold, oneTuple)
import ProjectM36.Tuple
import ProjectM36.Attribute
import ProjectM36.AtomFunctionError
import ProjectM36.AtomFunction
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Control.Monad
import Data.List (sort)

primitiveAtomFunctions :: AtomFunctions
primitiveAtomFunctions = HS.fromList [
  --match on any relation type
  AtomFunction { atomFuncName = "add",
                 atomFuncType = [IntegerAtomType, IntegerAtomType, IntegerAtomType],
                 atomFuncBody = body (\(IntegerAtom i1:IntegerAtom i2:_) -> pure (IntegerAtom (i1 + i2)))},
  AtomFunction { atomFuncName = "id",
                 atomFuncType = [TypeVariableType "a", TypeVariableType "a"],
                 atomFuncBody = body (\(x:_) -> pure x)},
  AtomFunction { atomFuncName = "sum",
                 atomFuncType = foldAtomFuncType IntegerAtomType IntegerAtomType,
                 atomFuncBody = body (\(RelationAtom rel:_) -> relationSum rel)},
  AtomFunction { atomFuncName = "count",
                 atomFuncType = foldAtomFuncType (TypeVariableType "a") IntegerAtomType,
                 atomFuncBody = body (\(RelationAtom relIn:_) -> relationCount relIn)},
  AtomFunction { atomFuncName = "max",
                 atomFuncType = foldAtomFuncType IntegerAtomType IntegerAtomType,
                 atomFuncBody = body (\(RelationAtom relIn:_) -> relationMax relIn)},
  AtomFunction { atomFuncName = "min",
                 atomFuncType = foldAtomFuncType IntegerAtomType IntegerAtomType,
                 atomFuncBody = body (\(RelationAtom relIn:_) -> relationMin relIn)},
  AtomFunction { atomFuncName = "lt",
                 atomFuncType = [IntegerAtomType, IntegerAtomType, BoolAtomType],
                 atomFuncBody = body $ integerAtomFuncLessThan False},
  AtomFunction { atomFuncName = "lte",
                 atomFuncType = [IntegerAtomType, IntegerAtomType, BoolAtomType],
                 atomFuncBody = body $ integerAtomFuncLessThan True},
  AtomFunction { atomFuncName = "gte",
                 atomFuncType = [IntegerAtomType, IntegerAtomType, BoolAtomType],
                 atomFuncBody = body $ integerAtomFuncLessThan False >=> boolAtomNot},
  AtomFunction { atomFuncName = "gt",
                 atomFuncType = [IntegerAtomType, IntegerAtomType, BoolAtomType],
                 atomFuncBody = body $ integerAtomFuncLessThan True >=> boolAtomNot},
  AtomFunction { atomFuncName = "not",
                 atomFuncType = [BoolAtomType, BoolAtomType],
                 atomFuncBody = body $ \(b:_) -> boolAtomNot b },
  AtomFunction { atomFuncName = "int",
                 atomFuncType = [IntegerAtomType, IntAtomType],
                 atomFuncBody = body $ \(IntegerAtom v:_) -> if v < fromIntegral (maxBound :: Int) then
                                                   pure (IntAtom (fromIntegral v))
                                                 else
                                                   Left InvalidIntBoundError
                                                   },
  AtomFunction { atomFuncName = "groupConsecutiveIntegers",
                 atomFuncType = foldAtomFuncType IntegerAtomType (RelationAtomType (attributesFromList [Attribute "rel" (RelationAtomType (attributesFromList [Attribute "value" IntegerAtomType]))])),
                 atomFuncBody = body (\(RelationAtom relIn:_) -> groupConsecutiveIntegers relIn) }
  ]
  where
    body = AtomFunctionBody Nothing
                         
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

--instead of grouping integers by equality, group them in a single group if they are consecutive
groupConsecutiveIntegers :: Relation -> Either AtomFunctionError Atom
groupConsecutiveIntegers relIn = 
  pure $ RelationAtom (Relation relAttrs (RelationTupleSet tups))
  where
    sortedInts = sort $ relFold (\tupIn acc -> newVal tupIn : acc) [] relIn
    consecutiveInts = foldr seqInt [] sortedInts
    --split up consecutive lists of ints
    seqInt v [] = [[v]]
    seqInt v (xs@(x:_):ys) | x == succ v = (v:xs):ys
                      | x == v = xs:ys
                      | otherwise = [v]:xs:ys
    seqInt _ ([]:_) = error "invalid sorted list"
    newVal tupIn = castInteger (tupleAtoms tupIn V.! 0)
    --create relation of split ints
    relAttrs = attributesFromList [Attribute "rel" (RelationAtomType attrs)]
    attrs = attributesFromList [Attribute "value" IntegerAtomType]
    tups = map mapper consecutiveInts
    mapper vs = RelationTuple relAttrs (V.singleton (RelationAtom (Relation attrs (RelationTupleSet (mkTuplesFromIntList vs)))))
    mkTuplesFromIntList vs = map (\v -> RelationTuple attrs (V.singleton (IntegerAtom v))) vs

