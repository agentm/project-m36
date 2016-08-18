module ProjectM36.AtomFunctions.Primitive where
import ProjectM36.Base
import ProjectM36.Relation (relFold)
import ProjectM36.Tuple
import ProjectM36.AtomFunction
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE

primitiveAtomFunctions :: AtomFunctions
primitiveAtomFunctions = HS.fromList [
  --match on any relation type
  AtomFunction { atomFuncName = "add",
                 atomFuncType = [IntAtomType, IntAtomType, IntAtomType],
                 atomFuncBody = body (\((IntAtom i1):(IntAtom i2):_) -> IntAtom $  i1 + i2)},
  AtomFunction { atomFuncName = "id",
                 atomFuncType = [AnyAtomType, AnyAtomType],
                 atomFuncBody = body (\(x:_) -> x)},
  AtomFunction { atomFuncName = "sum",
                 atomFuncType = foldAtomFuncType IntAtomType IntAtomType,
                 atomFuncBody = body (\(RelationAtom rel:_) -> relationSum rel)},
  AtomFunction { atomFuncName = "count",
                 atomFuncType = foldAtomFuncType AnyAtomType IntAtomType,
                 atomFuncBody = body (\((RelationAtom relIn):_) -> relationCount relIn)},
  AtomFunction { atomFuncName = "max",
                 atomFuncType = foldAtomFuncType IntAtomType IntAtomType,
                 atomFuncBody = body (\((RelationAtom relIn):_) -> relationMax relIn)},
  AtomFunction { atomFuncName = "min",
                 atomFuncType = foldAtomFuncType IntAtomType IntAtomType,
                 atomFuncBody = body (\((RelationAtom relIn):_) -> relationMin relIn)},
  AtomFunction { atomFuncName = "lt",
                 atomFuncType = [IntAtomType, IntAtomType, BoolAtomType],
                 atomFuncBody = body $ intAtomFuncLessThan False},
  AtomFunction { atomFuncName = "lte",
                 atomFuncType = [IntAtomType, IntAtomType, BoolAtomType],
                 atomFuncBody = body $ intAtomFuncLessThan True},
  AtomFunction { atomFuncName = "gte",
                 atomFuncType = [IntAtomType, IntAtomType, BoolAtomType],
                 atomFuncBody = body $ boolAtomNot . (:[]) . intAtomFuncLessThan False},
  AtomFunction { atomFuncName = "gt",
                 atomFuncType = [IntAtomType, IntAtomType, BoolAtomType],
                 atomFuncBody = body $ boolAtomNot . (:[]) . intAtomFuncLessThan True},
  AtomFunction { atomFuncName = "not",
                 atomFuncType = [BoolAtomType, BoolAtomType],
                 atomFuncBody = body boolAtomNot},
  AtomFunction { atomFuncName = "makeByteString",
                 atomFuncType = [TextAtomType, ByteStringAtomType],
                 --I need a proper error-handling scheme for AtomFunctions!
                 atomFuncBody = body $ \((TextAtom textIn):_) -> ByteStringAtom $ (B64.decodeLenient . TE.encodeUtf8) textIn }
  ]
  where
    body = AtomFunctionBody Nothing
                         
intAtomFuncLessThan :: Bool -> [Atom] -> Atom
intAtomFuncLessThan equality ((IntAtom i1):(IntAtom i2):_) = BoolAtom $ i1 `op` i2
  where
    op = if equality then (<=) else (<)
intAtomFuncLessThan _ _= BoolAtom False

boolAtomNot :: [Atom] -> Atom
boolAtomNot ((BoolAtom b):_) = BoolAtom $ not b
boolAtomNot _ = BoolAtom False

--used by sum atom function
relationSum :: Relation -> Atom
relationSum relIn = IntAtom $ relFold (\tupIn acc -> acc + (newVal tupIn)) 0 relIn
  where
    --extract Int from Atom
    newVal :: RelationTuple -> Int
    newVal tupIn = castInt ((tupleAtoms tupIn) V.! 0)
    
relationCount :: Relation -> Atom
relationCount relIn = IntAtom $ relFold (\_ acc -> acc + 1) (0::Int) relIn

relationMax :: Relation -> Atom
relationMax relIn = IntAtom $ relFold (\tupIn acc -> max acc (newVal tupIn)) minBound relIn
  where
    newVal tupIn = castInt ((tupleAtoms tupIn) V.! 0)

relationMin :: Relation -> Atom
relationMin relIn = IntAtom $ relFold (\tupIn acc -> min acc (newVal tupIn)) maxBound relIn
  where
    newVal tupIn = castInt ((tupleAtoms tupIn) V.! 0)

castInt :: Atom -> Int
castInt (IntAtom i) = i
castInt _ = error "attempted to cast non-IntAtom to Int"