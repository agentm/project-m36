module ProjectM36.AtomFunctions.Primitive where
import ProjectM36.Atom
import ProjectM36.Base
import ProjectM36.Relation (relFold)
import ProjectM36.Tuple
import ProjectM36.AtomFunction
import ProjectM36.DataTypes.Primitive
import qualified Data.HashSet as HS
import Data.Typeable (cast)
import qualified Data.Vector as V
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

primitiveAtomFunctions :: AtomFunctions
primitiveAtomFunctions = HS.fromList [
  --match on any relation type
  AtomFunction { atomFuncName = "add",
                 atomFuncType = [intAtomType, intAtomType, intAtomType],
                 atomFunc = (\(i1:i2:_) -> Atom $ ((unsafeCast i1)::Int) + unsafeCast i2)},
  AtomFunction { atomFuncName = "id",
                 atomFuncType = [AnyAtomType, AnyAtomType],
                 atomFunc = (\(x:_) -> x)},
  AtomFunction { atomFuncName = "sum",
                 atomFuncType = foldAtomFuncType intAtomType intAtomType,
                 atomFunc = (\(relAtom:_) -> relationSum $ unsafeCast relAtom)},
  AtomFunction { atomFuncName = "count",
                 atomFuncType = foldAtomFuncType AnyAtomType intAtomType,
                 atomFunc = (\((relIn):_) -> relationCount (castRelation relIn))},
  AtomFunction { atomFuncName = "max",
                 atomFuncType = foldAtomFuncType intAtomType intAtomType,
                 atomFunc = (\((relIn):_) -> relationMax (castRelation relIn))},
  AtomFunction { atomFuncName = "min",
                 atomFuncType = foldAtomFuncType intAtomType intAtomType,
                 atomFunc = (\((relIn):_) -> relationMin (castRelation relIn))},
  AtomFunction { atomFuncName = "lt",
                 atomFuncType = [intAtomType, intAtomType, boolAtomType],
                 atomFunc = intAtomFuncLessThan False},
  AtomFunction { atomFuncName = "lte",
                 atomFuncType = [intAtomType, intAtomType, boolAtomType],
                 atomFunc = intAtomFuncLessThan True},
  AtomFunction { atomFuncName = "gte",
                 atomFuncType = [intAtomType, intAtomType, boolAtomType],
                 atomFunc = boolAtomNot . (:[]) . intAtomFuncLessThan False},
  AtomFunction { atomFuncName = "gt",
                 atomFuncType = [intAtomType, intAtomType, boolAtomType],
                 atomFunc = boolAtomNot . (:[]) . intAtomFuncLessThan True},
  AtomFunction { atomFuncName = "not",
                 atomFuncType = [boolAtomType, boolAtomType],
                 atomFunc = boolAtomNot},
  AtomFunction { atomFuncName = "makeByteString",
                 atomFuncType = [textAtomType, byteStringAtomType],
                 --I need a proper error-handling scheme for AtomFunctions!
                 atomFunc = \(textIn:_) -> Atom $ (B64.decodeLenient . TE.encodeUtf8) ((unsafeCast textIn)::T.Text) }
  ]
                         
unsafeCast :: (Atomable a) => Atom -> a
unsafeCast (ConstructedAtom _ _ _) = error "unsafeCast attempt on ConstructedAtom"
unsafeCast (Atom atom) = case cast atom of
                          Just x -> x
                          Nothing -> error "unsafeCast failed"

intAtomFuncLessThan :: Bool -> [Atom] -> Atom
intAtomFuncLessThan equality (iatom1:iatom2:_) = (\i1 i2 -> Atom $ ((unsafeCast i1)::Int) `op` unsafeCast i2) iatom1 iatom2
  where
    op = if equality then (<=) else (<)
intAtomFuncLessThan _ _= Atom False

boolAtomNot :: [Atom] -> Atom
boolAtomNot (bool:_) = Atom $ not (unsafeCast bool)
boolAtomNot _ = Atom False

--used by sum atom function
relationSum :: Relation -> Atom
relationSum relIn = Atom $ relFold (\tupIn acc -> acc + (newVal tupIn)) 0 relIn
  where
    --extract Int from Atom
    newVal :: RelationTuple -> Int
    newVal tupIn = castInt $ (tupleAtoms tupIn) V.! 0
    
relationCount :: Relation -> Atom
relationCount relIn = Atom $ relFold (\_ acc -> acc + 1) (0::Int) relIn

relationMax :: Relation -> Atom
relationMax relIn = Atom $ relFold (\tupIn acc -> max acc (newVal tupIn)) minBound relIn
  where
    newVal tupIn = castInt $ (tupleAtoms tupIn) V.! 0

relationMin :: Relation -> Atom
relationMin relIn = Atom $ relFold (\tupIn acc -> min acc (newVal tupIn)) maxBound relIn
  where
    newVal tupIn = castInt $ (tupleAtoms tupIn) V.! 0
    
castInt :: Atom -> Int
castInt (ConstructedAtom _ _ _) = error "castInt attempt on ConstructedAtom"
castInt (Atom atom) = case cast atom of
  Just i -> i
  Nothing -> error "int cast failure" -- the type checker must ensure this can never occur
    
