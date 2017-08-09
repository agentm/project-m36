{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.DataTypes.Interval where
import ProjectM36.AtomFunctionBody
import ProjectM36.Base
import ProjectM36.AtomType
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunctionError
import qualified Data.HashSet as HS

import Debug.Trace

-- in lieu of typeclass support, we just hard-code the types which can be part of an interval
supportsInterval :: AtomType -> Bool
supportsInterval typ = case typ of
  IntAtomType -> True
  DoubleAtomType -> True
  TextAtomType -> False -- just because it supports ordering, doesn't mean it makes sense in an interval
  DayAtomType -> True               
  DateTimeAtomType -> True
  ByteStringAtomType -> False
  BoolAtomType -> False                         
  IntervalAtomType _ -> False
  RelationAtomType _ -> False
  ConstructedAtomType _ _ -> False --once we support an interval-style typeclass, we might enable this
  TypeVariableType _ -> False
  
supportsOrdering :: AtomType -> Bool  
supportsOrdering typ = case typ of
  IntAtomType -> True
  DoubleAtomType -> True
  TextAtomType -> True
  DayAtomType -> True               
  DateTimeAtomType -> True
  ByteStringAtomType -> False
  BoolAtomType -> False                         
  IntervalAtomType _ -> False
  RelationAtomType _ -> False
  ConstructedAtomType _ _ -> False --once we support an interval-style typeclass, we might enable this
  TypeVariableType _ -> False
  
atomCompare :: Atom -> Atom -> Either AtomFunctionError Ordering
atomCompare a1 a2 = let aType = atomTypeForAtom a1 
                        go a b = Right (compare a b)
                        typError = Left (AtomTypeDoesNotSupportOrdering (prettyAtomType aType)) in
                    if atomTypeForAtom a1 /= atomTypeForAtom a2 then
                      Left AtomFunctionTypeMismatchError
                    else if not (supportsOrdering aType) then
                           typError
                         else
                           case (a1, a2) of
                             (IntAtom a, IntAtom b) -> go a b
                             (DoubleAtom a, DoubleAtom b) -> go a b
                             (TextAtom a, TextAtom b) -> go a b
                             (DayAtom a, DayAtom b) -> go a b
                             (DateTimeAtom a, DateTimeAtom b) -> go a b
                             _ -> typError

--check that interval is properly ordered and that the boundaries make sense
createInterval :: Atom -> Atom -> OpenInterval -> OpenInterval -> Either AtomFunctionError Atom
createInterval atom1 atom2 bopen eopen = do
  cmp <- atomCompare atom1 atom2
  case cmp of
    GT -> Left InvalidIntervalOrdering
    EQ -> if bopen || eopen then
            Left InvalidIntervalBoundaries
          else 
            Right valid
    LT -> Right valid
 where valid = IntervalAtom atom1 atom2 bopen eopen

intervalAtomFunctions :: AtomFunctions
intervalAtomFunctions = HS.fromList [
  AtomFunction { atomFuncName = "interval",
                 atomFuncType = [TypeVariableType "a",
                                 TypeVariableType "a",
                                 BoolAtomType,
                                 BoolAtomType,
                                 IntervalAtomType (TypeVariableType "a")],
                 atomFuncBody = compiledAtomFunctionBody $ \(atom1:atom2:BoolAtom bopen:BoolAtom eopen:_) -> do
                   let aType = atomTypeForAtom atom1 
                   if supportsInterval aType then
                     createInterval atom1 atom2 bopen eopen
                     else
                     Left (AtomTypeDoesNotSupportInterval (prettyAtomType aType))
               },
  AtomFunction {
    atomFuncName = "interval_overlaps",
    atomFuncType = [IntervalAtomType (TypeVariableType "a"),
                    IntervalAtomType (TypeVariableType "a"),
                    BoolAtomType],
    atomFuncBody = compiledAtomFunctionBody $ \(i1@IntervalAtom{}:i2@IntervalAtom{}:_) -> do
      res <- intervalOverlaps i1 i2
      pure (BoolAtom res)
    }]


intervalOverlaps :: Atom -> Atom -> Either AtomFunctionError Bool
intervalOverlaps (IntervalAtom i1start i1end i1startopen i1endopen) (IntervalAtom i2start i2end i2startopen i2endopen) = do
      cmp1 <- atomCompare i1start i2end
      cmp2 <- atomCompare i2start i1end
      let startcmp = if i1startopen then oplt else oplte
          endcmp = if i2startopen then oplt else oplte
          oplte op = op == LT || op == EQ
          oplt op = op == LT
      pure (startcmp cmp1 && endcmp cmp2)
intervalOverlaps _ _ = Left AtomFunctionTypeMismatchError      
  
