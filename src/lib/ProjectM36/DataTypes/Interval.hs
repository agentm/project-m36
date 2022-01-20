{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.DataTypes.Interval where
import ProjectM36.AtomFunctionBody
import ProjectM36.Base
import ProjectM36.AtomType
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunctionError
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Control.Monad (when)
import Data.Maybe

type OpenInterval = Bool                     

intervalSubType :: AtomType -> AtomType
intervalSubType typ = if isIntervalAtomType typ then
                        case typ of
                          ConstructedAtomType _ tvMap -> 
                            fromMaybe err (M.lookup "a" tvMap)
                          _ -> err
                        else
                        err
  where
   err = error "intervalSubType on non-interval type"
  
                 
-- in lieu of typeclass support, we just hard-code the types which can be part of an interval
supportsInterval :: AtomType -> Bool
supportsInterval typ = case typ of
  IntAtomType -> True
  IntegerAtomType -> True
  ScientificAtomType -> True
  DoubleAtomType -> True
  TextAtomType -> False -- just because it supports ordering, doesn't mean it makes sense in an interval
  DayAtomType -> True               
  DateTimeAtomType -> True
  ByteStringAtomType -> False
  BoolAtomType -> False
  UUIDAtomType -> False
  RelationAtomType _ -> False
  ConstructedAtomType _ _ -> False --once we support an interval-style typeclass, we might enable this
  RelationalExprAtomType -> False
  TypeVariableType _ -> False
  
supportsOrdering :: AtomType -> Bool  
supportsOrdering typ = case typ of
  IntAtomType -> True
  IntegerAtomType -> True
  ScientificAtomType -> True
  DoubleAtomType -> True
  TextAtomType -> True
  DayAtomType -> True               
  DateTimeAtomType -> True
  ByteStringAtomType -> False
  BoolAtomType -> False
  UUIDAtomType -> False
  RelationAtomType _ -> False
  RelationalExprAtomType -> False
  ConstructedAtomType _ _ -> False --once we support an interval-style typeclass, we might enable this
  TypeVariableType _ -> False
  
atomCompare :: Atom -> Atom -> Either AtomFunctionError Ordering
atomCompare a1 a2 = let aType = atomTypeForAtom a1 
                        go a b = Right (compare a b)
                        typError = Left (AtomTypeDoesNotSupportOrderingError (prettyAtomType aType)) in
                    if atomTypeForAtom a1 /= atomTypeForAtom a2 then
                      Left AtomFunctionTypeMismatchError
                    else if not (supportsOrdering aType) then
                           typError
                         else
                           case (a1, a2) of
                             (IntegerAtom a, IntegerAtom b) -> go a b
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
    GT -> Left InvalidIntervalOrderingError
    EQ -> if bopen || eopen then
            Left InvalidIntervalBoundariesError
          else 
            Right valid
    LT -> Right valid
 where valid = ConstructedAtom "Interval" iType [atom1, atom2, BoolAtom bopen, BoolAtom eopen]
       iType = intervalAtomType (atomTypeForAtom atom1)
       
intervalAtomType :: AtomType -> AtomType       
intervalAtomType typ = ConstructedAtomType "Interval" (M.singleton "a" typ)

intervalAtomFunctions :: AtomFunctions
intervalAtomFunctions = HS.fromList [
  Function { funcName = "interval",
             funcType = [TypeVariableType "a",
                          TypeVariableType "a",
                          BoolAtomType,
                          BoolAtomType,
                          intervalAtomType (TypeVariableType "a")],
             funcBody = compiledAtomFunctionBody $ \(atom1:atom2:BoolAtom bopen:BoolAtom eopen:_) -> do
                   let aType = atomTypeForAtom atom1 
                   if supportsInterval aType then
                     createInterval atom1 atom2 bopen eopen
                     else
                     Left (AtomTypeDoesNotSupportIntervalError (prettyAtomType aType))
               },
  Function {
    funcName = "interval_overlaps",
    funcType = [intervalAtomType (TypeVariableType "a"),
                    intervalAtomType (TypeVariableType "a"),
                    BoolAtomType],
    funcBody = compiledAtomFunctionBody $ \(i1@ConstructedAtom{}:i2@ConstructedAtom{}:_) -> 
      BoolAtom <$> intervalOverlaps i1 i2
    }]
                        
isIntervalAtomType :: AtomType -> Bool
isIntervalAtomType (ConstructedAtomType nam tvMap) = 
  nam == "Interval" && M.keys tvMap == ["a"] && case M.lookup "a" tvMap of
    Nothing -> False
    Just subType -> supportsInterval subType || subType == TypeVariableType "a"
isIntervalAtomType _ = False
    
intervalOverlaps :: Atom -> Atom -> Either AtomFunctionError Bool
intervalOverlaps (ConstructedAtom dCons1 typ1 [i1start,
                                               i1end,
                                               BoolAtom i1startopen,
                                               BoolAtom i1endopen]) (ConstructedAtom dCons2 typ2 
                                                                     [i2start, 
                                                                      i2end, 
                                                                      BoolAtom i2startopen,
                                                                      BoolAtom i2endopen]) = do
  when (dCons1 /= "Interval" || dCons2 /= "Interval" || not (isIntervalAtomType typ1) || not (isIntervalAtomType typ2)) (Left AtomFunctionTypeMismatchError)
  cmp1 <- atomCompare i1start i2end
  cmp2 <- atomCompare i2start i1end
  let startcmp = if i1startopen || i2endopen then oplt else oplte
      endcmp = if i2startopen || i1endopen then oplt else oplte
      oplte op = op == LT || op == EQ
      oplt op = op == LT
  pure (startcmp cmp1 && endcmp cmp2)
intervalOverlaps _ _ = Left AtomFunctionTypeMismatchError      
  
intervalTypeConstructorMapping :: TypeConstructorMapping
intervalTypeConstructorMapping = [(ADTypeConstructorDef "Interval" ["a"], [])]
                                    

