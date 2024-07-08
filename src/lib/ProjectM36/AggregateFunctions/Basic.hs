module ProjectM36.AggregateFunctions.Basic where
import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.AtomFunctionError
import qualified Data.HashSet as HS

-- count, sum, max, min
basicAggregateFunctions :: AggregateFunctions
basicAggregateFunctions = HS.fromList
  [
{-    AggregateFunction { aggFuncName = "count",
                        aggFuncFoldFunc = (\_ _ rel ->
                                             case cardinality rel of
                                               Finite i -> pure (IntegerAtom (fromIntegral i))
                                               Countable -> Left InvalidIntBoundError),
                        aggFuncAccumType = IntegerAtomType
                      },-}
    AggregateFunction { aggFuncName = "sum",
                        aggFuncFoldFunc = sumFold,
                        aggFuncFoldType = [IntegerAtomType],
                        aggFuncAccumType = IntegerAtomType
                      }
  ]

sumFold :: AggregateFunctionBodyType
sumFold tup attrName (IntegerAtom acc) [] =
  case atomForAttributeName attrName tup of
    Right (IntegerAtom i) -> pure (IntegerAtom (acc + i))
    Right _ -> Left AtomFunctionTypeMismatchError
    Left _ -> Left (AtomFunctionAttributeNameNotFoundError attrName)
sumFold _ _ _ _ = Left AtomFunctionTypeMismatchError
