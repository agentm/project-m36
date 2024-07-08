module ProjectM36.AggregateFunctions where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Relation
import ProjectM36.AtomFunctionError
import qualified Data.HashSet as HS

functionForName :: FunctionName -> AggregateFunctions -> Either RelationalError AggregateFunction
functionForName fname aggFuncs =
  if HS.null foundFunc then
    Left $ NoSuchFunctionError fname
    else
    Right $ head $ HS.toList foundFunc
  where
    foundFunc = HS.filter (\f -> aggFuncName f == fname) aggFuncs

evalAggregateFunction :: AggregateFunctionBodyType -> AttributeName -> Atom -> [Atom] -> Relation -> Either AtomFunctionError Atom
evalAggregateFunction foldFunc attrInTuple startVal foldFuncArgs rel =
  relFold tupFolder (Right startVal) rel
  where
    tupFolder :: RelationTuple -> Either AtomFunctionError Atom -> Either AtomFunctionError Atom
    tupFolder tup (Right acc) =
      case foldFunc tup attrInTuple acc foldFuncArgs of
        Left err -> Left err
        Right acc' -> Right acc'
    tupFolder _tup e@Left{} = e
