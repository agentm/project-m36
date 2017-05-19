module ProjectM36.TupleFunction where
import ProjectM36.Base
import qualified Data.Map as M
import ProjectM36.Error

tupleFunctionForName :: TupleFunctionName -> TupleFunctions -> Either RelationalError TupleFunction
tupleFunctionForName fName funcs = case M.lookup fName funcs of
  Nothing -> Left (NoSuchFunctionError fName)
  Just f -> Right f