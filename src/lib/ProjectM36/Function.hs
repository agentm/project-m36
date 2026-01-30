-- | Module for functionality common between the various Function types (AtomFunction, DatabaseContextFunction).
{-# LANGUAGE TypeApplications #-}
module ProjectM36.Function where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AtomFunctionError (AtomFunctionError(AtomFunctionMissingReturnTypeError))
import ProjectM36.ScriptSession
import qualified Data.HashSet as HS
import Data.List (intercalate)
import qualified Data.Text as T

-- for merkle hash                       

-- | Return the underlying function to run the Function.
function :: FunctionBody a -> a
function (FunctionScriptBody _ f) = f
function (FunctionBuiltInBody f) = f
function (FunctionObjectLoadedBody _ _ _ f) = f

-- | Return the text-based Haskell script, if applicable.
functionScript :: Function a acl -> Maybe FunctionBodyScript
functionScript func = case funcBody func of
  FunctionScriptBody script _ -> Just script
  _ -> Nothing

-- | Change atom function definition to reference proper object file source. Useful when moving the object file into the database directory.
processObjectLoadedFunctionBody :: ObjectModuleName -> ObjectFileEntryFunctionName -> FilePath -> FunctionBody a -> FunctionBody a
processObjectLoadedFunctionBody modName fentry objPath body =
  FunctionObjectLoadedBody objPath modName fentry f
  where
    f = function body

processObjectLoadedFunctions :: Functor f => ObjectModuleName -> ObjectFileEntryFunctionName -> FilePath -> f (Function a acl) -> f (Function a acl)
processObjectLoadedFunctions modName entryName path =
  fmap (\f -> f { funcBody = processObjectLoadedFunctionBody modName entryName path (funcBody f) } )

loadFunctions :: ModName -> FuncName -> Maybe FilePath -> FilePath -> IO (Either LoadSymbolError [Function a acl])
#ifdef PM36_HASKELL_SCRIPTING
loadFunctions modName funcName' mModDir objPath =
  case mModDir of
    Just modDir -> do
      eNewFs <- loadFunctionFromDirectory LoadAutoObjectFile modName funcName' modDir objPath
      case eNewFs of
        Left err -> pure (Left err)
        Right newFs ->
          pure (Right (processFuncs newFs))
    Nothing -> do
      loadFunction LoadAutoObjectFile modName funcName' objPath
 where
   --functions inside object files probably won't have the right function body metadata
   processFuncs = map processor
   processor newF = newF { funcBody = processObjectLoadedFunctionBody modName funcName' objPath (funcBody newF)}
#else
loadFunctions _ _ _ _ = pure (Left LoadSymbolError)
#endif

functionForName :: FunctionName -> HS.HashSet (Function a acl) -> Either RelationalError (Function a acl)
functionForName funcName' funcSet =
  case HS.toList $ HS.filter (\f -> funcName f == funcName') funcSet of
    [] -> Left $ NoSuchFunctionError funcName'
    x : _ -> Right x

{-
  \[IntegerAtom val1, IntegerAtom val2] -> 
     Right $ IntegerAtom $ apply_discount val1 val2
-}
wrapAtomFunction :: [TypeConstructor] -> FunctionName -> Either RelationalError String
wrapAtomFunction tConss@(_:_) funcName' = pure $
  -- we have to make a string-based, dynamic wrapper since we need to get a consistent function type out of the code
  -- there's no value in having an AtomFunction with no return type, so there must be a list with at least one value
        "\\[" <>
        intercalate "," (map (\(i,c) -> convType c <> " val" <> show @Int i) (zip [1 ..] (init tConss))) <>
        "] -> Right $ " <> 
        convType (last tConss) <>
        " $ " <>
        T.unpack funcName' <>
        " " <>
        intercalate " " (map (\i -> "val" <> show i) [1 .. length tConss - 1])
  where
      convType tCons =
        case tCons of
          PrimitiveTypeConstructor tyName _ ->
            T.unpack tyName <> "Atom"
wrapAtomFunction [] _ = Left (AtomFunctionUserError AtomFunctionMissingReturnTypeError)
