-- | Module for functionality common between the various Function types (AtomFunction, DatabaseContextFunction).
module ProjectM36.Function where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Serialise.Base ()
import ProjectM36.ScriptSession
import qualified Data.ByteString.Lazy as BL
import Codec.Winery
import qualified Data.HashSet as HS

-- for merkle hash                       
hashBytes :: Function a -> BL.ByteString
hashBytes func = BL.fromChunks [fname, ftype, fbody]
  where
    fname = serialise (funcName func)
    ftype = serialise (funcType func)
    fbody = case funcBody func of
      FunctionScriptBody s _ -> serialise s
      FunctionBuiltInBody _ -> serialise ()
      FunctionObjectLoadedBody a b c _ -> serialise (a,b,c)

-- | Return the underlying function to run the Function.
function :: FunctionBody a -> a
function (FunctionScriptBody _ f) = f
function (FunctionBuiltInBody f) = f
function (FunctionObjectLoadedBody _ _ _ f) = f

-- | Return the text-based Haskell script, if applicable.
functionScript :: Function a -> Maybe FunctionBodyScript
functionScript func = case funcBody func of
  FunctionScriptBody script _ -> Just script
  _ -> Nothing

-- | Change atom function definition to reference proper object file source. Useful when moving the object file into the database directory.
processObjectLoadedFunctionBody :: ObjectModuleName -> ObjectFileEntryFunctionName -> FilePath -> FunctionBody a -> FunctionBody a
processObjectLoadedFunctionBody modName fentry objPath body =
  FunctionObjectLoadedBody objPath modName fentry f
  where
    f = function body

processObjectLoadedFunctions :: Functor f => ObjectModuleName -> ObjectFileEntryFunctionName -> FilePath -> f (Function a) -> f (Function a)
processObjectLoadedFunctions modName entryName path =
  fmap (\f -> f { funcBody = processObjectLoadedFunctionBody modName entryName path (funcBody f) } )

loadFunctions :: ModName -> FuncName -> Maybe FilePath -> FilePath -> IO (Either LoadSymbolError [Function a])
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

functionForName :: FunctionName -> HS.HashSet (Function a) -> Either RelationalError (Function a)
functionForName funcName' funcSet = if HS.null foundFunc then
                                         Left $ NoSuchFunctionError funcName'
                                        else
                                         Right $ head $ HS.toList foundFunc
  where
    foundFunc = HS.filter (\f -> funcName f == funcName') funcSet
