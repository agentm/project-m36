-- | Module for functionality common between the various Function types (AtomFunction, DatabaseContextFunction).
module ProjectM36.Function where
import ProjectM36.Base
import ProjectM36.Serialise.Base ()
import qualified Data.ByteString.Lazy as BL
import Codec.Winery

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
