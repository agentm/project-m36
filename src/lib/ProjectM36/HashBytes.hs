-- | A unified class for walking the database structure to produce a hash used for Merkle trees and validation.
{-# LANGUAGE DefaultSignatures, FlexibleInstances #-}
module ProjectM36.HashBytes where
import qualified Data.ByteString.Lazy as BL
import Codec.Winery
import ProjectM36.Base
import ProjectM36.Serialise.Base ()
import qualified Data.HashSet as HS
import Data.List (sortOn)
import qualified Data.Map as M
import qualified ProjectM36.TypeConstructorDef as TCons

class HashBytes a where
  hashBytes :: a -> BL.ByteString

  default hashBytes :: Serialise a => a -> BL.ByteString
  hashBytes x = BL.fromStrict (serialise x)

instance HashBytes (HS.HashSet (Function a)) where
  hashBytes afs = foldr (mappend . hashBytes) mempty (sortOn funcName (HS.toList afs))

-- problem- built-in function bodies cannot be hashed- is it an issue?
instance HashBytes (Function a) where
  hashBytes func = BL.fromChunks [fname, ftype, fbody]
   where
    fname = serialise (funcName func)
    ftype = serialise (funcType func)
    fbody = case funcBody func of
      FunctionScriptBody s _ -> serialise s
      FunctionBuiltInBody _ -> serialise ()
      FunctionObjectLoadedBody a b c _ -> serialise (a,b,c)

--for building the Merkle hash, but not for ddl hashing (notification are irrelevant to the DDL and relation variables are mapped to their types instead of relexprs)
instance HashBytes DatabaseContext where
  hashBytes ctx = BL.concat [incDeps, rvs, nots, tConsMap] <> atomFs <> dbcFs
   where
    incDeps = hashBytes (inclusionDependencies ctx)
    rvs = hashBytes (relationVariables ctx)
    atomFs = hashBytes (atomFunctions ctx)
    dbcFs = hashBytes (dbcFunctions ctx)
    nots = hashBytes (notifications ctx)
    tConsMap = hashBytes (typeConstructorMapping ctx)

instance HashBytes Notifications    
    
instance HashBytes InclusionDependencies where
  hashBytes incDeps = BL.fromStrict $ serialise (sortOn fst (M.toList incDeps))

-- not used for DDL hashing- we want the resultant type there, but this is fine for the Merkle hash
instance HashBytes RelationVariables

instance HashBytes TypeConstructorMapping where
  --type constructor names must be unique whether primitive or ADTs, so no names should result in an arbitrary order (if two names were the same)
  hashBytes tConsMap = BL.fromStrict $ serialise (sortOn (TCons.name . fst) tConsMap)
