-- | A unified class for walking the database structure to produce a hash used for Merkle trees and validation.
{-# LANGUAGE DefaultSignatures, FlexibleInstances, TypeApplications, GeneralizedNewtypeDeriving, RankNTypes, ExistentialQuantification, BangPatterns #-}
module ProjectM36.HashSecurely where
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Scientific as BSB
import ProjectM36.Base
import ProjectM36.Tuple (tupleAttributes, tupleAtoms)
import ProjectM36.Serialise.Base ()
import ProjectM36.IsomorphicSchema
import ProjectM36.Transaction
import qualified Data.HashSet as HS
import qualified ProjectM36.DataConstructorDef as DC
import ProjectM36.MerkleHash
import Data.List (sortOn)
import qualified Data.Map as M
import qualified ProjectM36.TypeConstructorDef as TCons
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock
import Codec.Winery (Serialise)
import Data.Int (Int64)

import Debug.Trace

newtype SecureHash = SecureHash { _unSecureHash :: B.ByteString }
  deriving (Serialise, Show)

--data SHash = forall a. HashSecurely a => SHash a

-- an accumulating buffer which calls Ctx.update when the buffer hits a threshold
{-data HashBytesBuffer = HashBytesBuffer { _ctx :: SHA256.Ctx,
                                         _buffer :: B.ByteString
                                       }

newBuffer :: HashBytesBuffer
newBuffer = HashBytesBuffer { _ctx = SHA256.init,
                              _buffer = mempty }
-}

bufferThreshold :: Int64
bufferThreshold = 100 * 1048576 -- 100 MB limit to hash

{-
hashl :: [SecureHash] -> SecureHash
hashl l = SecureHash $ foldr (\i acc -> SHA256.hash (_unSecureHash i <> acc)) mempty l
  
hashb :: B.ByteString -> SecureHash
hashb = SecureHash . SHA256.hash

hashbl :: BL.ByteString -> SecureHash
hashbl = SecureHash . SHA256.hashlazy

hashSecurelyL :: (HashSecurely a, Foldable f) => T.Text -> f a -> SecureHash
hashSecurelyL nam l =
  SecureHash $ foldr (\i !acc -> SHA256.hash (_unSecureHash (hashSecurely i) <> acc)) (TE.encodeUtf8 nam) l
-}
{-
hashSecurelyTuples :: [RelationTuple] -> SecureHash
hashSecurelyTuples [] = hashb "RelationTupleSetempty"
hashSecurelyTuples tups = hashSecurelyL "RelationTuple" tups
-}
-- | Execute a secure hashing algorithm against the transaction graph for Merkle hash validation purposes.
{-
class HashSecurely a where
  hashSecurely :: a -> SecureHash

instance HashSecurely SHash where
  hashSecurely (SHash v) = hashSecurely v

instance HashSecurely (HS.HashSet (Function a)) where
  hashSecurely afs = hashSecurelyL "HashSetFunction" (sortOn funcName (HS.toList afs))

-- problem- built-in function bodies cannot be hashed- is it an issue?
instance HashSecurely (Function a) where
  hashSecurely func =
    hashSecurelyL "Function" (SHash (funcName func):
                              SHash (funcBody func):
                              map SHash (funcType func))

instance HashSecurely AtomType where
  hashSecurely typ =
    case typ of
      IntAtomType -> hashb "IntAtomType"
      IntegerAtomType -> hashb "IntegerAtomType"
      ScientificAtomType -> hashb "ScientificAtomType"
      DoubleAtomType -> hashb "DoubleAtomType"
      TextAtomType -> hashb "TextAtomType"
      DayAtomType -> hashb "DayAtomType"
      DateTimeAtomType -> hashb "DateTimeAtomType"
      ByteStringAtomType -> hashb "ByteStringAtomType"
      BoolAtomType -> hashb "BoolAtomType"
      UUIDAtomType -> hashb "UUIDAtomType"
      RelationAtomType attrs -> hashSecurelyL "RelationAtomType" (attributesVec attrs)
      ConstructedAtomType tConsName tvarMap -> hashSecurelyL "ConstructedAtomType" (SHash tConsName : map SHash (M.toAscList tvarMap))
      RelationalExprAtomType -> hashb "RelationalExprAtomType"
      TypeVariableType tvn -> hashSecurelyL "TypeVariableType" [tvn]

instance (HashSecurely a, HashSecurely b) => HashSecurely (a,b) where
  hashSecurely (a,b) = hashSecurely [SHash a, SHash b]

instance HashSecurely [SHash] where
  hashSecurely l = hashSecurelyL "" l

instance HashSecurely Notification where
  hashSecurely notif = hashSecurelyL "Notification" [changeExpr notif, reportOldExpr notif, reportNewExpr notif]

instance HashSecurely Attribute where
  hashSecurely (Attribute aname atyp) = hashSecurelyL "Attribute" [SHash aname, SHash atyp]
  
instance HashSecurely (FunctionBody a) where
  hashSecurely (FunctionScriptBody s _) = hashSecurelyL "FunctionScriptBody" [s]
  hashSecurely (FunctionBuiltInBody _) = hashb "FunctionBuiltInBody"
  hashSecurely (FunctionObjectLoadedBody a b c _) = hashSecurelyL "FunctionObjectLoadedBody" (map T.pack [a,b,c])

--for building the Merkle hash, but not for ddl hashing (notification are irrelevant to the DDL and relation variables are mapped to their types instead of relexprs)
instance HashSecurely DatabaseContext where
  hashSecurely ctx = hashSecurelyL "DatabaseContext" [SHash incDeps,
                                                      SHash rvs,
                                                      SHash nots,
                                                      SHash tConsMap,
                                                      SHash atomFs,
                                                      SHash dbcFs]
   where
    incDeps = inclusionDependencies ctx
    rvs = relationVariables ctx
    atomFs = atomFunctions ctx
    dbcFs = dbcFunctions ctx
    nots = notifications ctx
    tConsMap = typeConstructorMapping ctx

instance HashSecurely Notifications where
  hashSecurely notificationsMap = hashSecurelyL "Notifications" (M.toAscList notificationsMap)

--instance HashSecurely [a] where
--  hashSecurely vals = SecureHash $ foldr (\v acc -> SHA256.hash (_unSecureHash (hashSecurely v))) mempty vals
    
instance HashSecurely InclusionDependencies where
  hashSecurely incDeps =
    hashSecurelyL "InclusionDependencies" incDeps

-- not used for DDL hashing- we want the resultant type there, but this is fine for the Merkle hash
-- override to use more laziness in serialization
instance HashSecurely RelationVariables where
  hashSecurely relvars = hashSecurelyL "RelationVariables" (M.toAscList relvars)

instance HashSecurely () where
  hashSecurely () = hashb "()"

instance HashSecurely InclusionDependency where
  hashSecurely (InclusionDependency exprA exprB) = hashSecurelyL "InclusionDependency" [exprA, exprB]

-- hash tuples to ensure that we don't accumulate lots of serialized tuple data
instance (HashBytes a, HashSecurely a) => HashSecurely (RelationalExprBase a) where
  hashSecurely (MakeRelationFromExprs mAttrs tupleExprs) =
    hashSecurelyL "MakeRelationFromExprs" (case mAttrs of
                                             Nothing -> [SHash @T.Text "Nothing", SHash tupleExprs]
                                             Just attrs -> (SHash @T.Text "Just": SHash tupleExprs: map SHash attrs))
  hashSecurely (MakeStaticRelation attrs tupSet) =
    hashSecurelyL "MakeStaticRelation" [SHash attrs, SHash tupSet]
  hashSecurely (ExistingRelation (Relation attrs tupSet)) =
    hashSecurelyL "ExistingRelation" [SHash tupSet, SHash attrs]
  hashSecurely (RelationVariable rvName marker) =
    hashSecurelyL "RelationVariable" [SHash rvName, SHash marker]
  hashSecurely (Project attrNames expr) =
    hashSecurelyL "Project" [SHash attrNames, SHash expr]
  hashSecurely (Union exprA exprB) =
    hashSecurelyL "Union" [exprA, exprB]
  hashSecurely (Join exprA exprB) =
    hashSecurelyL "Join" [exprA, exprB]
  hashSecurely (Rename nameA nameB expr) =
    hashSecurelyL "Rename" [SHash nameA, SHash nameB, SHash expr]
  hashSecurely (Difference exprA exprB) =
    hashSecurelyL "Difference" [exprA, exprB]
  hashSecurely (Group names name expr) =
    hashSecurelyL "Group" [SHash names, SHash name, SHash expr]
  hashSecurely (Ungroup name expr) =
    hashSecurelyL "Ungroup" [SHash name, SHash expr]
  hashSecurely (Restrict pred' expr) =
    hashSecurelyL "Restrict" [SHash pred', SHash expr]
  hashSecurely (Equals exprA exprB) =
    hashSecurelyL "Equals" [exprA, exprB]
  hashSecurely (NotEquals exprA exprB) =
    hashSecurelyL "NotEquals" [exprA, exprB]
  hashSecurely (Extend ext expr) =
    hashSecurelyL "Extend" [SHash ext, SHash expr]
  hashSecurely (With withExprs expr) =
    hashSecurelyL "With" (SHash expr: map SHash (sortOn (\(WithNameExpr rv _, _) -> rv) withExprs))
-}
{-
instance (HashBytes a, HashSecurely a) => HashSecurely (TupleExprsBase a) where
  hashSecurely (TupleExprs marker exprs) =
    --hashSecurelyL "TupleExprs" (SHash marker : map SHash exprs)
    hashbl $ "TupleExprs" <> foldr (\tupExpr acc ->
              (if BL.length acc > bufferThreshold then
                  BL.fromStrict (_unSecureHash (hashbl acc))
                else
                  acc) <>
              hashBytes tupExpr
             ) mempty exprs
-}
{-
instance HashSecurely a => HashSecurely (TupleExprBase a) where
  hashSecurely (TupleExpr exprMap) =
    hashSecurelyL "TupleExpr" (M.toAscList exprMap)
-}
{-
instance HashSecurely StringType where
  hashSecurely rv = hashb (TE.encodeUtf8 rv)

instance HashSecurely GraphRefTransactionMarker where
  hashSecurely (TransactionMarker marker) = hashb $ "TransactionMarker" <> (BL.toStrict (UUID.toByteString marker))
  hashSecurely UncommittedContextMarker = hashb "UncommittedContextMarker"
-}
-- optimized version for common case of lots of tuple exprs, bundle them up, then hash last
-- we really should use streamly for hashing blocks of bytestrings
{-
instance HashBytes a => HashSecurely (TupleExprBase a) where
  hashSecurely (TupleExpr exprMap) = --hashSecurelyL "TupleExpr" (M.toAscList exprMap)
    hashb $ BL.toStrict $ "TupleExpr" <> foldr (\(attrName, atomExpr) acc ->
                                    (if BL.length acc > bufferThreshold then
                                       BL.fromStrict (SHA256.hashlazy acc)
                                      else
                                      acc) <>
                                    "Attribute" <> BL.fromStrict (TE.encodeUtf8 attrName) <>
                                    hashBytes atomExpr
                                    ) mempty m
    where
      m = M.toAscList exprMap
-}
{-
instance HashSecurely a => HashSecurely (AtomExprBase a) where
  hashSecurely (AttributeAtomExpr aname) = hashSecurelyL "AttributeAtomExpr" [aname]
  hashSecurely (NakedAtomExpr atom) = hashSecurelyL "NakedAtomExpr" [atom]
  hashSecurely (FunctionAtomExpr fname args marker) = hashSecurelyL "FunctionAtomExpr" (SHash fname: SHash marker: map SHash args)
  hashSecurely (RelationAtomExpr relExpr) = hashSecurelyL "RelationAtomExpr" [relExpr]
  hashSecurely (ConstructedAtomExpr dConsName args marker) =
    hashSecurelyL "ConstructedAtomExpr" (SHash dConsName: SHash marker: map SHash args)
-}
{-
instance HashSecurely RelationTupleSet where
  hashSecurely tupSet = 
   case asList tupSet of
     [] -> hashb "RelationTupleSetempty"
     tups -> hashSecurelyTuples tups
-}
{-
instance HashSecurely RelationTuple where
  hashSecurely tup =
    hashSecurelyL "RelationTuple" [SHash (tupleAttributes tup),
                                   SHash (tupleAtoms tup)]
-}
{-
instance HashSecurely (V.Vector Atom) where --optimized instance so that we don't have to convert to a list
  hashSecurely atoms = SecureHash $ 
    V.foldr (\atm acc -> (SHA256.hash (acc <> _unSecureHash (hashSecurely atm)))
            ) mempty atoms
-}
{-
instance HashSecurely Attributes where
  hashSecurely attrs = hashSecurelyL "Attributes" (attributesVec attrs)

instance HashSecurely (BL.ByteString) where
  hashSecurely bs = SecureHash (SHA256.hash (BL.toStrict bs))
-}
{-
instance HashSecurely Atom where
  hashSecurely atm =
    case atm of
      IntegerAtom i -> hashSecurelyL "IntegerAtom" [BSB.toLazyByteString (BSB.integerDec i)]
      IntAtom i -> hashSecurelyL "IntAtom" [BSB.toLazyByteString (BSB.intDec i)]
      ScientificAtom s -> hashSecurelyL "ScientificAtom" [BSB.toLazyByteString (BSB.scientificBuilder s)]
      DoubleAtom d -> hashSecurelyL "DoubleAtom" [BSB.toLazyByteString (BSB.doubleDec d)]
      TextAtom t -> hashSecurelyL "TextAtom" [t]
      DayAtom d -> hashSecurelyL "DayAtom" [BSB.toLazyByteString (BSB.integerDec (toModifiedJulianDay d))]
      DateTimeAtom dt -> hashSecurelyL "DateTimeAtom" [dt]
      ByteStringAtom bs -> hashSecurelyL "ByteStringAtom" [BL.fromStrict bs]
      BoolAtom b -> hashSecurelyL "BoolAtom" [if b then "1"::T.Text else "0"]
      UUIDAtom u -> hashSecurelyL "UUIDAtom" [UUID.toByteString u]
      RelationAtom r -> hashSecurelyL "RelationAtom" [r]
      RelationalExprAtom e -> hashSecurelyL "RelationalExprAtom" [e]
      ConstructedAtom d typ args -> hashSecurelyL "ConstructedAtom" (SHash d: SHash typ: map SHash args)
-}
{-
instance HashSecurely a => HashSecurely (AttributeExprBase a) where
  hashSecurely (AttributeAndTypeNameExpr aname tcons marker) = hashSecurelyL "AttributeAndTypeNameExpr" [SHash aname, SHash tcons, SHash marker]
  hashSecurely (NakedAttributeExpr attr) = hashSecurelyL "NakedAttributeExpr" [attr]
  
instance HashSecurely TypeConstructorMapping where
  --type constructor names must be unique whether primitive or ADTs, so no names should result in an arbitrary order (if two names were the same)
  hashSecurely tConsMap = hashSecurelyL "TypeConstructorMapping" (sortOn (TCons.name . fst) tConsMap)

instance HashSecurely Relation where
  hashSecurely (Relation attrs tupSet) =
    hashSecurelyL "Relation" [SHash tupSet, SHash attrs]

instance HashSecurely TypeConstructor where
  hashSecurely tcons =
    case tcons of
      ADTypeConstructor tName args -> hashSecurelyL "ADTypeConstructor" (SHash tName : map SHash args)
      PrimitiveTypeConstructor tConsName typ -> hashSecurelyL "PrimitiveTypeConstructor" [SHash tConsName, SHash typ]
      RelationAtomTypeConstructor attrExprs -> hashSecurelyL "RelationAtomTypeConstructor" attrExprs
      TypeVariable tv -> hashSecurelyL "TypeVariable" [tv]

instance HashSecurely DataConstructorDef where
  hashSecurely (DataConstructorDef dConsName args) =
    hashSecurelyL "DataConstructorDef" (SHash dConsName : map SHash args)

instance HashSecurely DataConstructorDefArg where
  hashSecurely (DataConstructorDefTypeConstructorArg tCons) =
    hashSecurelyL "DataConstructorDefTypeConstructorArg" [tCons]
  hashSecurely (DataConstructorDefTypeVarNameArg tv) =
    hashSecurelyL "DataConstructorDefTypeVarNameArg" [tv]

instance HashSecurely [DataConstructorDef] where
  hashSecurely defs =
    hashSecurelyL "DataConstructoDefList" (sortOn DC.name defs)

instance HashSecurely TypeConstructorDef where
  hashSecurely (ADTypeConstructorDef tCons args) =
    hashSecurelyL "ADTypeConstructorDef" (SHash tCons, map SHash args)
  hashSecurely (PrimitiveTypeConstructorDef tCons typ) =
    hashSecurelyL "PrimitiveTypeConstructorDef" [SHash tCons, SHash typ]

instance HashSecurely TransactionId where
  hashSecurely tid =
    hashSecurelyL "TransactionId" [UUID.toByteString tid]

instance HashSecurely Schema where
  hashSecurely s =
    hashSecurelyL "Schema" [s]

-- schema isomorphs have no specific ordering
instance HashSecurely SchemaIsomorphs where
  hashSecurely isos =
    hashSecurelyL "SchemaIsomorphs" (sortOn sortIso isos)
    where
      sortIso iso = mconcat (isomorphInRelVarNames iso)

instance HashSecurely SchemaIsomorph where
  hashSecurely (IsoRestrict r p (a,b)) =
    hashSecurelyL "IsoRestrict" [SHash r, SHash p, SHash a, SHash b]
  hashSecurely (IsoRename a b) =
    hashSecurelyL "IsoRename" [a,b]
  hashSecurely (IsoUnion (a,b) p r) =
    hashSecurelyL "IsoUnion" [SHash a, SHash b, SHash p, SHash r]

instance HashSecurely a => HashSecurely (RestrictionPredicateExprBase a) where
  hashSecurely TruePredicate = hashSecurelyL "TruePredicate" ([] :: [RestrictionPredicateExpr])
  hashSecurely (AndPredicate a b) = hashSecurelyL "AndPredicate" [a,b]
  hashSecurely (OrPredicate a b) = hashSecurelyL "OrPredicate" [a,b]
  hashSecurely (NotPredicate a) = hashSecurelyL "NotPredicate" [a]
  hashSecurely (RelationalExprPredicate e) = hashSecurelyL "RelationalExprPredicate" [e]
  hashSecurely (AtomExprPredicate a) = hashSecurelyL "AtomExprPredicate" [a]
  hashSecurely (AttributeEqualityPredicate a e) = hashSecurelyL "AttributeEqualityPredicate" [SHash a, SHash e]

-- for transaction timestamps
instance HashSecurely UTCTime where
  hashSecurely tim =
    hashSecurelyL "UTCTime" [BSB.toLazyByteString (BSB.integerDec (toModifiedJulianDay (utctDay tim))),
                              BSB.toLazyByteString (BSB.integerDec (diffTimeToPicoseconds (utctDayTime tim)))]
-}
{-
-- for parent transaction hashes
instance HashSecurely MerkleHash where
  hashSecurely = hashb . _unMerkleHash

-- used in hashing relation variable types
instance HashSecurely (M.Map RelVarName Relation) where
  hashSecurely m =
    hashSecurelyL "rvtypes" (M.toAscList m)

instance HashSecurely a => HashSecurely (AttributeNamesBase a) where
  hashSecurely (AttributeNames s) = hashSecurelyL "AttributeNames" (S.toAscList s)
  hashSecurely (InvertedAttributeNames s) = hashSecurelyL "InvertedAttributeNames" (S.toAscList s)
  hashSecurely (UnionAttributeNames a b) = hashSecurelyL "UnionAttributeNames" [a, b]
  hashSecurely (IntersectAttributeNames a b) = hashSecurelyL "IntersectAttributeNames" [a, b]
  hashSecurely (RelationalExprAttributeNames r) = hashSecurelyL "RelationalExprAttributeNames" [r]

instance HashSecurely a => HashSecurely (ExtendTupleExprBase a) where
  hashSecurely (AttributeExtendTupleExpr name expr) =
    hashSecurelyL "AttributeExtendTupleExpr" [SHash name, SHash expr]

instance HashSecurely a => HashSecurely (WithNameExprBase a) where
  hashSecurely (WithNameExpr rv marker) = hashSecurelyL "WithNameExpr" [SHash rv, SHash marker]
-}

--hashIt :: HashBytes a => a -> SecureHash
--hashIt val = -- use sha256 ctx updates instead of top-level function

-- foldr (\a (ctx, bs) -> (ctx, bx)) (init, mempty) 
-- optimization: concatenate a tuple's atoms together for hashing at once
class HashBytes a where
  hashBytes :: a -> SHA256.Ctx -> SHA256.Ctx

instance HashBytes Atom where
  hashBytes atm ctx =
    case atm of
      IntegerAtom i -> up ("IntegerAtom" <> build (BSB.integerDec i))
      IntAtom i -> up ("IntAtom" <> build (BSB.intDec i))
      ScientificAtom s -> up ("ScientificAtom" <> build (BSB.scientificBuilder s))
      DoubleAtom d -> up ("DoubleAtom" <> build (BSB.doubleDec d))
      TextAtom t -> up ("TextAtom" <> TE.encodeUtf8 t)
      DayAtom d -> up ("DayAtom" <> build (BSB.integerDec (toModifiedJulianDay d)))
      DateTimeAtom dt -> up ("DateTimeAtom" <>
                             build (BSB.integerDec (toModifiedJulianDay (utctDay dt)) <>
                              BSB.integerDec (diffTimeToPicoseconds (utctDayTime dt))))
      ByteStringAtom bs -> up ("ByteStringAtom" <> bs)
      BoolAtom b -> up ("BoolAtom" <> if b then "1" else "0")
      UUIDAtom u -> up ("UUIDAtom" <> BL.toStrict (UUID.toByteString u))
      RelationAtom r -> hashBytesL ctx "RelationAtom" [SHash r]
      RelationalExprAtom e -> hashBytesL ctx "RelationalExprAtom" [SHash e]
      ConstructedAtom d typ args ->
          hashBytesL ctx "ConstructedAtom" ([SHash d, SHash typ] <> map SHash args)
      where
        build = BL.toStrict . BSB.toLazyByteString
        up = SHA256.update ctx


instance HashBytes T.Text where
  hashBytes t ctx = SHA256.update ctx (TE.encodeUtf8 t)
  
instance HashBytes Relation where
  hashBytes (Relation attrs tupSet) ctx =
    hashBytesL ctx "Relation" [SHash attrs, SHash tupSet]

data SHash = forall a. HashBytes a => SHash !a

hashBytesL :: Foldable f => SHA256.Ctx -> B.ByteString -> f SHash -> SHA256.Ctx
hashBytesL ctx name l = foldr (\(SHash i) ctx' -> hashBytes i ctx') (SHA256.update ctx name) l

instance HashBytes a => HashBytes (RelationalExprBase a) where    
  hashBytes (MakeRelationFromExprs mAttrs tupleExprs) ctx =
    hashBytesL ctx "MakeRelationFromExprs" [SHash mAttrs, SHash tupleExprs]
  hashBytes (MakeStaticRelation attrs tupSet) ctx =
    hashBytesL ctx "MakeStaticRelation" [SHash attrs, SHash tupSet]
  hashBytes (ExistingRelation (Relation attrs tupSet)) ctx =
    hashBytesL ctx "ExistingRelation" [SHash tupSet, SHash attrs]
  hashBytes (RelationVariable rvName marker) ctx =
    hashBytesL ctx "RelationVariable" [SHash rvName, SHash marker]
  hashBytes (Project attrNames expr) ctx =
    hashBytesL ctx "Project" [SHash attrNames, SHash expr]
  hashBytes (Union exprA exprB) ctx =
    hashBytesL ctx "Union" [SHash exprA, SHash exprB]
  hashBytes (Join exprA exprB) ctx =
    hashBytesL ctx "Join" [SHash exprA, SHash exprB]
  hashBytes (Rename nameA nameB expr) ctx =
    hashBytesL ctx "Rename" [SHash nameA, SHash nameB, SHash expr]
  hashBytes (Difference exprA exprB) ctx =
    hashBytesL ctx "Difference" [SHash exprA, SHash exprB]
  hashBytes (Group names name expr) ctx =
    hashBytesL ctx "Group" [SHash names, SHash name, SHash expr]
  hashBytes (Ungroup name expr) ctx =
    hashBytesL ctx "Ungroup" [SHash name, SHash expr]
  hashBytes (Restrict pred' expr) ctx =
    hashBytesL ctx "Restrict" [SHash pred', SHash expr]
  hashBytes (Equals exprA exprB) ctx =
    hashBytesL ctx "Equals" [SHash exprA, SHash exprB]
  hashBytes (NotEquals exprA exprB) ctx =
    hashBytesL ctx "NotEquals" [SHash exprA, SHash exprB]
  hashBytes (Extend ext expr) ctx =
    hashBytesL ctx "Extend" [SHash ext, SHash expr]
  hashBytes (With withExprs expr) ctx =
    hashBytesL ctx "With" (SHash expr: map SHash (sortOn (\(WithNameExpr rv _, _) -> rv) withExprs))

instance HashBytes a => HashBytes (AttributeNamesBase a) where
  hashBytes (AttributeNames s) ctx = hashBytesL ctx "AttributeNames" (map SHash (S.toAscList s))
  hashBytes (InvertedAttributeNames s) ctx = hashBytesL ctx "InvertedAttributeNames" (map SHash (S.toAscList s))
  hashBytes (UnionAttributeNames a b) ctx = hashBytesL ctx "UnionAttributeNames" [SHash a, SHash b]
  hashBytes (IntersectAttributeNames a b) ctx = hashBytesL ctx "IntersectAttributeNames" [SHash a, SHash b]
  hashBytes (RelationalExprAttributeNames r) ctx = hashBytesL ctx "RelationalExprAttributeNames" [SHash r]

instance HashBytes a => HashBytes (ExtendTupleExprBase a) where
  hashBytes (AttributeExtendTupleExpr name expr) ctx =
    hashBytesL ctx "AttributeExtendTupleExpr" [SHash name, SHash expr]

instance HashBytes a => HashBytes (WithNameExprBase a) where
  hashBytes (WithNameExpr rv marker) ctx = hashBytesL ctx "WithNameExpr" [SHash rv, SHash marker]
  
instance HashBytes GraphRefTransactionMarker where
  hashBytes (TransactionMarker tid) ctx = SHA256.update ctx (BL.toStrict ("TransactionMarker" <> UUID.toByteString tid))
  hashBytes UncommittedContextMarker ctx = SHA256.update ctx "UncommittedContextMarker"


instance HashBytes a => HashBytes (TupleExprBase a) where
  hashBytes (TupleExpr exprMap) ctx =
    foldr (\(attrName, atomExpr) ctx' ->
             hashBytesL ctx' "TupleExpr" [SHash attrName, SHash atomExpr])
    ctx (M.toAscList exprMap)

instance HashBytes a => HashBytes (AtomExprBase a) where
  hashBytes atomExpr ctx =
    case atomExpr of
      (AttributeAtomExpr a) -> hashBytesL ctx "AttributeAtomExpr" [SHash a]
      (NakedAtomExpr a) -> hashBytesL ctx "NakedAtomExpr" [SHash a]
      (FunctionAtomExpr fname args marker) ->
        hashBytesL ctx "FunctionAtomExpr" $ [SHash fname, SHash marker] <> map SHash args
      (RelationAtomExpr r) -> hashBytesL ctx "RelationAtomExpr" [SHash r]
      (ConstructedAtomExpr dConsName args marker) ->
        hashBytesL ctx "ConstructedAtomExpr" ([SHash dConsName, SHash marker] <> map SHash args)

instance HashBytes () where
  hashBytes () ctx = SHA256.update ctx "()"

instance HashBytes AtomType where
  hashBytes typ ctx =
    case typ of
      IntAtomType -> hashb "IntAtomType"
      IntegerAtomType -> hashb "IntegerAtomType"
      ScientificAtomType -> hashb "ScientificAtomType"
      DoubleAtomType -> hashb "DoubleAtomType"
      TextAtomType -> hashb "TextAtomType"
      DayAtomType -> hashb "DayAtomType"
      DateTimeAtomType -> hashb "DateTimeAtomType"
      ByteStringAtomType -> hashb "ByteStringAtomType"
      BoolAtomType -> hashb "BoolAtomType"
      UUIDAtomType -> hashb "UUIDAtomType"
      RelationAtomType attrs -> hashBytesL ctx "RelationAtomType" (V.map SHash (attributesVec attrs))
      ConstructedAtomType tConsName tvarMap -> hashBytesL ctx "ConstructedAtomType" (SHash tConsName : map SHash (M.toAscList tvarMap))
      RelationalExprAtomType -> hashb "RelationalExprAtomType"
      TypeVariableType tvn -> hashBytesL ctx "TypeVariableType" [SHash tvn]
    where
      hashb = SHA256.update ctx

instance HashBytes Attributes where
  hashBytes attrs ctx =
    hashBytesL ctx "Attributes" (V.map SHash (attributesVec attrs))

instance HashBytes RelationTupleSet where
  hashBytes tupSet ctx =
    hashBytesL ctx "RelationTupleSet" (map SHash (asList tupSet))

instance HashBytes a => HashBytes (Maybe [AttributeExprBase a]) where
  hashBytes Nothing ctx = SHA256.update ctx "MaybeAttributeExprBaseNothing"
  hashBytes (Just exprs) ctx =
    hashBytesL ctx "MaybeAttributeExprBase" (map SHash exprs)

instance HashBytes a => HashBytes (TupleExprsBase a) where
  hashBytes (TupleExprs marker tupleExprs) ctx =
    hashBytesL ctx "TupleExprs" (SHash marker : map SHash tupleExprs)

instance HashBytes Attribute where
  hashBytes (Attribute name typ) ctx =
    hashBytesL ctx "Attribute" [SHash name, SHash typ]

instance (HashBytes a, HashBytes b) => HashBytes (a, b) where
  hashBytes (a,b) ctx =
    hashBytesL ctx "HTuple" [SHash a, SHash b]

instance HashBytes RelationTuple where
  hashBytes tup ctx =
    hashBytesL ctx "RelationTuple" (V.cons (SHash (tupleAttributes tup)) (V.map SHash (tupleAtoms tup)))

instance HashBytes a => HashBytes (AttributeExprBase a) where
  hashBytes (AttributeAndTypeNameExpr aname tcons marker) ctx =
    hashBytesL ctx "AttributeAndTypeNameExpr" [SHash aname, SHash tcons, SHash marker]
  hashBytes (NakedAttributeExpr attr) ctx =
    hashBytesL ctx "NakedAttributeExpr" [SHash attr]

instance HashBytes TypeConstructor where
  hashBytes tcons ctx =
    case tcons of
      ADTypeConstructor tName args ->
        hashBytesL ctx "ADTypeConstructor" (SHash tName : map SHash args)
      PrimitiveTypeConstructor tConsName typ ->
        hashBytesL ctx "PrimitiveTypeConstructor" [SHash tConsName, SHash typ]
      RelationAtomTypeConstructor attrExprs ->
        hashBytesL ctx "RelationAtomTypeConstructor" (map SHash attrExprs)
      TypeVariable tv ->
        hashBytesL ctx "TypeVariable" [SHash tv]

instance HashBytes TransactionId where
  hashBytes tid ctx = SHA256.update ctx ("TransactionId" <> BL.toStrict (UUID.toByteString tid))

instance HashBytes Schema where
  hashBytes (Schema morphs) ctx =
    hashBytesL ctx "Schema" (map SHash (sortOn sortIso morphs))
    where
      sortIso iso = mconcat (isomorphInRelVarNames iso)
                            

instance HashBytes SchemaIsomorph where
  hashBytes (IsoRestrict r p (a,b)) ctx =
    hashBytesL ctx "IsoRestrict" [SHash r, SHash p, SHash a, SHash b]
  hashBytes (IsoRename a b) ctx =
    hashBytesL ctx "IsoRename" [SHash a, SHash b]
  hashBytes (IsoUnion (a,b) p r) ctx =
    hashBytesL ctx "IsoUnion" [SHash a, SHash b, SHash p, SHash r]

instance HashBytes a => HashBytes (RestrictionPredicateExprBase a) where
  hashBytes TruePredicate ctx = SHA256.update ctx "TruePredicate"
  hashBytes (AndPredicate a b) ctx = hashBytesL ctx "AndPredicate" [SHash a, SHash b]
  hashBytes (OrPredicate a b) ctx = hashBytesL ctx "OrPredicate" [SHash a, SHash b]
  hashBytes (NotPredicate a) ctx = hashBytesL ctx "NotPredicate" [SHash a]
  hashBytes (RelationalExprPredicate e) ctx = hashBytesL ctx "RelationalExprPredicate" [SHash e]
  hashBytes (AtomExprPredicate a) ctx = hashBytesL ctx "AtomExprPredicate" [SHash a]
  hashBytes (AttributeEqualityPredicate a e) ctx = hashBytesL ctx "AttributeEqualityPredicate" [SHash a, SHash e]


instance HashBytes MerkleHash where
  hashBytes h ctx =
    SHA256.update ctx (_unMerkleHash h)

instance HashBytes UTCTime where
  hashBytes tim ctx =
    SHA256.update ctx (BL.toStrict ("UTCTime" <>
                                    BSB.toLazyByteString (BSB.integerDec (toModifiedJulianDay (utctDay tim))) <>
                                    BSB.toLazyByteString (BSB.integerDec (diffTimeToPicoseconds (utctDayTime tim)))))

instance HashBytes DatabaseContext where
  hashBytes db ctx =
    hashBytesL ctx "DatabaseContext" [SHash (inclusionDependencies db),
                                      SHash (relationVariables db),
                                      SHash (notifications db),
                                      SHash (typeConstructorMapping db),
                                      SHash (atomFunctions db),
                                      SHash (dbcFunctions db)]

instance HashBytes InclusionDependencies where
  hashBytes incDeps ctx =
    hashBytesL ctx "InclusionDependencies" (map SHash (M.toAscList incDeps))

instance HashBytes RelationVariables where
  hashBytes rvs ctx =
    hashBytesL ctx "RelationVariables" (map SHash (M.toAscList rvs))

instance HashBytes Notifications where
  hashBytes nots ctx =
    hashBytesL ctx "Notifications" (map SHash (M.toAscList nots))

instance HashBytes TypeConstructorMapping where
  hashBytes tConsMap ctx =
    hashBytesL ctx "TypeConstructorMapping" (map SHash (sortOn (TCons.name . fst) tConsMap))

instance HashBytes AtomFunctions where
  hashBytes afuncs ctx =
    hashBytesL ctx "AtomFunctions" (map SHash (sortOn funcName (HS.toList afuncs)))

instance HashBytes AtomFunction where
  hashBytes func ctx =
    hashBytesL ctx "AtomFunction" (SHash (funcName func):
                                   SHash (funcBody func):
                                   map SHash (funcType func))

instance HashBytes DatabaseContextFunction where
  hashBytes func ctx =
    hashBytesL ctx "DatabaseContextFunction" (SHash (funcName func):
                                              SHash (funcBody func):
                                              map SHash (funcType func))

instance HashBytes DatabaseContextFunctions where
  hashBytes dbcfuncs ctx =
    hashBytesL ctx "DatabaseContextFunctions" (map SHash (sortOn funcName (HS.toList dbcfuncs)))

instance HashBytes InclusionDependency where    
  hashBytes (InclusionDependency exprA exprB) ctx =
    hashBytesL ctx "InclusionDependency" [SHash exprA, SHash exprB]

instance HashBytes Notification where
  hashBytes notif ctx =
    hashBytesL ctx "Notification" [SHash (changeExpr notif),
                                   SHash (reportOldExpr notif),
                                   SHash (reportNewExpr notif)]

instance HashBytes DataConstructorDef where
  hashBytes (DataConstructorDef dConsName args) ctx =
    hashBytesL ctx "DataConstructorDef" (SHash dConsName : map SHash args)

instance HashBytes [DataConstructorDef] where
  hashBytes defs ctx =
    hashBytesL ctx "DataConstructoDefList" (map SHash (sortOn DC.name defs))

instance HashBytes TypeConstructorDef where
  hashBytes (ADTypeConstructorDef tCons args) ctx =
    hashBytesL ctx "ADTypeConstructorDef" (SHash tCons: map SHash args)
  hashBytes (PrimitiveTypeConstructorDef tCons typ) ctx =
    hashBytesL ctx "PrimitiveTypeConstructorDef" [SHash tCons, SHash typ]

instance HashBytes (FunctionBody a) where
  hashBytes (FunctionScriptBody s _) ctx = hashBytesL ctx "FunctionScriptBody" [SHash s]
  hashBytes (FunctionBuiltInBody _) ctx = SHA256.update ctx "FunctionBuiltInBody"
  hashBytes (FunctionObjectLoadedBody a b c _) ctx = hashBytesL ctx "FunctionObjectLoadedBody" (map (SHash . T.pack) [a,b,c])

instance HashBytes DataConstructorDefArg where
  hashBytes (DataConstructorDefTypeConstructorArg tCons) ctx =
    hashBytesL ctx "DataConstructorDefTypeConstructorArg" [SHash tCons]
  hashBytes (DataConstructorDefTypeVarNameArg tv) ctx =
    hashBytesL ctx "DataConstructorDefTypeVarNameArg" [SHash tv]

instance HashBytes (M.Map RelVarName Relation) where
  hashBytes m ctx =
    hashBytesL ctx "rvtypes" (map SHash (M.toAscList m))

-- | Hash a transaction within its graph context to create a Merkle hash for it.
hashTransaction :: Transaction -> S.Set Transaction -> MerkleHash
hashTransaction trans parentTranses = MerkleHash (SHA256.finalize newHash)
  where
    newHash = hashBytesL SHA256.init "Transaction" (map SHash transIds <>
                                         map SHash (M.toAscList (subschemas trans)) <>
                                         map SHash parentMerkleHashes <>
                                         [SHash tstamp,
                                           SHash (concreteDatabaseContext trans)])
    tstamp = stamp (transactionInfo trans)
    parentMerkleHashes = map (getMerkleHash) (S.toAscList parentTranses)
    getMerkleHash t = merkleHash (transactionInfo t)
    transIds = transactionId trans : S.toAscList (parentIds trans)

-- | Return a hash of just DDL-specific (schema) attributes. This is useful for determining if a client has the appropriate updates needed to work with the current schema.
mkDDLHash :: DatabaseContext -> M.Map RelVarName Relation -> SecureHash
mkDDLHash ctx rvtypemap = do
  -- we cannot merely hash the relational representation of the type because the order of items matters when hashing
  -- registered queries are not included here because a client could be compatible with a schema even if the queries are not registered. The client should validate registered query state up-front. Perhaps there should be another hash for registered queries.
  SecureHash $ SHA256.finalize $ hashBytesL SHA256.init "DDLHash" [SHash (inclusionDependencies ctx),
                                                                    SHash (atomFunctions ctx),
                                                                    SHash (dbcFunctions ctx),
                                                                    SHash (typeConstructorMapping ctx),
                                                                    SHash rvtypemap]
