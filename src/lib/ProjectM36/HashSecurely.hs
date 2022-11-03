-- | A unified class for walking the database structure to produce a hash used for Merkle trees and validation.
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, RankNTypes, ExistentialQuantification, BangPatterns #-}
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

newtype SecureHash = SecureHash { _unSecureHash :: B.ByteString }
  deriving (Serialise, Show, Eq)

-- run a SHA256 hasher across the necessary data structures
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
hashBytesL ctx name = foldr (\(SHash i) ctx'@(SHA256.Ctx !bs) -> bs `seq` hashBytes i ctx') (SHA256.update ctx name)

instance HashBytes a => HashBytes (RelationalExprBase a) where
  hashBytes (MakeRelationFromExprs mAttrs tupleExprs) ctx =
    hashBytesL ctx "MakeRelationFromExprs" [SHash mAttrs, SHash tupleExprs]
  hashBytes (MakeStaticRelation attrs tupSet) ctx = -- blowing up here!
    hashBytesL ctx "MakeStaticRelation" [SHash attrs, SHash tupSet]
--  hashBytes _ ctx = ctx
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
                                         SHash (concreteDatabaseContext trans)]
                                                   )
    tstamp = stamp (transactionInfo trans)
    parentMerkleHashes = map getMerkleHash (S.toAscList parentTranses)
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
