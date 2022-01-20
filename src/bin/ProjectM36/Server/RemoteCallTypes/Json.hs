--create a bunch of orphan instances for use with the websocket server
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Server.RemoteCallTypes.Json where
import ProjectM36.AtomFunctionError
import ProjectM36.Base
import ProjectM36.DataFrame
import ProjectM36.DatabaseContextFunctionError
import ProjectM36.DataTypes.Primitive
import ProjectM36.Error
import ProjectM36.IsomorphicSchema
import ProjectM36.Server.RemoteCallTypes
import ProjectM36.MerkleHash
import ProjectM36.Attribute as A

import Data.Aeson
import Data.ByteString.Base64 as B64
import Data.Text.Encoding
import Data.Time.Calendar
import Data.UUID
import Data.Scientific

instance ToJSON RelationalExpr
instance FromJSON RelationalExpr

instance ToJSON TupleExpr
instance FromJSON TupleExpr

instance ToJSON TupleExprs
instance FromJSON TupleExprs

instance ToJSON RestrictionPredicateExpr
instance FromJSON RestrictionPredicateExpr

instance ToJSON AtomExpr
instance FromJSON AtomExpr

instance ToJSON RelationTupleSet
instance FromJSON RelationTupleSet

instance ToJSON RelationTuple
instance FromJSON RelationTuple

instance ToJSON ExecuteRelationalExpr
instance FromJSON ExecuteRelationalExpr

instance ToJSON Relation
instance FromJSON Relation

instance ToJSON Attribute where
  toJSON (Attribute attrName aType) = object [ "name" .= attrName
                                             , "type" .= toJSON aType ]
instance FromJSON Attribute where
  parseJSON = withObject "Attribute" $ \v -> Attribute <$> v .: "name" <*> v .: "type"

instance ToJSON ExtendTupleExpr
instance FromJSON ExtendTupleExpr

instance ToJSON AttributeNames
instance FromJSON AttributeNames

instance ToJSON AttributeExpr
instance FromJSON AttributeExpr

instance ToJSON AtomType
instance FromJSON AtomType

instance ToJSON TypeConstructor
instance FromJSON TypeConstructor

instance ToJSON SchemaExpr
instance FromJSON SchemaExpr

instance ToJSON SchemaIsomorph
instance FromJSON SchemaIsomorph

instance ToJSON DataFrame
instance FromJSON DataFrame

instance ToJSON DataFrameTuple
instance FromJSON DataFrameTuple

instance ToJSON AttributeOrder
instance FromJSON AttributeOrder

instance ToJSON Order
instance FromJSON Order

instance ToJSON Atom where
  toJSON atom@(IntegerAtom i) = object [ "type" .= atomTypeForAtom atom,
                                     "val" .= i ]  
  toJSON atom@(IntAtom i) = object [ "type" .= atomTypeForAtom atom,
                                     "val" .= i ]
  toJSON atom@(DoubleAtom i) = object [ "type" .= atomTypeForAtom atom,
                                        "val" .= i ]
  toJSON atom@(ScientificAtom i) = object [ "type" .= atomTypeForAtom atom,
                                        "val" .= (coefficient i, base10Exponent i) ]
  toJSON atom@(TextAtom i) = object [ "type" .= atomTypeForAtom atom,
                                      "val" .= i ]
  toJSON atom@(DayAtom i) = object [ "type" .= atomTypeForAtom atom,
                                     "val" .= toGregorian i ]
  toJSON atom@(DateTimeAtom i) = object [ "type" .= atomTypeForAtom atom,
                                          "val" .= i ]
  toJSON atom@(ByteStringAtom i) = object [ "type" .= atomTypeForAtom atom,
                                            "val" .= decodeUtf8 (B64.encode i) ]
  toJSON atom@(BoolAtom i) = object [ "type" .= atomTypeForAtom atom,
                                      "val" .= i ]
  toJSON atom@(UUIDAtom u) = object [ "type" .= atomTypeForAtom atom,
                                      "val" .= u ]
  toJSON atom@(RelationAtom i) = object [ "type" .= atomTypeForAtom atom,
                                          "val" .= i ]
  toJSON atom@(RelationalExprAtom i) = object [ "type" .= atomTypeForAtom atom,
                                            "val" .= i ]
  toJSON (ConstructedAtom dConsName atomtype atomlist) = object [
    "dataconstructorname" .= dConsName,
    "type" .= toJSON atomtype,
    "atomlist" .= toJSON atomlist
    ]

instance FromJSON Atom where
  parseJSON = withObject "atom" $ \o -> do
    atype <- o .: "type"
    case atype of
      TypeVariableType _ -> fail "cannot pass TypeVariableType over the wire"
      caType@(ConstructedAtomType _ _) -> ConstructedAtom <$> o .: "dataconstructorname" <*> pure caType <*> o .: "atom"
      RelationAtomType _ -> RelationAtom <$> o .: "val"
      IntAtomType -> IntAtom <$> o .: "val"
      IntegerAtomType -> IntegerAtom <$> o .: "val"
      ScientificAtomType -> do
        (c,e) <- o .: "val"
        pure (ScientificAtom (scientific c e))
      DoubleAtomType -> DoubleAtom <$> o .: "val"
      TextAtomType -> TextAtom <$> o .: "val"
      DayAtomType -> do
        (y, m, d) <- o .: "val"
        pure (DayAtom (fromGregorian y m d))
      DateTimeAtomType -> DateTimeAtom <$> o .: "val"
      ByteStringAtomType -> do
        b64bs <- fmap encodeUtf8 (o .: "val")
        case B64.decode b64bs of
          Left err -> fail ("Failed to parse base64-encoded ByteString: " ++ err)
          Right bs -> pure (ByteStringAtom bs)
      BoolAtomType -> BoolAtom <$> o .: "val"
      UUIDAtomType -> do
        mUUID <- fromString <$> o .: "val"
        case mUUID of
          Just u -> pure $ UUIDAtom u
          Nothing -> fail "Invalid UUID String"
      RelationalExprAtomType -> RelationalExprAtom <$> o .: "val"

instance ToJSON Notification
instance FromJSON Notification

instance ToJSON ScriptCompilationError
instance FromJSON ScriptCompilationError

instance ToJSON MerkleHash where
  toJSON h = object [ "merklehash" .= decodeUtf8 (B64.encode (_unMerkleHash h))]
instance FromJSON MerkleHash where
  parseJSON = withObject "merklehash" $ \o -> do
    b64bs <- encodeUtf8 <$> o .: "merklehash"
    case B64.decode b64bs of
      Left err -> fail ("Failed to parse merkle hash: " ++ err)
      Right bs -> pure (MerkleHash bs)

instance ToJSON Attributes where
  toJSON attrs = object ["attributes" .= map toJSON (A.toList attrs)]
instance FromJSON Attributes where
  parseJSON = withObject "Attributes" $ \o ->
    A.attributesFromList <$> o .: "attributes"

instance ToJSON RelationalError
instance FromJSON RelationalError

instance ToJSON SchemaError
instance FromJSON SchemaError

instance ToJSON MergeError
instance FromJSON MergeError

instance ToJSON ImportError'
instance FromJSON ImportError'

instance ToJSON DatabaseContextFunctionError
instance FromJSON DatabaseContextFunctionError

instance ToJSON MergeStrategy
instance FromJSON MergeStrategy

instance ToJSON PersistenceError
instance FromJSON PersistenceError

instance ToJSON AtomFunctionError
instance FromJSON AtomFunctionError

instance ToJSON WithNameExpr
instance FromJSON WithNameExpr


