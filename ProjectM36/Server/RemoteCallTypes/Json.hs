--create a bunch of orphan instances for use with the websocket server
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Server.RemoteCallTypes.Json where
import ProjectM36.Server.RemoteCallTypes
import ProjectM36.Base
import ProjectM36.ConcreteTypeRep
import ProjectM36.Error
import Control.Exception (IOException)

import Data.Aeson
import Data.UUID.Aeson ()
import Data.Proxy
import Data.Typeable
import Data.Text
import Data.Time.Calendar
import ProjectM36.Atom
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunctions.Primitive
import ProjectM36.DataTypes.Day
import Data.ByteString

instance ToJSON RelationalExpr
instance FromJSON RelationalExpr

instance ToJSON TupleExpr
instance FromJSON TupleExpr

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

instance ToJSON Attribute
instance FromJSON Attribute

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

instance ToJSON TypeConstructorArg
instance FromJSON TypeConstructorArg

instance ToJSON SerialRep
instance FromJSON SerialRep

instance FromJSON ConcreteTypeRep where
  parseJSON = withObject "ctr" $ \o -> do
    serialRep <- o .: "serialrep"
    pure $ fromSerial serialRep
    
instance ToJSON ConcreteTypeRep where
  toJSON ctr = object [
    "serialrep" .= toSerial ctr
    ]
                   
instance ToJSON Atom where                   
  toJSON atom@(Atom _) = let atype = atomTypeForAtom atom
                           in
                           if atype == intAtomType then
                             object [
                              "type" .= atype,
                              "val" .= ((unsafeCast atom) :: Int)]
                           else if atype == textAtomType then
                             object [
                               "type" .= atype,
                               "val" .= ((unsafeCast atom) :: Text)]
                           else if atype == doubleAtomType then
                             object [
                               "type" .= atype,
                               "val" .= ((unsafeCast atom) :: Double)]
                           else if atype == dayAtomType then
                             object [
                               "type" .= atype,
                               "val" .= ((unsafeCast atom) :: Day)]
                           else if atype == byteStringAtomType then
                             object [
                               "type" .= atype,
                               "val" .= toText ((unsafeCast atom) :: ByteString)]
                           else if atype == boolAtomType then
                             object [
                               "type" .= atype,
                               "val" .= ((unsafeCast atom) :: Bool)]
                           else if isRelationAtomType atype then
                              object [
                                "type" .= atype,
                                "val" .= castRelation atom ]
                           else error "invalid primitive in ToJSON implementation"

  toJSON (ConstructedAtom dConsName atomtype atomlist) = object [
    "dataconstructorname" .= dConsName,
    "type" .= toJSON atomtype,
    "atomlist" .= toJSON atomlist
    ]
    
instance FromJSON Atom where
  parseJSON = withObject "atom" $ \o -> do
    atype <- o .: "type" 
    case atype of
      AnyAtomType -> fail "cannot pass AnyAtomType over the wire"
      caType@(ConstructedAtomType _ _) -> ConstructedAtom <$> o .: "dataconstructorname" <*> pure caType <*> o .: "atom"
      RelationAtomType _ -> error "not implemented"
      AtomType cTypeRep -> if unCTR cTypeRep == typeRep (Proxy :: Proxy Int) then
                             intAtom <$> o .: "val"
                             else if unCTR cTypeRep == typeRep (Proxy :: Proxy Text) then
                                    textAtom <$> o .: "val"
                                  else if unCTR cTypeRep == typeRep (Proxy :: Proxy Double) then
                                         doubleAtom <$> o .: "val"
                                       else if unCTR cTypeRep == typeRep (Proxy :: Proxy Day) then
                                              dayAtom <$> o .: "val"
                                                 else if unCTR cTypeRep == typeRep (Proxy :: Proxy ByteString) then do
                                                       txt <- (o .: "val")
                                                       case (fromText txt) :: Either String ByteString of
                                                         Left err -> fail $ "bytestring parse failure: " ++ show err
                                                         Right bs -> pure $ byteStringAtom bs
                                                   else if unCTR cTypeRep == typeRep (Proxy :: Proxy Bool) then
                                                          boolAtom <$> o .: "val"
                                                      else
                                                        error "unsupported typerep serialization"


instance ToJSON Notification
instance FromJSON Notification

instance ToJSON RelationalError
instance FromJSON RelationalError

instance ToJSON MergeError
instance FromJSON MergeError

instance ToJSON MergeStrategy
instance FromJSON MergeStrategy

instance ToJSON PersistenceError

