{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Atom where
import ProjectM36.Base
import ProjectM36.Error
import Data.Typeable
import ProjectM36.ConcreteTypeRep
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock
import Data.ByteString (ByteString)
import qualified Data.Map as M

relationForAtom :: Atom -> Either RelationalError Relation
relationForAtom (Atom atom) = case cast atom of
  Just rel@(Relation _ _) -> Right rel
  Nothing -> Left $ AttributeIsNotRelationValuedError ""
relationForAtom (ConstructedAtom _ _ _) = Left $ AttributeIsNotRelationValuedError ""

atomTypeForProxy :: (Atomable a) => Proxy a -> AtomType
atomTypeForProxy prox = AtomType $ CTR (typeRep prox)

--some convenience functions
stringAtomType :: AtomType
stringAtomType = atomTypeForProxy (Proxy :: Proxy StringType)

intAtomType :: AtomType
intAtomType = atomTypeForProxy (Proxy :: Proxy Int)

boolAtomType :: AtomType
boolAtomType = atomTypeForProxy (Proxy :: Proxy Bool)

dateAtomType :: AtomType
dateAtomType = atomTypeForProxy (Proxy :: Proxy Day)

dateTimeAtomType :: AtomType
dateTimeAtomType = atomTypeForProxy (Proxy :: Proxy UTCTime)

byteStringAtomType :: AtomType
byteStringAtomType = atomTypeForProxy (Proxy :: Proxy ByteString)

doubleAtomType :: AtomType
doubleAtomType = atomTypeForProxy (Proxy :: Proxy Double)

--another hard-coded typerep dispatch table :/
makeAtomFromText :: AttributeName -> AtomType -> T.Text -> Either RelationalError Atom
makeAtomFromText _ (AtomType cTypeRep) textIn = if 
  unCTR cTypeRep == typeRep (Proxy :: Proxy Int) then               
    either (Left . ParseError . T.pack) (Right . Atom) (fromText textIn :: Either String Int)
  else if unCTR cTypeRep == typeRep (Proxy :: Proxy Double) then                      
         either (Left . ParseError . T.pack) (Right . Atom) (fromText textIn :: Either String Double)
  else if unCTR cTypeRep == typeRep (Proxy :: Proxy T.Text) then                      
         either (Left . ParseError . T.pack) (Right . Atom) (fromText textIn :: Either String T.Text)
  else if unCTR cTypeRep == typeRep (Proxy :: Proxy Day) then                      
         either (Left . ParseError . T.pack) (Right . Atom) (fromText textIn :: Either String Day)
  else if unCTR cTypeRep == typeRep (Proxy :: Proxy UTCTime) then                   
         either (Left . ParseError . T.pack) (Right . Atom) (fromText textIn :: Either String UTCTime)
  else if unCTR cTypeRep == typeRep (Proxy :: Proxy ByteString) then                 
         either (Left . ParseError . T.pack) (Right . Atom) (fromText textIn :: Either String ByteString)
  else if unCTR cTypeRep == typeRep (Proxy :: Proxy Bool) then
         either (Left . ParseError . T.pack) (Right . Atom) (fromText textIn :: Either String Bool)
       else
         Left $ ParseError textIn
makeAtomFromText attrName _ _ = Left $ AtomTypeNotSupported attrName

--textAtom shortcut
stringAtom :: T.Text -> Atom
stringAtom t = Atom t

--intAtom shortcut
intAtom :: Int -> Atom
intAtom i = Atom i

--not safe to use without upfront type-checking
castRelation :: Atom -> Relation
castRelation (ConstructedAtom _ _ _) = error "castRelation attempt on ConstructedAtom"
castRelation (Atom atom) = case cast atom of
                             Just rel -> rel
                             Nothing -> error "castRelation attempt on non-relation-typed Atom"

unsafeCast :: (Atomable a) => Atom -> a
unsafeCast (ConstructedAtom _ _ _) = error "unsafeCast attempt on ConstructedAtom"
unsafeCast (Atom atom) = case cast atom of
                          Just x -> x
                          Nothing -> error "unsafeCast failed"

emptyAtomConstructor :: AtomConstructor
emptyAtomConstructor = AtomConstructor M.empty

basicAtomTypes :: AtomTypes
basicAtomTypes = M.fromList [("Int", (intAtomType, emptyAtomConstructor))]

