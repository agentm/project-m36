module ProjectM36.Atom where
import ProjectM36.Base
import ProjectM36.Error
import Data.Typeable
import ProjectM36.ConcreteTypeRep
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock
import Data.ByteString (ByteString)

relationForAtom :: Atom -> Either RelationalError Relation
relationForAtom (Atom atom) = case cast atom of
  Just rel@(Relation _ _) -> Right rel
  Nothing -> Left $ AttributeIsNotRelationValuedError ""
relationForAtom (ConstructedAtom _ _ _) = Left $ AttributeIsNotRelationValuedError ""

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

textAtom :: T.Text -> Atom
textAtom t = Atom t

--intAtom shortcut
intAtom :: Int -> Atom
intAtom i = Atom i

doubleAtom :: Double -> Atom
doubleAtom d = Atom d

byteStringAtom :: ByteString -> Atom
byteStringAtom b = Atom b

boolAtom :: Bool -> Atom
boolAtom b = Atom b

--not safe to use without upfront type-checking
castRelation :: Atom -> Relation
castRelation (ConstructedAtom _ _ _) = error "castRelation attempt on ConstructedAtom"
castRelation (Atom atom) = case cast atom of
                             Just rel -> rel
                             Nothing -> error "castRelation attempt on non-relation-typed Atom"

{-
basicAtomTypes :: AtomTypes
basicAtomTypes = M.union primitiveAtomTypes moreTypes
  where
    moreTypes = M.fromList [("Day", (dateAtomType, 
                                     AtomConstructor (M.singleton "Day" ["Int"])))
                            ]
-}  
atomToText :: Atom -> T.Text
atomToText (Atom atom) = toText atom
atomToText (ConstructedAtom dConsName _ atoms) = dConsName `T.append` T.intercalate " " (map atomToText atoms)



