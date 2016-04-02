-- the Out-of-the-Tarpit example in Haskell and Project:M36
{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
import ProjectM36.Client
import qualified Data.Text as T
import ProjectM36.Atom
import Data.Proxy

type Text = T.Text

--custom types cannot yet be made due to hardcoding in the Atom Binary implementation
type Address = Text
type Agent = Text
type Name = Text
type Price = Double
type FileName = Text
data RoomType = Kitchen | Bathroom | LivingRoom deriving (Atomable)
roomTypeAtomType = atomTypeForProxy (Proxy :: RoomType)

data PriceBand = Low | Medium | High | Premium 
priceBandAtomType = atomTypeForProxy (Proxy :: PriceBand)

data AreaCode = City | Suburban | Rural
areaCodeAtomType = atomTypeForProxy (Proxy :: AreaCode)

data SpeedBand = VeryFastBand | FastBand | MediumBand | SlowBand
speedBandAtomType = atomTypeForProxy (Proxy :: SpeedBand)

  
main :: IO ()
main = do
  let connInfo = InProcessConnectionInfo emptyNotificationCallback
      check x = case x of 
        Left err -> error (show err)
        Right x' -> x'
  eConn <- connectProjectM36 connInfo
  let conn = check eConn
  
  eSessionId <- createSessionAtHead "master" conn
  let sessionId = check eSessionId
  
  createSchema sessionId conn
  
createSchema :: Connection -> IO ()  
createSchema conn = do
  let propertyAttrs = [Attribute "address" addressAtomType,
                       Attribute "price" priceAtomType
                       Attribute "photo" fileNameAtomType,
                       Attribute "dateRegistered" dateAtomType]
      offerAttrs = [Attribute "address" addressAtomType,
                    Attribute "offerPrice" priceAtomType,
                    Attribute "offerDate" dateAtomType,
                    Attribute "bidderName" nameAtomType,
                    Attribute "bidderAddress" addressAtomType,
                    Attribute "decisionDate" dateAtomType,
                    Attribute "accepted" boolAtomType]
      decisionAttrs = [Attribute "address" addressAtomType,             
                       Attribute "offerDate" dateAtomType,
                       Attribute "bidderName" nameAtomType,
                       Attribute "bidderAddress" addressAtomType,
                       Attribute "decisionDate" dateAtomType,
                       Attribute "accepted" boolAtomType]
      roomAttrs = [Attribute "address" addressAtomType, 
                   Attribute "roomName" stringAtomType,
                   Attribute "width" doubleAtomType,
                   Attribute "breadth" doubleAtomType,
                   Attribute "type" roomTypeAtomType]
      floorAttrs = [Attribute "address" addressAtomType,
                    Attribute "roomName" stringAtomType,
                    Attribute "floor" intAtomType]
      commissionAttrs = [Attribute "priceBand" priceBandAtomType,
                    Attribute "areaCode" areaCodeAtomType,
                    Attribute "saleSpeed" speedBandAtomType,
                    Attribute "commission" doubleAtomType]
      relvarMap = [("property", propertyAttrs),
                   ("offer", offerAttrs),
                   ("decision", decisionAttrs),
                   ("room", roomAttrs),
                   ("floor", floorAttrs),
                   ("commission", commissionAttrs)]
      dbcontextExprs = map (\(name, attrs) -> Define name (attributesFromList attrs)) relvarMap
  mapM executeDatabaseContextExpr dbcontextExprs
  
  
  