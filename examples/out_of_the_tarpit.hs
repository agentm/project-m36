-- the Out-of-the-Tarpit example in Haskell and Project:M36
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
import ProjectM36.Client
import ProjectM36.DataTypes.Primitive
import ProjectM36.Tupleable
import ProjectM36.Error
import Data.Either
import GHC.Generics
import Data.Binary
import Control.DeepSeq
import qualified Data.Text as T
import Data.Time.Calendar
import qualified Data.Vector as V

--create various database value (atom) types
type Price = Double

type Name = T.Text

type Address = T.Text

data RoomType = Kitchen | Bathroom | LivingRoom
          deriving (Generic, Atomable, Eq, Show, Binary, NFData)
                   
data PriceBand = Low | Medium | High | Premium
               deriving (Generic, Atomable, Eq, Show, Binary, NFData)
                        
data AreaCode = City | Suburban | Rural
              deriving (Generic, Atomable, Eq, Show, Binary, NFData)

data SpeedBand = VeryFastBand | FastBand | MediumBand | SlowBand 
               deriving (Generic, Atomable, Eq, Show, Binary, NFData)

main :: IO ()
main = do
  --connect to the database
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback []
      check x = case x of 
        Left err -> error (show err)
        Right x' -> x'
  eConn <- connectProjectM36 connInfo
  let conn = check eConn
  
  --create a database session at the default branch of the fresh database
  eSessionId <- createSessionAtHead conn "master"
  let sessionId = check eSessionId

  createSchema sessionId conn
  
data Property = Property {  
  address :: T.Text,
  price :: Price,
  photo :: T.Text,
  dateRegistered :: Day
  }
              deriving (Generic, Eq)
                       
instance Tupleable Property                       

data Offer = Offer {
  offerAddress :: Address,
  offerPrice :: Price,
  offerDate :: Day,
  bidderName :: Name,
  bidderAddress :: Address,
  decisionDate :: Day,
  accepted :: Bool
  }
           deriving (Generic, Eq)
                    
instance Tupleable Offer                    
                    
data Decision = Decision {                    
  decAddress :: Address,
  decOfferDate :: Day, --the dec prefix is needed until OverloadedRecordFields is available
  decBidderName :: Name,
  decBidderAddress :: Address,
  decDecisionDate :: Day,
  decAccepted :: Bool
  }
  deriving (Generic, Eq)
           
instance Tupleable Decision           
                       
data Room = Room {
  roomAddress :: Address,
  roomName :: Name,
  width :: Double,
  breadth :: Double,
  roomType :: RoomType
  }
  deriving (Generic, Eq)
                         
instance Tupleable Room

data Floor = Floor {
  floorAddress :: Address,
  floorRoomName :: Name,
  floorNum :: Int
  }
  deriving (Generic, Eq)
           
instance Tupleable Floor

data Commission = Commission {
  priceBand :: PriceBand,
  areaCode :: AreaCode,
  saleSpeed :: SpeedBand,
  commission :: Price
  } deriving (Generic, Eq)
             
instance Tupleable Commission              

createSchema :: SessionId -> Connection -> IO ()  
createSchema sessionId conn = do
  --create attributes for relvars
  let 
      --create uniqueness constraints                     
      incDepKeys = map (uncurry databaseContextExprForUniqueKey)
                [("property", ["address"]),
                 ("offer", ["offerAddress", "offerDate", "bidderName", "bidderAddress"]),
                 ("decision", ["decAddress", "decOfferDate", "decBidderName", "decBidderAddress"]),
                 ("room", ["roomAddress", "roomName"]),
                 ("floor", ["floorAddress", "floorRoomName"]),
                 --"commision" misspelled in OotT
                 ("commission", ["priceBand", "areaCode", "saleSpeed"])
                 ]
      --create foreign key constraints
      foreignKeys = [("offer_property_fk", 
                      ("offer", ["offerAddress"]), 
                      ("property", ["address"])),
                     ("decision_offer_fk",
                      ("decision", ["decAddress", "decOfferDate", "decBidderName", "decBidderAddress"]),
                      ("offer", ["offerAddress", "offerDate", "bidderName", "bidderAddress"])),
                     ("room_property_fk",
                      ("room", ["roomAddress"]),
                      ("property", ["address"])),
                     ("floor_property_fk",
                      ("floor", ["floorAddress"]),
                      ("property", ["address"]))
                    ]
      incDepForeignKeys = map (\(n, a, b) -> databaseContextExprForForeignKey n a b) foreignKeys
      --define the relvars
      relvarMap = [("property", toAttributes (undefined :: Property)),
                   ("offer", toAttributes (undefined :: Offer)),
                   ("decision", toAttributes (undefined :: Decision)),
                   ("room", toAttributes (undefined :: Room)),
                   ("floor", toAttributes (undefined :: Floor)),
                   ("commission", toAttributes (undefined :: Commission))]
      rvDefs = map (\(name, typ) -> Define name (map NakedAttributeExpr (V.toList typ))) relvarMap     
      --create the new algebraic data types
      new_adts = [toDatabaseContextExpr (undefined :: RoomType),
                  toDatabaseContextExpr (undefined :: PriceBand),
                  toDatabaseContextExpr (undefined :: AreaCode),
                  toDatabaseContextExpr (undefined :: SpeedBand)]
      --create the stored atom functions
      priceBandScript = "(\\(DoubleAtom price:_) -> do\n let band = if price < 10000.0 then \"Low\" else if price < 20000.0 then \"Medium\" else if price < 30000.0 then \"High\" else \"Premium\"\n let aType = ConstructedAtomType \"PriceBand\" empty\n pure (ConstructedAtom band aType [])) :: [Atom] -> Either AtomFunctionError Atom"
      areaCodeScript = "(\\(TextAtom address:_) -> let aType = ConstructedAtomType \"AreaCode\" empty in if address == \"90210\" then pure (ConstructedAtom \"City\" aType []) else pure (ConstructedAtom \"Rural\" aType [])) :: [Atom] -> Either AtomFunctionError Atom"
      speedBandScript = "(\\(DayAtom d1:DayAtom d2:_) -> do\n let aType = ConstructedAtomType \"SpeedBand\" empty\n     (_, month1, _) = toGregorian d1\n     (_, month2, _) = toGregorian d2\n if month1 == 11 && month2 == 11 then pure (ConstructedAtom \"VeryFast\" aType []) else pure (ConstructedAtom \"MediumBand\" aType [])) :: [Atom] -> Either AtomFunctionError Atom"
      atomFuncs = [createScriptedAtomFunction "priceBandForPrice" [doubleTypeConstructor] (ADTypeConstructor "PriceBand" []) priceBandScript,
                   createScriptedAtomFunction "areaCodeForAddress" [textTypeConstructor] (ADTypeConstructor "AreaCode" []) areaCodeScript,
                   createScriptedAtomFunction "datesToSpeedBand" [dayTypeConstructor, dayTypeConstructor] (ADTypeConstructor "SpeedBand" []) speedBandScript
                  ]
  --gather up and execute all database updates
  putStrLn "load relvars"
  _ <- handleIOErrors $ mapM (executeDatabaseContextExpr sessionId conn) (new_adts ++ rvDefs ++ incDepKeys ++ incDepForeignKeys)
  
  putStrLn "load atom functions"
  _ <- handleIOErrors $ mapM (executeDatabaseContextIOExpr sessionId conn) atomFuncs
  
  putStrLn "load data"
  let properties = [Property { address = "123 Main St.",
                               price = 200000,
                               photo = "123_main.jpg",
                               dateRegistered = fromGregorian 2016 4 3},
                    Property { address = "456 Main St.",
                               price = 150000,
                               photo = "456_main.jpg",
                               dateRegistered = fromGregorian 2016 5 6}]
  insertPropertiesExpr <- handleError $ toInsertExpr properties "property"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertPropertiesExpr
  
  let offers = [Offer { offerAddress = "123 Main St.",
                        offerPrice = 180000,
                        offerDate = fromGregorian 2017 1 2,
                        bidderName = "Steve",
                        bidderAddress = "789 Main St.",
                        decisionDate = fromGregorian 2017 2 2,
                        accepted = False }]
  
  insertOffersExpr <- handleError $ toInsertExpr offers "offer"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertOffersExpr
  
  let rooms = [Room { roomAddress = "123 Main St.",
                      roomName = "Fabulous Kitchen",
                      width = 10,
                      breadth = 10,
                      roomType = Kitchen },
               Room { roomAddress = "123 Main St.",
                      roomName = "Clean Bathroom",
                      width = 7,
                      breadth = 5,
                      roomType = Bathroom }]
              
  insertRoomsExpr <- handleError $ toInsertExpr rooms "room"             
  handleIOError $ executeDatabaseContextExpr sessionId conn insertRoomsExpr
  
  let decisions = [Decision { decAddress = "123 Main St.",
                              decOfferDate = fromGregorian 2017 1 2,
                              decBidderName = "Steve",
                              decBidderAddress = "789 Main St.",
                              decDecisionDate = fromGregorian 2017 05 04,
                              decAccepted = False }]
  insertDecisionsExpr <- handleError $ toInsertExpr decisions "decision"                  
  handleIOError $ executeDatabaseContextExpr sessionId conn insertDecisionsExpr
  
  let floors = [Floor { floorAddress = "123 Main St.",
                        floorRoomName = "Bathroom",
                        floorNum = 1
                      }]
  insertFloorsExpr <- handleError $ toInsertExpr floors "floor"               
  handleIOError $ executeDatabaseContextExpr sessionId conn insertFloorsExpr
  
  let commissions = [Commission { priceBand = Medium,
                                  areaCode = City,
                                  saleSpeed = MediumBand,
                                  commission = 10000 }]
  insertCommissionsExpr <- handleError $ toInsertExpr commissions "commission"                    
  handleIOError $ executeDatabaseContextExpr sessionId conn insertCommissionsExpr

handleError :: Either RelationalError a -> IO a
handleError eErr = case eErr of
    Left err -> print err >> error "Died due to errors."
    Right v -> pure v
    
handleIOError :: IO (Either RelationalError a) -> IO a
handleIOError m = do
  e <- m
  handleError e

handleIOErrors :: IO [Either RelationalError a] -> IO [a]
handleIOErrors m = do
  eErrs <- m
  case lefts eErrs of
    [] -> pure (rights eErrs)    
    errs -> handleError (Left (someErrors errs))

  