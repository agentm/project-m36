-- the Out-of-the-Tarpit example in Haskell and Project:M36
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
import ProjectM36.Client
import ProjectM36.DataTypes.Primitive
import ProjectM36.Tupleable
import Data.Either
import Control.Monad
import GHC.Generics
import Data.Binary
import Control.DeepSeq
import qualified Data.Text as T
import Data.Time.Calendar (Day)
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
  
data Test = Test {  
  dateo :: Int,
  date2o :: Int
  }
          deriving (Generic, Eq)
                   
instance Tupleable Test                   
  
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
  dec_address :: Address,
  dec_offerDate :: Day, --the dec_ prefix is needed until OverloadedRecordFields is available
  dec_bidderName :: Name,
  dec_bidderAddress :: Address,
  dec_decisionDate :: Day,
  dec_accepted :: Bool
  }
  deriving (Generic, Eq)
           
instance Tupleable Decision           
                       
data Room = Room {
  room_address :: Address,
  roomName :: Name,
  width :: Double,
  breadth :: Double,
  roomType :: RoomType
  }
  deriving (Generic, Eq)
                         
instance Tupleable Room

data Floor = Floor {
  floor_address :: Address,
  floor_roomName :: Name,
  floor :: Int
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
                 ("offer", ["address", "offerDate", "bidderName", "bidderAddress"]),
                 ("decision", ["address", "offerDate", "bidderName", "bidderAddress"]),
                 ("room", ["address", "roomName"]),
                 ("floor", ["address", "roomName"]),
                 --"commision" misspelled in OotT
                 ("commission", ["priceBand", "areaCode", "saleSpeed"])
                 ]
      --create foreign key constraints
      foreignKeys = [("offer_property_fk", 
                      ("offer", ["address"]), 
                      ("property", ["address"])),
                     ("decision_offer_fk",
                      ("decision", ["address", "offerDate", "bidderName", "bidderAddress"]),
                      ("offer", ["address", "offerDate", "bidderName", "bidderAddress"])),
                     ("room_property_fk",
                      ("room", ["address"]),
                      ("property", ["address"])),
                     ("floor_property_fk",
                      ("floor", ["address"]),
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
  eErrs <- mapM (executeDatabaseContextExpr sessionId conn) (new_adts ++ rvDefs ++ incDepKeys ++ incDepForeignKeys)
  let errs = lefts eErrs
  unless (null errs) (putStrLn (show errs))    
  
  eErrs' <- mapM (executeDatabaseContextIOExpr sessionId conn) atomFuncs
  let errs' = lefts eErrs'
  unless (null errs') (putStrLn (show errs'))