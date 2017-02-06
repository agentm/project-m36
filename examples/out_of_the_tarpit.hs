-- the Out-of-the-Tarpit example in Haskell and Project:M36
{-# LANGUAGE DeriveAnyClass #-}
import ProjectM36.Client
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad
import Data.Monoid

addressAtomType :: AtomType
addressAtomType = TextAtomType

nameAtomType :: AtomType
nameAtomType = TextAtomType

priceAtomType :: AtomType
priceAtomType = DoubleAtomType

fileNameAtomType :: AtomType
fileNameAtomType = TextAtomType

roomAtomType :: AtomType
roomAtomType = ConstructedAtomType "Room" M.empty

priceBandAtomType :: AtomType
priceBandAtomType = ConstructedAtomType "PriceBand" M.empty

areaCodeAtomType :: AtomType
areaCodeAtomType = ConstructedAtomType "AreaCode" M.empty

speedBandAtomType :: AtomType
speedBandAtomType = ConstructedAtomType "SpeedBand" M.empty
  
main :: IO ()
main = do
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback []
      check x = case x of 
        Left err -> error (show err)
        Right x' -> x'
  eConn <- connectProjectM36 connInfo
  let conn = check eConn
  
  eSessionId <- createSessionAtHead "master" conn  
  let sessionId = check eSessionId
  
  createSchema sessionId conn
  
createSchema :: SessionId -> Connection -> IO ()  
createSchema sessionId conn = do
  let propertyAttrs = [Attribute "address" addressAtomType,
                       Attribute "price" priceAtomType,
                       Attribute "photo" fileNameAtomType,
                       Attribute "dateRegistered" DayAtomType]
      offerAttrs = [Attribute "address" addressAtomType,
                    Attribute "offerPrice" priceAtomType,
                    Attribute "offerDate" DayAtomType,
                    Attribute "bidderName" nameAtomType,
                    Attribute "bidderAddress" addressAtomType,
                    Attribute "decisionDate" DayAtomType,
                    Attribute "accepted" BoolAtomType]
      decisionAttrs = [Attribute "address" addressAtomType,             
                       Attribute "offerDate" DayAtomType,
                       Attribute "bidderName" nameAtomType,
                       Attribute "bidderAddress" addressAtomType,
                       Attribute "decisionDate" DayAtomType,
                       Attribute "accepted" BoolAtomType]
      roomAttrs = [Attribute "address" addressAtomType, 
                   Attribute "roomName" TextAtomType,
                   Attribute "width" DoubleAtomType,
                   Attribute "breadth" DoubleAtomType,
                   Attribute "type" roomAtomType]
      floorAttrs = [Attribute "address" addressAtomType,
                    Attribute "roomName" TextAtomType,
                    Attribute "floor" IntAtomType]
      commissionAttrs = [Attribute "priceBand" priceBandAtomType,
                    Attribute "areaCode" areaCodeAtomType,
                    Attribute "saleSpeed" speedBandAtomType,
                    Attribute "commission" DoubleAtomType]
      idKey rvName attrNames = AddInclusionDependency (rvName <> "_key") $ inclusionDependencyForKey (AttributeNames (S.fromList attrNames)) (RelationVariable rvName ())
      incDepKeys =  map (uncurry idKey) 
                [("property", ["address"]),
                 ("offer", ["address", "offerDate", "bidderName", "bidderAddress"]),
                 ("decision", ["address", "offerDate", "bidderName", "bidderAddress"]),
                 ("room", ["address", "roomName"]),
                 ("floor", ["address", "roomName"]),
                 --"commision" misspelled in OotT
                 ("commission", ["priceBand", "areaCode", "saleSpeed"])
                 ]
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
      attrsL = AttributeNames . S.fromList
      makeFk (fkName, (rvA, attrsA), (rvB, attrsB)) = AddInclusionDependency fkName $ InclusionDependency (Project (attrsL attrsA) (RelationVariable rvA ())) (Project (attrsL attrsB) (RelationVariable rvB ()))
      incDepForeignKeys = map makeFk foreignKeys
      relvarMap = [("property", propertyAttrs),
                   ("offer", offerAttrs),
                   ("decision", decisionAttrs),
                   ("room", roomAttrs),
                   ("floor", floorAttrs),
                   ("commission", commissionAttrs)]
      simple_adt tCons dConsList = AddTypeConstructor (ADTypeConstructorDef tCons []) (map (\name -> DataConstructorDef name []) dConsList)
      new_adts = map (uncurry simple_adt) [
        ("Room", ["Kitchen", "Bathroom", "LivingRoom"]),
        ("PriceBand", ["Low", "Medium", "High", "Premium"]),
        ("AreaCode", ["City", "Suburban", "Rural"]),
        ("SpeedBand", ["VeryFastBand", "FastBand", "MediumBand", "SlowBand"])]
      rvDefs = map (\(name, attrs) -> Define name (map NakedAttributeExpr attrs)) relvarMap
  mErrs <- mapM (executeDatabaseContextExpr sessionId conn) (new_adts ++ rvDefs ++ incDepKeys ++ incDepForeignKeys)
  let errs = catMaybes mErrs
  when (length errs > 0) (error (show errs))
  
  
