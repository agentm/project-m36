{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DerivingVia #-}


import Codec.Winery (Serialise, WineryVariant(WineryVariant))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON(toEncoding, toJSON))
import Data.Data (Proxy(Proxy))
import Data.Either (lefts, rights)
import Data.Functor (($>))
import Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (pack)
import GHC.Generics (Generic)
import qualified ProjectM36.Base as Base
import ProjectM36.Client
       ( AtomExprBase(NakedAtomExpr)
       , Atomable(toAddTypeExpr, toAtom)
       , AttributeName
       , Connection
       , ConnectionInfo(InProcessConnectionInfo)
       , DatabaseContextExpr
       , DatabaseContextExprBase(Delete)
       , PersistenceStrategy(NoPersistence)
       , RelationalError
       , RelationalExprBase(RelationVariable, Restrict)
       , RestrictionPredicateExprBase(AttributeEqualityPredicate)
       , SessionId
       , close
       , commit
       , connectProjectM36
       , createSessionAtHead
       , databaseContextExprForUniqueKey
       , defaultHeadName
       , emptyNotificationCallback
       , executeDatabaseContextExpr
       , executeRelationalExpr
       , withTransaction
       )
import qualified ProjectM36.Relation as Relation
import ProjectM36.Tupleable
       ( Tupleable(fromTuple)
       , toDefineExpr
       , toDeleteExpr
       , toInsertExpr
       , toUpdateExpr
       )
import qualified System.Random as R


import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Web.Scotty as S

-- |    This is an example showcasing the updating of data
--      with the help of plants. \\
--      This is a very simple example and of course not how
--      plants grow in nature. \\
--      The main goal of this example is to showcas the updating
--      of data and the different options of persistance.



-- |    Here is a python script for pretty printing the output \
--      if you include the acii:
{-|
#!/usr/bin/python3
# this can be replaced with the path to your python executable
# this is a vary hacky script just for visualisation of the json response
import json
import sys
import re

for line in sys.stdin:
    parsed = json.loads(line)
    jstr = json.dumps(parsed, indent=4, sort_keys=True)
    leng = len(re.findall(r"((?: )*\"stage\": \")", jstr)[0])
    l = jstr.encode('utf-8').decode('unicode_escape')
    ls = l.split("\n")
    res = ""
    for i in ls:
        if not re.match(r"(?: )*(?:{|\"\w|}|\[|\])", i):
            spaces = leng
            if re.match(r"(?: )*\",$", i):
                spaces -= 1
            i = " " * spaces + i
        res += i + "\n"
    print(res)
-}
--      Either you make it executable with chmod +x scriptfilename or you \
--      execute it with python3 scriptfilename. \
--      Either way you then can use it to prettyprint when making a request. \
--      You can just pipe the putput form culr for eamxple into this command:
--
--      curl -X POST -H 'Accept: application/json' localhost:8001/plant/water/angu | python3 scriptfilename


data Stage = Seed | Sprout | Seedling | Adult | Dead
    deriving (Show, Eq, Ord, Generic, NFData, Atomable)
    deriving Serialise via WineryVariant Stage


instance ToJSON Stage where --  The where can be removed and the implementation commented out
                            --  if the ascii values are not wanted
  toJSON s = toJSON $ show s <> "\nascii only provided for visualisation:\n" <> stagetoAscii s

  toEncoding s = toEncoding $ show s <> "\nascii only provided for visualisation:\n" <> stagetoAscii s

instance FromJSON Stage

instance S.Parsable Stage where
  parseParam t
    | t == "seed" || t == "Seed" = Right Seed
    | t == "sprout" || t == "Sprout" = Right Sprout
    | t == "seedling" || t == "Seedling" = Right Seedling
    | t == "adult" || t == "Adult" = Right Adult
    | t == "dead" || t == "Dead" = Right Dead
    | otherwise = Left t


-- Just for ToJSON for ilustration of stage
type ASCIIStage = String

seedStr :: ASCIIStage
seedStr = "<>\n\
          \'''\n"
sproutStr :: ASCIIStage
sproutStr = " ..\n\
            \<|>\n\
            \''''\n"
seedlingStr :: ASCIIStage
seedlingStr = " `\\⁄o \n\
              \ v|/\n\
              \'´;'''\n"
adultStr :: ASCIIStage
adultStr = "*~(#)~\n\
           \ \\,Y,e\n\
           \  \\|/\n\
           \''+'\\''\n"
deadStr :: ASCIIStage
deadStr = "  ,__:<\n\
          \'/'''''\n"

-- Just used for ToJSON illustration of stages
stagetoAscii :: Stage -> ASCIIStage
stagetoAscii Seed = seedStr
stagetoAscii Sprout = sproutStr
stagetoAscii Seedling = seedlingStr
stagetoAscii Adult = adultStr
stagetoAscii Dead = deadStr

-- used for update stage
next :: Stage -> Stage
next Seed     = Sprout
next Sprout   = Seedling
next Seedling = Adult
next s        = s

data Plant = Plant { name :: Text, species :: Text, stage :: Stage, waterings:: Integer } deriving (Show, Generic)
instance Tupleable Plant
instance ToJSON Plant
instance FromJSON Plant


main :: IO ()
main = do
  putStrLn "Connecting to plant farm"
  c <- dbConnection
  _ <- createSchema c
  putStrLn "Planting some plants"
  _ <- insertSampleData c
  let port = 8001
  putStrLn $ "Started Plant farm at " <> show port
  S.scotty port $ do

    --  retive a plant by name
    S.get "/plant/:name" $ do
        n <- S.param "name"
        e <- liftIO $ getPlant c n
        p <- handleWebError e
        S.json p


    --  save a plant providing it as json data
    S.post "/plant" $ do
        pl <- S.jsonData :: S.ActionM Plant
        e <- liftIO $ savePlant c [pl]
        p <- handleWebError e
        S.json p

    --  updating a plant providing it as json data
    S.put "/plant" $ do
        pl <- S.jsonData :: S.ActionM Plant
        e <- liftIO $ updatePlantFst c pl
        p <- handleWebError e
        S.json p

    --  watering the plant having the provided name.
    --  This will water the plant and migth let it progress to the next stage. It might also die.
    S.post "/plant/water/:name" $ do
      n <- S.param "name"
      e <- liftIO $ waterPlant c n
      p <- handleWebError e
      S.json p

    --  retriving all the plants as json data
    S.get "/plants" $ do
      e <- liftIO $ getAllPlants c
      ps <- handleWebErrors e
      S.json ps

    --  saving many plants at the same time
    S.post "/plants" $ do
        pl <- S.jsonData :: S.ActionM [Plant]
        e <- liftIO $ savePlant c pl
        p <- handleWebError e
        S.json p

    --  deleting all plants at a speciffic stage
    S.delete "/plants?stage=:stage" $ do
      s <- S.param "stage"
      e <- liftIO $ deletePlantsByStage c s
      p <- handleWebError e
      S.json p

    --  deleting all plants at a speciffic stage
    S.delete "/plants" $ do
      s <- S.param "name"
      e <- liftIO $ deletePlantByName c s
      p <- handleWebError e
      S.json p

    --  deleting all dead plants
    S.delete "/plants/clear" $ do
      e <- liftIO $ clearDeadPlants c
      ps <- handleWebErrors e
      S.json ps

handleWebError :: Either Err b -> S.ActionM b
handleWebError (Left e) = S.raise (TL.pack $ "An error Orrcured:\n" <> show e)
handleWebError (Right v) = pure v

handleWebErrors :: [Either Err b] -> S.ActionM [b]
handleWebErrors e = do
  case lefts e of
    [] -> pure (rights e)
    l -> S.raise (TL.pack $ "Errors Orrcured:\n" <> concatMap ((<>"\n") . show)l)


-- |    watering a plant and therby possibly updating its stage
waterPlant :: DBConnection -> Text -> IO (Either Err Plant)
waterPlant db n = do
  p <- getPlant db n
  case p of
        Left e  -> pure $ Left e
        Right v -> updateStage db $ v { waterings = waterings v + 1 }

-- |    used by 'waterPlant' updates the stage depending on random numbers
updateStage :: DBConnection -> Plant ->  IO (Either Err Plant)
updateStage db p@(Plant _ _ Dead _ ) = do
  tmp <- updatePlantFst db p
  case tmp of
    Left e  -> pure $ Left e
    Right _ -> pure $ Right p
updateStage db p = do
  res <- calculateStage p
  tmp <- updatePlantFst db res
  case tmp of
    Left e  -> pure $ Left e
    Right _ -> pure $ Right res
  where calculateStage p = do
          r1 <- R.randomRIO (1, 10) :: IO Integer
          let np = if r1 < waterings p then p { stage = next $ stage p, waterings = 0 } else p
          r2 <- R.randomRIO (1, 20)
          pure $ if r2 < waterings np then np { stage = Dead, waterings = 0 } else np

-- |    deletes all plants with Stage = Dead
clearDeadPlants :: DBConnection -> IO [Either Err Plant]
clearDeadPlants db = do
  _ <- deletePlantsByStage db Dead
  getAllPlants db


-- ****************************
-- |    *Database functions* :
-- ****************************


-- |    Error type for passing. It is not very specific.
--      Just minimal and non optimal for this example.
data Err = NotSpecified | NotFound deriving (Show ,Generic)

instance ToJSON Err

-- |    Just for convinience for passing around the SessionId
--      and the Connection
data DBConnection = DB SessionId Connection


-- |    Getting plants by their name.\
--      Because plant has name as primary key \
--      we can assume will get at most one result. \
--      Alternatively we could return a list.
getPlant :: DBConnection -> Text -> IO (Either Err Plant)
getPlant db n =  defaultHead (Left NotFound)  <$> (
    get db
    ( Restrict -- Restrict can be thought of as similar to "WITH" in SQL
      (AttributeEqualityPredicate "name" (NakedAtomExpr (toAtom n))) -- This is the Predicate we are "restricting" our RelationalExprBase with
      (RelationVariable "plants" ())
    )
    :: IO [Either Err Plant] -- this is needed because our 'get' function is
  )

defaultHead :: a -> [a] -> a
defaultHead d []    = d
defaultHead _ (x:_) = x

-- |    Getting all the plants
getAllPlants :: DBConnection -> IO [Either Err Plant]
getAllPlants db = get db (RelationVariable "plants" ())

-- |    Getting all the plants that satisfy the restrictions
getAllPlantsWith :: DBConnection -> Base.RestrictionPredicateExpr -> IO [Either Err Plant]
getAllPlantsWith db ex = get db (Restrict ex (RelationVariable "plants" ()))


-- |    Saving a plant
savePlant :: (Traversable t) => DBConnection -> t Plant -> IO (Either Err ())
savePlant db sps = insert db sps "plants"

-- |    An alternative to update alone because due to updates
--      not being cached yet \
--      they have to be calculated every update. \
--      This seems like an alternative until caching is implemented.
updatePlantFst :: DBConnection -> Plant -> IO (Either Err ())
updatePlantFst db pln = do
  _ <- delete db pln ["name"] "plants"
  savePlant db [pln]

-- |    Updating a plant. Due to the way project 36 works, \
--      updates have to be executed every time.\
--      This is slow but won't be an issue soon because they will
--      be cached.\
--      But because of this project 36 can provide O(1) commits.
updatePlant :: DBConnection -> Plant -> IO (Either Err ())
updatePlant db sps = update
  db          -- db connection
  sps         -- plant value
  ["name"]   -- the attributes to be updated by (in case of unique key)
  "plants"    -- the relation we want to update on


-- |    deleting all plants that have given name.
deletePlantByName :: DBConnection -> Text -> IO (Either Err ())
deletePlantByName db s = executeWithTransaction db $ Right $ Delete "plants" $ AttributeEqualityPredicate "name" (NakedAtomExpr (toAtom s))

-- |    deleting all plants that have given stage.
deletePlantsByStage :: DBConnection -> Stage -> IO (Either Err ())
deletePlantsByStage db s = executeWithTransaction db $ Right $ Delete "plants" $ AttributeEqualityPredicate "stage" (NakedAtomExpr (toAtom s))


-- |    Inserting the schema into the DB
createSchema :: DBConnection -> IO ()
createSchema (DB sessionId conn) = do
  _ <- handleIOErrorsAndQuit $ mapM (executeDatabaseContextExpr sessionId conn) [
        toAddTypeExpr (Proxy :: Proxy Stage) -- Adds the Type Stage as data to the DB
    ,   toDefineExpr (Proxy :: Proxy Plant) "plants" -- Creates the plants relation
    ,   databaseContextExprForUniqueKey "plants" ["name"] -- Makes name of the plants relation a uniqe Key,
                                                          -- Foreign Key restrictions are available too
    ]
  pure ()


-- |    inserting some sample data
insertSampleData :: DBConnection -> IO (Either Err ())
insertSampleData (DB sid conn) = do
    insert (DB sid conn) [
        Plant "angu" "Caladenia angustata" Seed 0
      , Plant "thely" "Thelymitra alcockiae" Seed 0
      , Plant "monstera" "Araceae" Seed 0
      ] "plants"

dbConnection :: IO DBConnection
dbConnection = do
  --  connect to the database
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback []
  --  The code below persists the data in a DB with the name "base". \\
--  let connInfo = InProcessConnectionInfo (CrashSafePersistence "base") emptyNotificationCallback [] \\
  --  In addition minimal persistance is available. \\
--  let connInfo = InProcessConnectionInfo (MinimalPersistence "base") emptyNotificationCallback []
  conn <- handleIOErrorAndQuit $ connectProjectM36 connInfo
  --create a database session at the default branch of the database
  sessionId <- handleIOErrorAndQuit $ createSessionAtHead conn defaultHeadName
  pure (DB sessionId conn)


-- |    A polymorphic function to insert data (a traversable of data) into the DB
insert :: (Tupleable a, Traversable t) => DBConnection -> t a -> Base.RelVarName -> IO (Either Err ())
insert db rlv rlvName = executeWithTransaction db $ toInsertExpr rlv rlvName

-- |    A polymorphic function to update data in the DB.
--      An update in one funtion would take:
--
--      - SessionId
--      - Connection
--      - Tuplable a        -- the Tuplable you want to update
--      - [AttributeName]   -- the Attributes of the Tuplable you want to update those are of StringType
--      - RelVarName        -- the name of the relation
--
--      With that you can via 'toUpdateExpr' create a 'DatabaseContextExpr' to be executed. \\
--      For inserting its very similarly done with 'toInsertExpr'.
update :: (Tupleable a) => DBConnection -> a -> [AttributeName]-> Base.RelVarName -> IO (Either Err ())
update db rlv attr rlvName = executeWithTransaction db $ toUpdateExpr rlvName attr rlv

-- |    A polymorphic function to delete data in the DB
delete :: (Tupleable a) => DBConnection -> a -> [AttributeName]-> Base.RelVarName -> IO (Either Err ())
delete db rlv attr rlvName = executeWithTransaction db $ toDeleteExpr rlvName attr rlv

-- |    A convinience function to make executing DBContextExpr with commiting simpler. \\
--      In particular for expr that just insert, delete and update.
--      Therefor ultimately return Either _ ()
executeWithTransaction :: DBConnection -> Either RelationalError DatabaseContextExpr -> IO (Either Err ())
executeWithTransaction (DB sid conn) expr = do
    iEx <- handleError expr
    case iEx of
      Left e -> pure $ Left e
      Right v -> handleIOError $ withTransaction sid conn (executeDatabaseContextExpr sid conn v) (commit sid conn)

-- |    A polymorphic function to get data (a list of data and possibly errors) from the DB
get :: Tupleable b => DBConnection -> Base.RelationalExpr -> IO [Either Err b]
get (DB sessionId conn) q = do
  eRel <-  executeRelationalExpr sessionId conn q
  e <- handleError eRel
  case e of
    Left err -> pure [Left err]
    Right pRel -> do
      ws <- Relation.toList pRel
      mapM (handleError . fromTuple) ws

-- |    for closing the DB connection.
--      Not really needed when using in-memory DB
closeConn :: DBConnection -> IO ()
closeConn (DB _ conn) = close conn


-- |    Error handling is heavily inspired by 'blog.hs' the blog example \\
--      by (agentm)[https://github.com/agentm] (https://github.com/agentm/project-m36, commit: f8432522adaafeae7c32bc2b8b6cb09c00396fc6). \\
--      As stated there your application should have propper error handling.
handleIOErrorAndQuit :: Show e => IO (Either e a) -> IO a
handleIOErrorAndQuit m = do
  v <- m
  handleErrorAndQuit v

handleError :: Show e => Either e a -> IO (Either Err a)
handleError eErr = case eErr of
  Left err -> print err $> Left NotSpecified
  Right v  -> pure $ Right v

handleIOError :: Show e => IO (Either e a) -> IO (Either Err a)
handleIOError m = do
  v <- m
  handleError v

handleErrorAndQuit :: Show e => Either e a -> IO a
handleErrorAndQuit eErr = case eErr of
  Left err -> print err >> error "Quit."
  Right v  -> pure v

handleIOErrorsAndQuit :: Show e => IO [Either e a] -> IO [a]
handleIOErrorsAndQuit m = do
  eErrs <- m
  case lefts eErrs of
    []    -> pure (rights eErrs)
    err:_ -> handleErrorAndQuit (Left err)

