-- a simple example of a blog schema
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}

import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.Tupleable

import Data.Either
import GHC.Generics
import Data.Binary
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Calendar
import Control.DeepSeq

--define your data types
data Blog = Blog {
  title :: T.Text,
  stamp :: UTCTime,
  category :: Category --note that this type is an algebraic data type
  }
          deriving (Generic, Show) --derive Generic so that Tupleable can use default instances
                   
--instantiate default Tupleable instances
instance Tupleable Blog

data Comment = Comment {
  blogTitle :: T.Text,
  commentTime :: UTCTime,
  contents :: T.Text
  } deriving (Generic, Show)
             
instance Tupleable Comment             

data Category = Food | Cats | Photos | Other T.Text -- note that this data type could not be represented by an "enumeration" as found in SQL databases
              deriving (Atomable, Eq, Show, NFData, Binary, Generic) -- derive Atomable so that values of this type can be stored as a database value
                       
-- add some short-hand error handling- your application should have proper handling
handleIOError :: Show e => IO (Either e a) -> IO a
handleIOError m = do
  v <- m
  handleError v
    
handleError :: Show e => Either e a -> IO a
handleError eErr = case eErr of
    Left err -> print err >> error "Died due to errors."
    Right v -> pure v
    
handleIOErrors :: Show e => IO [Either e a] -> IO [a]
handleIOErrors m = do
  eErrs <- m
  case lefts eErrs of
    [] -> pure (rights eErrs)    
    err:_ -> handleError (Left err)

main :: IO ()                       
main = do
  --connect to the database
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback []
  conn <- handleIOError $ connectProjectM36 connInfo
  
  sessionId <- handleIOError $ createSessionAtHead conn "master"

  createSchema sessionId conn  
  insertSampleData sessionId conn
  executeSampleQueries sessionId conn
  
--define the schema with the new Category atom (data) type, blog relvar, a comment relvar, and a foreign key relationship between them
createSchema :: SessionId -> Connection -> IO ()  
createSchema sessionId conn = do
  _ <- handleIOErrors $ mapM (executeDatabaseContextExpr sessionId conn) [
    toAddTypeExpr (undefined :: Category),
    toDefineExpr (undefined :: Blog) "blog",
    toDefineExpr (undefined :: Comment) "comment",
    databaseContextExprForForeignKey "blog_comment" ("comment", ["blogTitle"]) ("blog", ["title"]) ]
  pure ()

--create some sample values and insert them into the database's relation variables
insertSampleData :: SessionId -> Connection -> IO ()
insertSampleData sessionId conn = do
  let blogs = [Blog { title = "Eat More Tofu",
                      stamp = UTCTime (fromGregorian 2017 5 8) (secondsToDiffTime 1000),
                      category = Food },
               Blog { title = "Cat Falls Off Table",
                      stamp = UTCTime (fromGregorian 2017 6 10) (secondsToDiffTime 2000),
                      category = Cats }
               ]
      comments = [Comment { blogTitle = "Cat Falls Off Table",
                            commentTime = UTCTime (fromGregorian 2017 7 8) (secondsToDiffTime 3000),
                            contents = "more cats please" }]
  insertBlogsExpr <- handleError $ toInsertExpr blogs "blog"               
  handleIOError $ executeDatabaseContextExpr sessionId conn insertBlogsExpr
  
  insertCommentsExpr <- handleError $ toInsertExpr comments "comment"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertCommentsExpr
  
--issue a query and marshal the data back to the original data value  
executeSampleQueries :: SessionId -> Connection -> IO ()  
executeSampleQueries sessionId conn = do
  commentsRelation <- handleIOError $ executeRelationalExpr sessionId conn (RelationVariable "comment" ())
  
  comments <- toList commentsRelation >>= mapM (handleError . fromTuple) :: IO [Comment]
  print comments
