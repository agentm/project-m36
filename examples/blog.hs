-- a simple example of a blog schema
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, CPP, DerivingVia #-}

import ProjectM36.Client
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tupleable
import ProjectM36.Atom (relationForAtom)
import ProjectM36.Tuple (atomForAttributeName)

import Data.Either
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock
import Data.Time.Calendar
import Control.DeepSeq
import Data.Proxy
import Data.List
import Control.Monad (when, forM_)
import Codec.Winery

import Web.Scotty as S
import Text.Blaze.Html5 (h1, h2, h3, p, form, input, (!), toHtml, Html, a, toValue, hr, textarea)
import Text.Blaze.Html5.Attributes (name, href, type_, method, action, value)
import Text.Blaze.Html.Renderer.Text
--import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status
import Data.Time.Format.ISO8601
import System.Random

--define your data types
data Blog = Blog {
  title :: T.Text,
  entry :: T.Text,
  tstamp :: UTCTime,
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
              deriving (Atomable, Eq, Show, NFData, Generic) -- derive Atomable so that values of this type can be stored as a database value
              deriving Serialise via WineryVariant Category -- derive Serialisable to be able to transmit this data remotely
                       
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
  rando <- initStdGen
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback [] basicDatabaseContext rando "admin"
  conn <- handleIOError $ connectProjectM36 connInfo
  
  sessionId <- handleIOError $ createSessionAtHead conn "master"

  createSchema sessionId conn  
  insertSampleData sessionId conn
  --create the web routes
  scotty 3000 $ do
    S.get "/" (listBlogs sessionId conn)
    S.get "/blog/:blogid" (showBlogEntry sessionId conn)
    S.post "/comment" (addComment sessionId conn)
  
--define the schema with the new Category atom (data) type, blog relvar, a comment relvar, and a foreign key relationship between them
createSchema :: SessionId -> Connection -> IO ()  
createSchema sessionId conn = do
  _ <- handleIOErrors $ mapM (executeDatabaseContextExpr sessionId conn) [
    toAddTypeExpr (Proxy :: Proxy Category),
    toDefineExpr (Proxy :: Proxy Blog) "blog",
    toDefineExpr (Proxy :: Proxy Comment) "comment",
    databaseContextExprForForeignKey "blog_comment" ("comment", ["blogTitle"]) ("blog", ["title"]),
    databaseContextExprForUniqueKey "blog" ["title"]
    ]
  pure ()

--create some sample values and insert them into the database's relation variables
insertSampleData :: SessionId -> Connection -> IO ()
insertSampleData sessionId conn = do
  let blogs = [Blog { title = "Haskell Lenses",
                      entry = "I wear Haskell rose-colored lenses.",
                      tstamp = UTCTime (fromGregorian 2017 5 8) (secondsToDiffTime 1000),
                      category = Food },
               Blog { title = "Haskell Monad Analogy",
                      entry = "Monads are like burritos going through intestines.",
                      tstamp = UTCTime (fromGregorian 2017 6 10) (secondsToDiffTime 2000),
                      category = Cats }
               ]
      comments = [Comment { blogTitle = "Haskell Lenses",
                            commentTime = UTCTime (fromGregorian 2017 7 8) (secondsToDiffTime 3000),
                            contents = "You suck!" },
                  Comment {blogTitle = "Haskell Lenses",
                           commentTime = UTCTime (fromGregorian 2017 7 9) (secondsToDiffTime 2000),
                           contents = "I find your ideas intriguing and would like to subscribe to your newsletter."}
                 ]
  insertBlogsExpr <- handleError $ toInsertExpr blogs "blog"               
  handleIOError $ executeDatabaseContextExpr sessionId conn insertBlogsExpr
  
  insertCommentsExpr <- handleError $ toInsertExpr comments "comment"
  handleIOError $ executeDatabaseContextExpr sessionId conn insertCommentsExpr
  
-- handle relational errors with scotty
handleWebError :: Either RelationalError a -> ActionM a 
handleWebError (Left err) = render500 (toHtml (show err)) >> pure (error "bad")
handleWebError (Right v) = pure v

-- show a page with all the blog entries
listBlogs :: SessionId -> Connection -> ActionM ()
listBlogs sessionId conn = do
  eRel <- liftIO $ executeRelationalExpr sessionId conn (RelationVariable "blog" ())
  case eRel of
    Left err -> render500 (toHtml (show err))
    Right blogRel -> do
      blogs <- liftIO (toList blogRel) >>= mapM (handleWebError . fromTuple) :: ActionM [Blog]
      let sortedBlogs = sortBy (\b1 b2 -> tstamp b1 `compare` tstamp b2) blogs
      html . renderHtml $ do
        h1 "Blog Posts"
        forM_ sortedBlogs $ \blog -> a ! href (toValue $ "/blog/" <> title blog) $ h2 (toHtml (title blog))

render500 :: Html -> ActionM ()
render500 msg = do 
  html . renderHtml $ do
    h1 "Internal Server Error"  
    p msg
  status internalServerError500
  
--display one blog post along with its comments
showBlogEntry :: SessionId -> Connection -> ActionM ()
showBlogEntry sessionId conn = do
  blogid <- pathParam "blogid"
  --query the database to return the blog entry with a relation-valued attribute of the associated comments
  let blogRestrictionExpr = AttributeEqualityPredicate "title" (NakedAtomExpr (TextAtom blogid))
      extendExpr = AttributeExtendTupleExpr "comments" (RelationAtomExpr commentsRestriction)
      commentsRestriction = Restrict
                           (AttributeEqualityPredicate "blogTitle" (AttributeAtomExpr "title"))
                           (RelationVariable "comment" ())
  eRel <- liftIO $ executeRelationalExpr sessionId conn (Extend extendExpr 
                                                         (Restrict 
                                                          blogRestrictionExpr 
                                                          (RelationVariable "blog" ())))
  let render = html . renderHtml
      formatStamp = iso8601Show --formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
  case eRel of 
    Left err -> render500 (toHtml (show err))
    --handle successful query execution
    Right rel -> case singletonTuple rel of
      Nothing -> do --no results for this blog id
        render (h1 "No such blog post")
        status status404
      Just blogTuple -> case fromTuple blogTuple of --just one blog post found- it's a match!
        Left err -> render500 (toHtml (show err))
        Right blog -> do
          --extract comments for the blog
          commentsAtom <- handleWebError (atomForAttributeName "comments" blogTuple)
          commentsRel <- handleWebError (relationForAtom commentsAtom)
          comments <- liftIO (toList commentsRel) >>= mapM (handleWebError . fromTuple) :: ActionM [Comment]
          let commentsSorted = sortBy (\c1 c2 -> commentTime c1 `compare` commentTime c2) comments
          render $ do
            --show blog details
            h1 (toHtml (title blog))
            p (toHtml ("Posted at " <> formatStamp (tstamp blog) <> " under " <> show (category blog)))
            p (toHtml (entry blog))
            hr
            h3 "Comments"
            --list the comments
            forM_ commentsSorted $ \comment -> do
              p (toHtml ("Commented at " <> formatStamp (commentTime comment)))
              p (toHtml (contents comment))
            when (null comments) (p "No comments.")
            --add a comment form
            h3 "Add a Comment"
            form ! method "POST" ! action "/comment" $ do
              input ! type_ "hidden" ! name "blogid" ! value (toValue blogid)
              textarea ! name "contents" $ ""
              input ! type_ "submit"
            
--add a comment to a blog post
addComment :: SessionId -> Connection -> ActionM ()            
addComment sessionId conn = do
  blogid <- pathParam "blogid"
  commentText <- formParam "contents"
  now <- liftIO getCurrentTime
  
  case toInsertExpr [Comment {blogTitle = blogid,
                              commentTime = now,
                              contents = commentText }] "comment" of
    Left err -> handleWebError (Left err)
    Right insertExpr -> do      
      eRet <- liftIO (withTransaction sessionId conn (executeDatabaseContextExpr sessionId conn insertExpr) (commit sessionId conn))
      case eRet of
        Left err -> handleWebError (Left err)
        Right _ ->
          redirect (TL.fromStrict ("/blog/" <> blogid))
      
