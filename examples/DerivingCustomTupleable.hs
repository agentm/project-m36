{-# LANGUAGE DeriveGeneric, DerivingVia, GeneralizedNewtypeDeriving, TypeOperators, DataKinds, OverloadedStrings #-}

import ProjectM36.Tupleable.Deriving
import ProjectM36.Atomable (Atomable)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Codec.Winery

newtype BlogId = BlogId { getBlogId :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving Serialise via WineryRecord BlogId

instance NFData BlogId
instance Atomable BlogId

data Blog = Blog
  { blogId :: BlogId
  , blogTitle :: Text
  , blogAuthorName :: Text
  }
  deriving stock (Show, Generic)
  deriving (Tupleable)
    via Codec (Field (DropPrefix "blog" >>> CamelCase)) Blog
  deriving Serialise via WineryRecord Blog

data Comment = Comment
  { commentAuthorName :: Text
  , commentComment :: Text
  , commentFor :: BlogId
  }
  deriving stock (Show, Generic)
  deriving (Tupleable)
    via Codec (Field (DropPrefix "comment" >>> CamelCase)) Comment
  deriving Serialise via WineryRecord Comment

main :: IO ()
main = do
  let exampleBlog = Blog
        { blogId = 0
        , blogTitle = "Cat Pics"
        , blogAuthorName = "Alice"
        }
      exampleComment = Comment
        { commentAuthorName = "Bob"
        , commentComment = "great"
        , commentFor = 0
        }
  print exampleBlog
  print (toTuple exampleBlog)
  putStrLn ""
  print exampleComment
  print (toTuple exampleComment)
