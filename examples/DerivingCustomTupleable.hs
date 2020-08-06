{-# LANGUAGE DeriveGeneric, DerivingVia, GeneralizedNewtypeDeriving, TypeOperators, DataKinds, OverloadedStrings #-}

import ProjectM36.Tupleable.Deriving
import ProjectM36.Atomable (Atomable)
import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype BlogId = BlogId { getBlogId :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)

instance NFData BlogId
instance Binary BlogId
instance Atomable BlogId

data Blog = Blog
  { blogId :: BlogId
  , blogTitle :: Text
  , blogAuthorName :: Text
  }
  deriving stock (Show, Generic)
  deriving (Tupleable)
    via Codec (Field (DropPrefix "blog" >>> CamelCase)) Blog

data Comment = Comment
  { commentAuthorName :: Text
  , commentComment :: Text
  , commentFor :: BlogId
  }
  deriving stock (Show, Generic)
  deriving (Tupleable)
    via Codec (Field (DropPrefix "comment" >>> CamelCase)) Comment

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
