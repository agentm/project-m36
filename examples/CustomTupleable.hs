{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
--this example shows how to implement a non-generics-defined Tupleable instance which is required in scenarios where Haskell-side types are not supported server-side or where one wishes to represent a nested relation
import ProjectM36.Client
import ProjectM36.Tuple
import ProjectM36.Tupleable
import ProjectM36.Relation
import ProjectM36.Atom
import ProjectM36.Attribute as A
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics
import Data.Proxy

--in this contrived example, we wish to represent one relation with a nested relation of comments
data Blog = Blog {
  title :: Text,
  comments :: S.Set Comment
  } deriving (Show)
             
data Comment = Comment {
  authorName :: Text,
  comment :: Text
  } deriving (Eq, Show, Generic, Ord)
             
instance Tupleable Comment

instance Tupleable Blog where
  toTuple blogentry =
    mkRelationTupleFromMap (M.fromList [("title", TextAtom (title blogentry)), 
                                        ("comments", RelationAtom relFromComments)])
    where
      commentTuples = map toTuple (S.toList (comments blogentry))
      relFromComments = case mkRelationFromTuples (toAttributes (Proxy :: Proxy Comment)) commentTuples of
        Left err -> error (show err)
        Right rel -> rel
   
  fromTuple tupIn = do
    titleAtom <- atomForAttributeName "title" tupIn
    commentsAtom <- atomForAttributeName "comments" tupIn
    commentsRel <- relationForAtom commentsAtom
    comments' <- mapM fromTuple (tuplesList commentsRel)
    pure Blog {
      title = atomToText titleAtom,
      comments = S.fromList comments'}
      
  toAttributes _ =  A.attributesFromList [Attribute "title" TextAtomType,
                                          Attribute "comments" $ RelationAtomType (toAttributes (Proxy :: Proxy Comment))]

main :: IO ()
main = do
  let exampleBlog = Blog { title = "Cat Pics",
                           comments = S.fromList [Comment {authorName = "Steve",
                                                           comment = "great"},
                                                  Comment {authorName = "Bob",
                                                           comment = "enough"}
                                                 ]}
  print (toTuple exampleBlog)
  print (fromTuple (toTuple exampleBlog) :: Either RelationalError Blog)


                           
         




