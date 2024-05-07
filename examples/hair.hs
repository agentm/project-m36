{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, DerivingVia #-}
import ProjectM36.Client
import ProjectM36.Relation.Show.Term
import GHC.Generics
import Data.Text
import Control.DeepSeq
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import Data.Proxy
import Codec.Winery

data Hair = Bald | Brown | Blond | OtherColor Text
   deriving (Generic, Show, Eq, NFData, Atomable)
   deriving Serialise via WineryVariant Hair

main :: IO ()
main = do
 --connect to the database
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback [] basicDatabaseContext
      eCheck v = do
        x <- v
        case x of 
          Left err -> error (show err)
          Right x' -> pure x'
  conn <- eCheck $ connectProjectM36 connInfo

  --create a database session at the default branch of the fresh database
  sessionId <- eCheck $ createSessionAtHead conn "master"
  
  --create the data type in the database context
  eCheck $ executeDatabaseContextExpr sessionId conn (toAddTypeExpr (Proxy :: Proxy Hair))

  --create a relation with the new Hair AtomType
  let blond = NakedAtomExpr (toAtom Blond)
  eCheck $ executeDatabaseContextExpr sessionId conn
    (Assign "people"
     (MakeRelationFromExprs Nothing $ TupleExprs () [
         TupleExpr (M.fromList [("hair", blond), ("name", NakedAtomExpr (TextAtom "Colin"))])]))

  let restrictionPredicate = AttributeEqualityPredicate "hair" blond
  peopleRel <- eCheck $ executeRelationalExpr sessionId conn (Restrict restrictionPredicate (RelationVariable "people" ()))

  TIO.putStrLn (showRelation peopleRel)
  
