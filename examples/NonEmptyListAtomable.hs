{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
import ProjectM36.Client
import ProjectM36.Relation.Show.Term
import GHC.Generics
import Data.Text
import Data.Binary
import Control.DeepSeq
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import Data.Proxy
import Data.List.NonEmpty

data AtLeastOne = NonEmpty Int
   deriving (Generic, Show, Eq, Binary, NFData, Atomable)

main :: IO ()
main = do
 --connect to the database
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback []
      eCheck v = do
        x <- v
        case x of 
          Left err -> error (show err)
          Right x' -> pure x'
  conn <- eCheck $ connectProjectM36 connInfo

  --create a database session at the default branch of the fresh database
  sessionId <- eCheck $ createSessionAtHead conn "master"
  
  --create the data type in the database context
  eCheck $ executeDatabaseContextExpr sessionId conn (toAddTypeExpr (Proxy :: Proxy AtLeastOne))

  --create a relation with the new AtLeastOne AtomType
  let nonempty = NakedAtomExpr (toAtom ((1 ::Int):|[]) )
  let list = NakedAtomExpr (toAtom ((1 ::Int):[]) )
  eCheck $ executeDatabaseContextExpr sessionId conn (Assign "people" (MakeRelationFromExprs Nothing [
            TupleExpr (M.fromList [("nonempty", nonempty), ("list", list)])]))

  let restrictionPredicate = AttributeEqualityPredicate "atleastone" nonempty
  peopleRel <- eCheck $ executeRelationalExpr sessionId conn (Restrict restrictionPredicate (RelationVariable "people" ()))

  TIO.putStrLn (showRelation peopleRel)
  
