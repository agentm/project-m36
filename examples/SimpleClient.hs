import ProjectM36.Client
import ProjectM36.TupleSet
import ProjectM36.Relation.Show.Term
import Data.Text.IO as TIO

main :: IO ()
main = do
  -- 1. create a ConnectionInfo
  let connInfo = RemoteConnectionInfo "mytestdb" "127.0.0.1" (show defaultServerPort) emptyNotificationCallback
  -- 2. conncted to the remote database
  eConn <- connectProjectM36 connInfo
  case eConn of
    Left err -> print err
    Right conn -> do
      --3. create a session on the "master" branch
      eSessionId <- createSessionAtHead conn "master"
      case eSessionId of
        Left err -> print err
        Right sessionId -> do
          --4. define a new relation variable with a DatabaseContext expression
          let attrList = [Attribute "name" TextAtomType,
                          Attribute "age" IntAtomType]
              attrs = attributesFromList attrList
          mErr1 <- executeDatabaseContextExpr sessionId conn (Define "person" (map NakedAttributeExpr attrList))
          print mErr1
          --5. add a tuple to the relation referenced by the relation variable
          let (Right tupSet) = mkTupleSetFromList attrs [[TextAtom "Bob", IntAtom 45]]
          mErr2 <- executeDatabaseContextExpr sessionId conn (Insert "person" (MakeStaticRelation attrs tupSet))
          print mErr2
      
          --6. execute a relational algebra query
          let restrictionPredicate = AttributeEqualityPredicate "name" (NakedAtomExpr (TextAtom "Steve"))
          eRel <- executeRelationalExpr sessionId conn (Restrict restrictionPredicate (RelationVariable "person" ()))
          case eRel of
            Left err -> print err
            Right rel -> TIO.putStrLn (showRelation rel)
      
          --7. close the connection
          close conn
