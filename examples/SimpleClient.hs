import ProjectM36.Client
import ProjectM36.TupleSet
import ProjectM36.Atom
import ProjectM36.DataTypes.Primitive
import ProjectM36.Base
import ProjectM36.Relation.Show.Term

main :: IO ()
main = do
  -- 1. create a ConnectionInfo
  let connInfo = RemoteProcessConnectionInfo "mytestdb" (createNodeId "127.0.0.1" defaultServerPort) emptyNotificationCallback
  -- 2. conncted to the remote database
  eConn <- connectProjectM36 connInfo
  case eConn of
    Left err -> putStrLn (show err)
    Right conn -> do
      --3. create a session on the "master" branch
      eSessionId <- createSessionAtHead "master" conn
      case eSessionId of
        Left err -> putStrLn (show err)
        Right sessionId -> do
          --4. define a new relation variable with a DatabaseContext expression
          let attrList = [Attribute "name" textAtomType,
                          Attribute "age" intAtomType]
              attrs = attributesFromList attrList
          mErr1 <- executeDatabaseContextExpr sessionId conn (Define "person" (map NakedAttributeExpr attrList))
          putStrLn (show mErr1)
          --5. add a tuple to the relation referenced by the relation variable
          let (Right tupSet) = mkTupleSetFromList attrs [[textAtom "Bob", intAtom 45]]
          mErr2 <- executeDatabaseContextExpr sessionId conn (Insert "person" (MakeStaticRelation attrs tupSet))
          putStrLn (show mErr2)
      
          --6. execute a relational algebra query
          let restrictionPredicate = AttributeEqualityPredicate "name" (NakedAtomExpr (textAtom "Steve"))
          eRel <- executeRelationalExpr sessionId conn (Restrict restrictionPredicate (RelationVariable "person"))
          case eRel of
            Left err -> putStrLn (show err)
            Right rel -> putStrLn (show $ showRelation rel)
      
          --7. close the connection
          close conn
