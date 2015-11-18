{-# LANGUAGE OverloadedStrings #-}
import ProjectM36.Client
import ProjectM36.TupleSet
import ProjectM36.Atom
import ProjectM36.Base
import ProjectM36.Relation.Show.Term

main :: IO ()
main = do
  -- 1. create a ConnectionInfo
  let connInfo = RemoteProcessConnectionInfo "mytestdb" (createNodeId "127.0.0.1" defaultServerPort)
  -- 2. conncted to the remote database
  eConn <- connectProjectM36 connInfo
  case eConn of
    Left err -> putStrLn (show err)
    Right conn -> do
      --3. define a new relation variable with a DatabaseContext expression
      let attrs = attributesFromList [Attribute "name" stringAtomType, Attribute "age" intAtomType]
      mErr1 <- executeDatabaseContextExpr conn (Define "person" attrs)
      putStrLn (show mErr1)
      --4. add a tuple to the relation referenced by the relation variable
      let (Right tupSet) = mkTupleSetFromList attrs [[stringAtom "Bob", intAtom 45]]
      mErr2 <- executeDatabaseContextExpr conn (Insert "person" (MakeStaticRelation attrs tupSet))
      putStrLn (show mErr2)
      
      --5. execute a relational algebra query
      let restrictionPredicate = AttributeEqualityPredicate "name" (NakedAtomExpr (stringAtom "Steve"))
      eRel <- executeRelationalExpr conn (Restrict restrictionPredicate (RelationVariable "person"))
      case eRel of
        Left err -> putStrLn (show err)
        Right rel -> putStrLn (show $ showRelation rel)
      
      --6. close the connection
      close conn