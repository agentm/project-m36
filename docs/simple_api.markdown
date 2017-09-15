# The Project:M36 Simple Client Interface

## Introduction

For common use cases, the `ProjectM36.Client.Simple` API can assist in transaction management. The API is designed to be less overwhelming than the full-featured `ProjectM36.Client` (see [documentation](projectm36_client_library.markdown)).

As such, not all features are available through the simple API, but, because the simple API is a thin layer over the complete API, the user may dip into the complete API at any time.

## Usage

  1. Create a `DbConn` to connect to the database:

     ```haskell
     import ProjectM36.Client.Simple
     ...
       let connInfo = InProcessConnectionInfo (CrashSafePersistence "my.db") emptyNotificationCallback []
       eDbconn <- simpleConnectProjectM36 connInfo
     ```

     The result is `Either` a `DbError` or a connection linked to the `"master"` branch of the database.

  1. Run the `Db` monad using `withTransaction`:

     ```haskell
     withTransaction dbconn $ do
       execute $ Assign "x" (ExistingRelation relationTrue)
       query $ RelationVariable "x" ()
     ```

     If there is an error in the above update or query, then an exception is thrown behind the scenes which cancels the transactions,  rolls back any changes, and the error is returned  by `withTransaction`.

     `query` is used with `RelationalExpr` queries which read the current database state.

     `execute` is used with `DatabaseContextExpr` values which write the current database state.

     To execute a query without cancelling the transaction on an error, use the `queryOrErr` and `executeOrErr` variants.

  1. Close the connection:

     ```haskell
     close dbconn
     ```
