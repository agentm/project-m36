# Project:M36 Client Library

The Project:M36 client library is the preferred method for interacting with Project:M36 databases. It supports all features of the DBMS while other interfaces may not.

The client supports both in-process and remote Project:M36 access.

## Setup

1. Import the library's symbols
```haskell
import ProjectM36.Client
```
2. Create a Connection with a ConnectionInfo in the Either monad
```haskell
conn <- connectProjectM36 (InProcessConnectionInfo NoPersistence)
```

## Executing Expressions

1. Execute relational expression queries
```haskell
result <- executeRelationalExpr conn (Union (RelationVariable "x") (RelationVariable "y"))
```
2. Execute database context expressions which modify the current, mutable database context
```haskell
maybeErr <- executeDatabaseContextExpr conn (Define "person" (attributesFromList [Attribute "name" StringAtomType, Attribute "age" IntAtomType, Attribute "id" StringAtomType]))
```
3. Execute a transaction graph expression
```haskell
maybeErr <- executeGraphExpr conn (JumpToHead "branch2")
```

## Cleanup

1. Close the connection
```haskell
close conn
