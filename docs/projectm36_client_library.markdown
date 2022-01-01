# Project:M36 Client Library

The Project:M36 client library is the preferred method for interacting with Project:M36 databases. It supports all features of the DBMS while other interfaces may not.

**Note:** A less featureful but more convenient client API also is available: see [documentation](simple_api.markdown).

The client supports both in-process and remote Project:M36 access.

## Build Configuration

If using cabal to build, run `cabal install --lib` to make Project:M36 a library accessible from other cabal projects.

If using stack to build, copy the `extra-deps` section from the relevant stack.ghc.<ver>.yaml configuration into your own project and run `stack build` to install the Project:M36 library.

## Setup

1. Import the library's symbols
```haskell
import ProjectM36.Client
```
2. Create a Connection with a `ConnectionInfo` in the Either monad
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

## Dealing with Concurrency

As in all databases, transactions contend concurrently for access to the the latest database state. Unlike most DBMSs, Project:M36 features multiple heads to reduce contention. Still, at a high transaction rate, Project:M36 clients would still be likely to receive lots of ```TransactionIsNotAHeadError```s because the server's head has received additional commits by the time a client wants to commit its own transaction. To reduce the incidence of this error, the server offers an ```autoMergeToHead``` function which:

1. creates a temporary branch
1. commits the current disconnected transaction to the temporary branch
1. attempts a merge back to the original branch

The operations above occur atomically on the server. The purpose of this "automerge" is to reduce reliance on repeated client attempts to commit when head contention is high. The trade-off is that the client must handle any merge errors or use a different merge strategy.

This feature is similar to git's ```rebase``` functionality except that the merge occurs server side and is fully validated and committed, if possible.

## Cleanup

1. Close the connection
```haskell
close conn
