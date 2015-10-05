## Transaction Operators

Relational operators generate new relations and database context operators alter the relations referenced by relation variables. Finally, transaction operators alter the state of the database's transaction graph. This includes adding new transactions, branching, and changing the current database context to point at an arbitrary transaction in the past (time-travel).

#### The Database Context

Every database operation occurs within a database context. The database context is the state of the relation variables, constraints, and user-definable functions. While past transactions committed to the transaction graph are immutable, new transactions can be created from database contexts based on any transaction in the transaction graph.

When a new database is created, a new transaction graph is created for it with one base transaction. The base transaction is special because it is the only transaction in the graph without a parent. At this point, the editable database context necessarily references the singular transaction. It can be considered a mutable copy of the database context stored by the first transaction.

As the user interactively alters the database context such as by adding a new relation variable, the database context is no longer equal to that of the first transactions. This is called "database context divergence" because many of the same components exist in both the updated context and the context of the first transaction.

[Initial database starting state](http://g.gravizo.com/g?
digraph G {
 base[label="base transaction",shape=rectangle];
 new[label="mutable database context"];
 new -> base;
})

Once the user decides that the changes in the updated database context are worth adding to the transaction graph, the user issues a "commit" request. The commit operation converts the current database context into a new transaction which is then added to the graph with the parent transaction set to the same parent transaction as the database context. The user's mutable database context is shifted to be a copy of the context of the newly-committed transaction.

[Freshly committed transaction](http://g.gravizo.com/g?
digraph G {
 base[label="base transaction",shape=rectangle];
 discon[label="mutable database context"];
 fresh[label="newly committed transaction",shape=rectangle];
 fresh -> base;
 discon -> fresh;
})

It can be handy to attempt to modify the database without altering the data that others may wish to continue to use and modify. If a set of transactions may diverge from another use-case for the data, the user may choose to "branch" the transaction graph. Branching allows the user to assign an arbitrary name to a series of transactions and then potentially merge the set of transactions back to the originating branch. Project:M36 creates a "master" branch when the transaction graph is first created.

Note that this transaction graph approach is different from that of most SQL DBMSs which have a strictly linear transaction graph where all the contention is at the head of the graph and neither time-travel nor branching are supported.

#### Commit

To convert a mutable database context into a transaction, it must be "committed" to the transaction graph.

```
TutorialD (master): :commit
```

Once committed, the transaction's database context becomes immutable. Internally, Project:M36 uniquely identifies each transaction with a UUID.

#### Rollback

Sometimes, the user determines that the changes in the mutable database context can be discarded and the context reset to be a copy of its parent's transaction. This is called a "rollback" because it  rolls back to a previous known state.