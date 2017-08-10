## Transaction Graph Operators

Relational operators generate new relations and database context operators alter the relations referenced by relation variables. Finally, transaction operators alter the state of the database's transaction graph. This includes adding new transactions, branching, and changing the current database context to point at an arbitrary transaction in the past (time-travel).

#### The Database Context

Every database operation occurs within a database context. The database context is the state of the relation variables, constraints, and user-definable functions. While past transactions committed to the transaction graph are immutable, new transactions can be created from database contexts based on any transaction in the transaction graph.

When a new database is created, a new transaction graph is created for it with one base transaction. The base transaction is special because it is the only transaction in the graph without a parent. At this point, the editable database context necessarily references the singular transaction. It can be considered a mutable copy of the database context stored by the first transaction.

As the user interactively alters the database context such as by adding a new relation variable, the database context is no longer equal to that of the first transactions. This is called "database context divergence" because many of the same components exist in both the updated context and the context of the first transaction.

![Initial database starting state](http://g.gravizo.com/g?
digraph G {
 base[label="base transaction",shape=rectangle];
 new[label="mutable database context"];
 new -> base;
})

Once the user decides that the changes in the updated database context are worth adding to the transaction graph, the user issues a "commit" request. The commit operation converts the current database context into a new transaction which is then added to the graph with the parent transaction set to the same parent transaction as the database context. The user's mutable database context is shifted to be a copy of the context of the newly-committed transaction.

![Freshly committed transaction](http://g.gravizo.com/g?
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

To convert a mutable database context into a transaction, it must be "committed" to the transaction graph. The equivalent in SQL is COMMIT.

```
TutorialD (master/main): :commit
```

Once committed, the transaction's database context becomes immutable. Internally, Project:M36 uniquely identifies each transaction with a UUID.

#### Rollback

Sometimes, the user determines that the changes in the mutable database context can be discarded and the context reset to be a copy of its parent's transaction. This is called a "rollback" because it  rolls back to a previous known state. The equivalent in SQL is ROLLBACK.

```
TutorialD (master/main): :rollback
```

#### Show Transaction Graph

In order to navigate the transaction graph, one must specify transaction UUIDs. Viewing the entirety of the transaction graph as a relation is a single command.

```
TutorialD (master/main): :showgraph
┌─────────────┬──────────┬──────────────────────────────────────┬────────────────────────────┬──────────────────────────────┐
│current::Bool│head::Text│id::Text                              │parents::relation {id::Text}│stamp::DateTime               │
├─────────────┼──────────┼──────────────────────────────────────┼────────────────────────────┼──────────────────────────────┤

<big result elided>
```
The "current" attribute is a boolean value. If true, the transaction is the parent of the current database context. Commits will be children of this transaction.

The "head" attribute is a boolean value indicating whether or not the transaction is a "head" node of its graph. Head nodes are nodes which have no children.

The "id" attribute is the UUID needed in order to navigate the graph.

The "parents" attribute is a subrelation holding all parent transaction UUIDs for the given transaction.

The "stamp" attribute marks the time when the transaction was opened.

#### Branch

A new branch can be create from any point in the transaction. A branch associates a human-readable name with a set of transactions diverging from a parent transaction. After a branch, there are two heads to which one could commit.

```
TutorialD (master/main): :branch testing_data_2005
TutorialD (testing_data_2005):
```

Note how the prompt refers to the new branch. This indicates that further commits from the mutable database context will now be added to the new branch.

#### Jump to Head

To switch the current mutable database context to refer to a different branch, jump to a different head. Committing to a head ensure that the branch growth continues linearly.

Note that jumping to another head implies losing any changes in the current database context.

```
TutorialD (testing_data_2005): :jumphead master
TutorialD (master/main):
```

Further commits will be added to the target branch.

#### Jump to Transaction

Instead of jumping to the head of a branch for the purpose of adding more commits, it can be also useful to jump to a specific transaction in the past for two possible reasons:

* to examine the state of the database in the past; for example, for auditing purposes
* to branch the database from a point in the past

If one jumps to a non-head transaction, committing new transactions are no longer possible because the past transactions cannot be overwritten.

```
TutorialD (master/main): :jump 11b601c9-f46b-42b0-a2ce-cf18ced31b7f
TutorialD (11b601c9-f46b-42b0-a2ce-cf18ced31b7f):
```

Note that the ```tutd``` prompt now indicates that the mutable database context now refers to a specific transaction by UUID.

#### Walk Back To Time

Each transaction is committed along with a creation timestamp. From any point in the graph, one can "walk back" until the commit timestamp is past a given timestamp threshold.

```
TutorialD (master/main): x:=true
TutorialD (master/main): :commit
TutorialD (master/main): :showgraph
┌─────────────┬──────────┬──────────────────────────────────────┬────────────────────────────────────────┬──────────────────────────────┐
│current::Bool│head::Text│id::Text                              │parents::relation {id::Text}            │stamp::DateTime               │
├─────────────┼──────────┼──────────────────────────────────────┼────────────────────────────────────────┼──────────────────────────────┤
│True         │"master"  │"b4c3bd0a-c594-456f-8e1b-8e296c939df4"│┌──────────────────────────────────────┐│2017-08-10 16:45:34.635582 UTC│
│             │          │                                      ││id::Text                              ││                              │
│             │          │                                      │├──────────────────────────────────────┤│                              │
│             │          │                                      ││"5e441752-f78d-4a0d-b7f6-b2716ab82a4d"││                              │
│             │          │                                      │└──────────────────────────────────────┘│                              │
│False        │""        │"5e441752-f78d-4a0d-b7f6-b2716ab82a4d"│┌────────┐                              │2017-08-10 16:43:59.160044 UTC│
│             │          │                                      ││id::Text│                              │                              │
│             │          │                                      │└────────┘                              │                              │
└─────────────┴──────────┴──────────────────────────────────────┴────────────────────────────────────────┴──────────────────────────────┘

TutorialD (master/main): :walkbacktotime "2017-08-10 16:44:00"
TutorialD (<unknown>/main): :showexpr x
ERR: RelVarNotDefinedError "x"
```

In the above example, we start with a fresh database and commit relvar ```x```. Then, using the displayed graph, we walk back to a time before the stamp provided from the graph display and discover that, indeed, ```x``` was not defined in that point in time.

Note that all timestamps are in UTC time.

If you choose to walk back in time before the database existed, you will see a ```RootTransactionTraversalError```.

Note that some transactions, specifically merge transactions, can have multiple parents. In such walk back scenarios, an arbitrary parent is chosen to continue the search. For more precise control, use the other search/jump strategies to set your current transaction behind the merge transaction and then walk back.
