# Project:M36: Towards an Architecture for Data Independence

## Introduction

The Project:M36 relational algebra architecture sets itself apart from typical DBMSes by ensuring strict transactional isolation, a branchable/mergeable (rather than linear) transaction graph, and storage of relational expressions (rather than storage of tuples). Let's look at how these architectural features work together to make Project:M36 the first relational algebra engine to implement true data independence.

## What is Data Independence?

C.J. Date writes: 

`Certainly most SQL products have failed to live up to the [relational] model's full potential...in those products, what the user sees and what is physically stored are essentially the same thing... In particular, it accounts for the argument, so often heard, to the effect that we have to "denormalize for performance."`

`In principle, logical design has absolutely nothing to do with performance at all.`

(Date, C.J. "Database In Depth: Relational Theory for Practitioners". O'Reilly Media. 2005. pages 156-157.)

Data independence is therefore defined as the separation of the database user's experience from the database's means of in-memory or on-disk representation. As Date points out, virtually all database products implement a one-to-one mapping of relations to files in storage. 

One advantage of high-level abstractions in programming languages is to leverage advanced algorithms which can choose optimization paths on behalf of the user which a human may never be able to discover. While the promise of ever higher-level abstractions is realized by progamming languages such as Haskell, DBMS implementors have been resistant to data independence. This is due to several legacy factors such as companies pushing one-trick-pony database technologies and middlemen such as DBAs required to play gatekeeper against "bad" schema changes. The result is customers forced to run multiple disparate databases such as one for realtime transaction processing and another for analytics and another for audit logging. This recalcitrance has also led to the rise of databases such as "document" JSON databases with weak mathematical underpinnings in the name of performance as users become frustrated when hitting performance walls and dealing with arbitrary database limitations.

Implementations which closely tie the user interface to backend representation close the door on interesting alternative representations and optimizations. For example, SQL databases typically store a single relation as a list of tuples on disk with supplementary indexes stored in separate files. There is no reason that the same relation couldn't be on disk or in memory in any number of formats. Such a table might be useful in various formats ideal for servicing different types of queries. Data independence grants the DBMS the permission to create any number of representations- including none at all- as long as the correct answer is provided to the user. "Column-store" databases may try to slice the tuples in a different axis, but those databases simply shift the performance problems elsewhere. Only implementation details prevent a database from offering both column-oriented *and* tuple-oriented representation while allowing the database to choose.

That is why it is critical for the user's interface to the database *not* to include features such as "query hints" which would force a specific implementation in the backend. The user's interface must be declarative (describing the state change or query rather than *how* to execute it) and sufficiently high level. Any implementation detail leakage to the user's interface is a lost potential algorithmic optimization. While the topic of providing high-level interfaces is not specific to database design, we will continue to explore the ramifications of data independence in databases.


## Transactional Isolation

While other databases offer various transaction isolation levels with differing levels of implementation-specific surprises, Project:M36 solely suppports the maximum transaction isolation level. Instead of relying on details of [MVCC](https://www.postgresql.org/docs/current/mvcc-intro.html) which marks each tuple in a relation with a monotonically-increasing number identifying from which transaction the tuple was added or deleted, Project:M36 includes no singular backing store for tuples, instead storing a stream of state change requests. Because the transaction graph supports branching and merging (similar to version control such as git), transactions can be committed without contention at a singular head of the graph. Committing to disk can be implemented as writing a file to represent the transaction's state and 

## The Transaction Graph

These state changes are, in fact, the idempotent relational update expressions that the user has submitted. For example, given a transaction which includes a single relation `s`:

```
┌──────────┬────────┬───────────┬───────────┐
│city::Text│s#::Text│sname::Text│status::Int│
├──────────┼────────┼───────────┼───────────┤
│"Paris"   │"S2"    │"Jones"    │10         │
└──────────┴────────┴───────────┴───────────┘
```

if the user commits a transaction with an insert such as:

```
insert x relation{tuple{city "Boston", s# "S3", sname "Smith", status 400}}
```

then a typical database will find the file representing the relation, find some free space in it and save the new tuple along with the MVCC transaction identifier. Subsequent transactions will find the tuple and compare it to their own transaction id in order to determine its "visibility". Project:M36, on the other hand, after validating that all relational constraints (such as primary and foreign keys) still hold, will simply serialize and append the expression to the transaction graph log. By default, there is no file representing the relation.

For those familiar with Haskell, note how serializing the update expression is similar to being able to serialize a thunk or deferred execution.

The transaction graph also allows users to travel back to previously-committed transactions. This may be useful for audit logging or alternative schemas.

## Storage Representations

Given the ever-growing transaction graph based on state changes, let's examine how Project:M36 services subsequent queries.

## Benefits

The primary goal of a database should be to ensure data integrity. Project:M36 takes no shortcuts in achieving this goal, but choosing a backend architecture which minimizes the time which it takes to get the transaction to non-volatile storage (disk) by storing update expressions rather than attempting to shuffle tuples on disk. This architecture open the door to many optimization techniques, many of which are new to databases.

### Multiple On-Disk Representations

The promise of data independence allows database backends to store relations as they see fit. That can include any number of on-disk representations including none at all. For example, for a table accessed by multiple projections, the database may choose to materialize the relation in those projections. The representations are therefore created in service to the queries submitted or anticipated instead of being based on past update expressions. Because on-disk storage is obviously limited, the database must decide what is valuable to keep around for future queries. This could open a new area of research into the appropriate machine learning algorithms, but, fundamentally, this set of materialized query responses is effectively a cache which can always be refreshed. 

The alternative representations need not be a direct answer to queries- for example, a materialized relation from a transaction could be used to service a query from a later transaction if the updates since that transaction are applied.

The database could also anticipate, for example, scheduled queries and actually materialize relations before the query is executed. On-disk cache could represent "denormalized" tuple sets while the user-facing interface shows a properly normalized schema, thereby supporting data independence.

More research in determining just what should be in the cache is needed.

### Transactional Commits in O(1) Time

Because the Project:M36 backend need not save tuples into a specific file representing a relation, the backend can drastically reduce the amount of time needed to commit a transaction to disk. Specifically, following constraint validation (which can often be done in constant or logarithmic time relative to the tuple count), the fastest means of committing the transaction is to save the update expression itself. Since the previous state has already been presumably committed, any idempotent operation can be saved for deferred execution. This means that transactions can be committed to disk relative to the size of the update expression rather than the number of the tuples affected- committing can occur at a rate relative to that of update expression size (typically a few kilobytes) rather than relative to the number of tuples that the update expression happened to alter.

This technique gets transactions to disk faster, thus drastically increasing the rate transactions can be committed overall. Note that the execution of the update expression may be deferred by this process, so the cost to evaluate it may occur later, but this processing may be performed in the background after the transaction is committed. When the data is never requested, for example, for a defunct transaction graph branch, the actual execution could be deferred indefinitely, incurring zero cost to calculate the consquence of the update.

### Less Disk Usage

Because Project:M36 stores a stream of update expressions rather than tuples themselves, the minimum storage requirements for a given database may be reduced. For example, if a relation with an attribute `x` of type `Integer` has 1,000,000 tuples, a subsequent update to increment all 1,000,000 values by one may be cheaper to evaluate on-demand rather than immediately generating 1,000,000 new tuples and saving them to disk. If the new tuples are requested, the database can determine that the tuples are not materialized, calculate them, and return them, all without ever having the tuples on disk. Furthermore, if only a subset of the tuples are ever requested, for example, due to a query restriction, then some of the tuples may never need to be calculated or materialized.

### Audit Logging

Typical databases implement various strategies for audit logging, including:

* exposing MVCC-like values such as tuple "created at" and "created by" to the user and forcing all tables to be insert-only
* shuttling audit logs to a secondary database

Project:M36, through its inherent ability to travel the append-only transaction graph, enables audit-logging by allowing users to checkout previous transaction states. Committed transaction states are immutable, so no relation pollution or secondary database is necessary.

### Selective Caching

Typical databases materialize entire tuple sets representing each named relation (relation variable), but there is nothing special about relations that happen to be named. Data independence allows us to cache answers to common or even anticipated queries- this allows the schema to be normalized and unmaterialized while the *denormalized* query results are materialized cached. This flexibility saves disk space and allows the cache to be much more dynamic than in a typical DBMS.

### Never-Materialized Tuples Replace Partitions

Queries on very large relations often very often restricted (filtered by tuple) to generate subsets which are often partitioned, for example, by an event's date. Instead of requiring DBMS users to manually partition and slice their data in one, fixed, particular way, Project:M36's cacheing system will only cache those tuples that serve the interests of the queries. With something like an LRU cache, a data independent architecture will naturally cache "partitions" of the full relation without manual intervention.

## Conclusion

Project:M36's backend architecture enables new optimizations and features that are sorely lacking in existing DBMSes. By leveraging its novel transaction graph and deferred evaluation, Project:M36 handles more DBMS use-cases.

Project:M36's novel architecture opens the door for further database and algorithmic research.