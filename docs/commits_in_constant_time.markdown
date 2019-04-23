# Transactional Database Commits in O(1)

A.M.

[Project:M36](https://github.com/agentm/project-m36)

## Synopsis

Common relational database management systems operate in an imperative manner by executing statements immediately. This naive strategy quickly becomes an IO-bound bottleneck. By applying Haskell's laziness-in-evaluation to database updates, new doors for optimizations based on what is queried make themselves apparent. Furthermore, by serializing thunks representing idempotent database updates, the database can commit transactions in constant time with regards to the tuple count since the tuples themselves need not be serialized. This optimization, combined with *fsync*-coalescing, can allow databases to commit to disk faster than tuples updates can be processed. 

Using this deferred execution technique, database updates can be processed in the background or, in the case when the tuples are never requested, never.

These database update thunks and other interesting optimizations are implemented in [Project:M36](https://github.com/agentm/project-m36): an experimental, clean-room, purely Haskell implementation of an RDBMS engine based on the mathematical principles of the relational algebra.

## Rationale

A typical SQL RDBMS such as PostgreSQL executes a list of commands serially against a serializable representation of the database state without concern to whether or not the commands are effectively necessary to service subsequent requests. A database update which affects *n* tuples will necessitate disk writes of *O(n)* plus *O(n\*log(n))* writes for each affected index.

By validating and serializing thunks of validated commands, Project:M36 commits can be independent of the tuple write cost. The thunks, which are flushed to non-volatile storage to complete the transaction commit, allow the DBMS to defer execution in preference to commit time. 

The deferral is, however, not required, and could be left as a option to the user or application.

The final cost of committing in Project:M36 relates to the size of the update expression. If the update expression is, for example, a insertion of a large number of rows, then the commit cost will be *O(t)* where *t* is the number of tuples expressed in the update, but still independent of the cost of the existing tuples in the database.

## Example and Comparison to PostgreSQL

Let us walk through a specific example using SQL. While Project:M36 does not support SQL (and for [good reasons](https://github.com/agentm/project-m36/blob/master/docs/on_null.markdown)), comparing both databases with a common language is useful for illustrative purposes.

Let us consider an existing database with a table/relation named "BigTable" which has 10 billion rows/tuples (an arbitrarily large number) of which one column is named "x" and represents a randomly-assigned integer. Next, assume that the user of the database will execute an update which will update every row/tuple, commit the changes, then query a subset of the resultant updated rows/tuples.


### The Update Expression

Consider the following database update:

```
UPDATE "BigTable" SET x = x + 1;
```
In PostgreSQL, the cost of executing this expression is incurred immediately upon execution. New tuples are laid down to disk (if the dataset is larger than *shared_buffers*), duplicating the entirety of the existing table/relation in storage (due to MVCC) for a cost of *O(n)* where *n* is the number of existing rows/tuples in "BigTable". If *x* is indexed, then the index writes incur an additional cost of *O(n\*log(n))* writes. Furthermore, PostgreSQL will write all changes to its write-ahead log, so the cost is multiplied by some factor.

In Project:M36, the same expression can first be statically validated to adhere to the database constraints. The only constraint affected here is for the column/attribute *x* which requires an integer. Since adding one to any integer retains the integer type, the constraint can be determined to be unviolated without evaluating the expression, thus at a cost of *O(1)*. Project:M36 uses a novel WORM (write-once, read-many) strategy over journaled filesystems to avoid the need for a separate journal.

*For the purpose of this example, assume that integer overflow is not a concern.*

### The Commit

```
COMMIT;
```

In PostgreSQL, the write-ahead log mentioned above containing all tuples changes must now be flushed to non-volatile storage, creating a bottleneck to the slowest form of storage. A necessary checkpoint (which is configurable) could incur further writes as the tuples are flushed to disk as well.

In Project:M36, the update expression must also be flushed to disk, but the expression is a mere few bytes appended to the transaction graph.

### The Subsequent Query

Finally, the user queries a subset of the updated table/relation.

```
SELECT a, b, c, x FROM "BigTable" WHERE x > 10000;
```

In PostgreSQL, considering that the column/attribute has an index, *O(log(n))* reads are needed to identify on which pages in the table's on-disk storage the proper values can be found. Considering that the value *x* was previously described as random, despite the restriction, in the worst case, every page of the on-disk table may need to be read because each page could contain one value matching the restriction. This would necessitate reading *O(n)* tuples previously written to disk. After reading the tuples, the tuples must be filtered by the restriction to service the query.

In Project:M36, the state of the database is determined to be the difference of the previous state *S* (before the *x+1* update above) and *S'* (after the update). The serialized update expression can be deserialized and merged into the rewritten query:

```
SELECT orig.a, orig.b, orig.c, x_orig + 1 AS x FROM (SELECT a, b, c, x AS x_orig FROM "BigTable"@S) AS orig WHERE x > 10000;
```

where the *@* symbol references the state of "BigTable" at transaction state *S* in the past. The rewrite process can occur in constant time regarding the tuple count, but the execution does require reading the previous state *S* of "BigTable", filtering it as expected. The tuple read cost is equivalent to that of the same statement at *S'*: *O(log(n))* utilizing a b-tree index.

### Summary

| | PostgreSQL | Project:M36 |
|-|------------|-------------|
| UPDATE x+1 | *O(nlog(n))* | *O(1)* |
| COMMIT | flush *n* tuples to disk | flush expression to disk |
| SELECT x>10000 | *Ω(log(n))*, *O(n)* worst case | *O(log(n))* |

Even if the user were to query all the tuples back, the actual IO cost in Project:M36 would be lower since the updates are only "executed" on the fly and not necessarily written to disk. However, a clever cacheing strategy could note that state *S* is queried often enough to warrant a cached version. The table's data on disk, however, is not required to answer the query.

## Constraint Oracles

To be able to perform an update and commit in constant or less than *O(n)* time does have some preconditions:

  * constraints must be able to be statically validated in linear or logarithmic time

However, even if constraints lack a proper oracle such as an index or a static means of validation and, thus, cannot be validated in constant time, the user may still benefit from serialization of the database update expression since writing the validated tuples to disk can be deferred beyond commit time and could go directly into an on-disk cache.

## Worst Case Behavior

Every database constraint can be represented as an inclusion dependency whereby the evaluation of a relational expression `sub` must be a subset of the evaluation of relational expression `super` (`sub ⊆  super`) for the constraint to validate. Therefore, the worst case time to validate any constraint, assuming that the constraint has an ideal oracle, is O(n) in the tuple count. However, as mentioned above, there are many constraints that can be validated in constant or less-than-linear time. Furthermore, the validated tuples need not be written to disk once the constraint is found to hold. 

Since there is great variation in constraint validation cost, the RDBMS could pre-analyze and report on associated constraints costs or make use of data independence to guide storage to optimize queries needed to service the constraint checker.

## Caveats

Certain types of updates still take a similar amount of time to commit in Project:M36 in comparison to PostgreSQL. Specifically, any update which relies on data outside the database's purview will necessitate flushing the tuple data since the update expression contains new tuple data not based on existing tuples. For example, the equivalent of:

```
INSERT INTO "BigTable"(a, b, c, x) VALUES (1,5,10,10),(20,20,21,30),.... #many tuples
```
will still necessitate a *O(n)* tuple writes to disk in both databases. Though the asymptotic costs are equivalent, the number of writes are cut in half in Project:M36 since it does not require a write-ahead log.

While PostgreSQL makes use of MVCC (multi-version concurrency control) which marks which tuples belong to a linear progression of transaction states, Project:M36 stores a full history of database state using a transaction graph (a directed acyclic graph similar to the commit-based graph of `git`). The graph allows named branches to be created and merged which also reduce contention around a singular graph head.

## Applying Laziness to SQL RDBMSes

Would it be possible to apply transaction-based laziness to an SQL-based RDBMS? Certainly, with sufficient shoe-horning, it could work, but SQL includes a few malfeatures which would make it difficult, if not impossible:

  * Non-idempotent server-side functions prevent laziness because of side effects. PostgreSQL can mark functions as idempotent, but does not validate or enforce the idempotence, Project:M36, via Safe Haskell, can validate pure functions.
  * The lack of "time travel", the ability to query database state in the past prevents reliance on previous states, prevents direct referencing of previous database states on which the state differences can be built.
  * Tightly-coupled table-to-storage management prevents alternative data models. Indexing and constraint checking need to be statically verifiable and deferrable.

These hurdles are not insurmountable, just perhaps not a good fit for SQL databases with a lot of historical and implementation baggage.

## Benchmarks

Forthcoming.

## Conclusion

Transactional time-to-commit is critical in ensuring that business decisions can make it to non-volatile storage as quickly as possible. By introducing Haskell-like, lazy evaluation to update expressions and serializing the resultant thunk, commits can be serialized in constant time, disregarding the number of tuples modified in the expression.

This serialization strategy turns modern RDBMS architecture on its head since the table/relation-to-file mapping is broken, resulting in true data independence. Any on-disk representation of a relation at a specific transaction state therefore merely becomes a cached and purgeable entity, drastically shrinking on-disk cacheing requirements while expanding the scope in which said strategies can operate. Hopefully, a truly data independent architecture will result in new optimization discoveries.

Project:M36 implements a transaction graph which can represent the entirety of database state, leaving room for any number of alternative representations, even for a single relation or relation across transaction states. A simple example could include storing two different orderings of tuples of the same relation in order to optimally serve different join queries. Common RDBMSes offer only two on-disk representations: the set of fully-evaluated tuples and indexes to those tuples, preventing data independence. Data independence paves the way towards the elimination of implementation-specific, gatekeeper roles such as database administrators.

The promise of the relational algebra has always been the liberation of data from its data structure representation in order to be able to answer arbitrary queries. Modern DBMSes which typically map tables directly to one or more files on disk fail to live up to this promise. Project:M36 sets out to rectify this failure.

In a greater context, applying laziness to an architecture to make use of more interesting optimizations is hardly new. The OpenGL API views its "immediate" mode as effectively deprecated. Filesystems take advantage of laziness using copy-on-write semantics. High-level programming languages can make greater, more interesting inferences about how a computation must be execute. A computer can always consider many more possible solution paths than a human can.

The adherence to mathematical priniciples in Project:M36 results in features and optimization opportunities which have not yet been discovered. In contrast to RDBMSes designed around specific optimizations (such as indexes, specific disk layouts, or key-value lookups), Project:M36 sets out to demonstrate practically that future optimizations can only be discovered when operating within a mathematically-coherent architecture. 