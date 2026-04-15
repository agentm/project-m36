# Hotspot Optimizer

## Introduction

[Amdahl's Law](https://en.wikipedia.org/wiki/Amdahl%27s_law) proves that the optimizations of greatest value are those applied to the parts of the software that take the greatest time. In DBMS terms, that means the "best bang for the buck" optimizations can be found when working on the slowest or most resource-intensive queries.

Unfortunately, most DBMS products make the optimization process either manual and error-prone (such as by adding indexes) or opaque and semi-random (such as with query optimizers). For example, novice database administrators are likely to add indexes in a Monte-Carlo fashion without understanding why a query is slow. Many times, this shotgun approach even appears to work, but the DBA may not understand the consequences of adding indexes (such as slowing down INSERT expressions). For DBAs, the confusion comes from needing to learn manually-applied, product-specific, quirky optimizations. DBAs learn that all databases *must* have similar quirks.

## What is database optimization now?

The quirks of optimizing databases is not a problem inherent to the underlying mathematics of the relational algebra, rather a problem with the products themselves: DBAs are expected to tweak meaningless heuristics, experiment with the values of arbitrary configuration knobs, deal with legacy options, and adjust configuration that applies to the database as-a-whole instead of to individual query execution. Specific examples of these include (but are definitely not exclusive to):

### PostgreSQL `work_mem` 

"work_mem" is a session-configurable value which defines how much memory should be used for in-memory sorting or hashing before spilling over to slower disk. While one could adjust this value on a per-query basis, knowing what the value should be at any time is impossible. The database knows how much memory it is using and can estimate how much will be needed to execute the query plan, but this information is not used to determine how must to allocate the resource of memory. In addition, `work_mem` is only a upper-bound on a per-node basis- if the execution plan includes multiple hash nodes, then each one multiples the maximum possible `work_mem`.

`work_mem` has been part of PostgreSQL for twenty-odd years and unlikely to disappear. Developers use it as a knob to prevent their specific query from spilling to disk, but there is no way to peg it to a specific query plan, so if a database upgrade or change in query plan changes the memory profile, the `work_mem` is instantly wrong- either too high or too low. If other queries start using more memory after a high `work_mem` is set, then the database might start swapping. There is no way to use `work_mem` entirely safely, but it is used pervasively, often for "special" queries that require more resources than the typical queries.

### PostgreSQL index `fillfactor`

When creating some types of indexes- including the default b+tree- the user can specify a "fill factor". Set to the default of 90%, it means that 10% of the index space allocated will be reserved for future indexed values. This is a optimization designed to allow legroom for future tuple inserts, for example. For example, if the table will never change, then the user can set the value to 100 and leave no leftover room for additional tuples. Of course, if there are additional tuples, PostgreSQL will add more space to the index for those values. Setting the fillfactor is rarely used. What's special about 90%? Nothing- it's a reasonable guess given zero context.

### Microsoft SQL Server `legacy_cardinality_estimation`

This setting has been around since an query planner overhaul was implemented for MSSQL Server 2012. Some users find that the legacy estimator- which estimates the distribution of values within a column of a table- may return results which feed into the query planner for "better" or more predictable plans. In reality, this misfeature allows databases created with older SQL Server products to maintain the same query plans. If the "new" query planner doesn't do as good a job as the old one, why was it implemented to replace the old query planner? Planners are based on heuristics, but why would I want a planner that has *worse* heuristics?! In the worst case, why can't the planner gather plans using both estimators, run boths plans, then pick the better one next time? Why does making the planner make good choices from between *two* choices require manual intervention?

### Execution Plan Analysis

Execution plan analysis is a manual optimization strategy common to most DBMS products. The DBA or database user is required to generate a database execution plan, i.e. examine an otherwise opaque graph of job nodes describing how the database is actually going to generate an answer to a query. This may include sequential scans, index scans, and join strategies. The human is then expected to run experiments- add an index, try again, adjust table statistics, try again- to determine which course of manual intervention reduces the cost of the query.

But if a human can try these experiments, why can't the DBMS do it, too? What value is the human adding? A DBMS implementor might answer that the human needs to be in the loop to prevent resource (CPU, disk, network) overallocation, but couldn't the DBMS track these things or run the experiments as a lower-priority than application queries?

## How did we get here?

Faced with hundreds of such heuristic knobs, developers are most likely to live with whatever defaults the database provides.

DBMS products which have been around for decades tend to accumulate more and more knobs which are effectively instant technical debt. There's no way for a human to manually track and adjust them all, so developers use them as emergency release valves, i.e. when query performance is suddenly an emergency, developers can start twisting knobs to see if anything makes a difference, consequences be damned. Then, because developers become reliant on these backdoor   
"optimization" knobs, the knobs become calcified into the product, preventing the product from removing them or automating them away, resulting in a weird feedback loop whereby DBMS users' and DBMS product developers' expectations converge on an incompletely implemented feature based on implementation details which can never change.

That's really the crux of the issue- a DBMS product developer didn't know what heuristic value would be sensible in a real world database, so he exposed the value to the user "just in case". This is how implementation details are exposed to the user through a "[leaky abstraction](https://en.wikipedia.org/wiki/Leaky_abstraction)". Leaky abstractions also lead to permanent [accidental complexity](https://en.wikipedia.org/wiki/No_Silver_Bullet).

## What could database optimization be?

Consider that indexing has nothing to do with modeling data and business logic- nor does rewriting queries to take advantage of indexes. Being forced to create and maintain indexes is also a leaky abstraction! 

Shouldn't the DBMS be able to rearrange data at-will to serve the queries without human intervention? 

Could the age of LLMs finally eliminate the need for DBAs? 

The sad truth is that we never needed complex AI to implement databases which make good (but not perfect) decisions- what we needed was to eliminate leaky abstractions. Once a database opens the door to force users to, for example, add an index, optimizations need to take that index into consideration- the DBMS cannot know, however, if the index is still relevant- because it is a user-maintained database object, the database *must* retain it and decide how and when use it.

Now re-evaluate what an index actually is (simplified): an index contains values and likely some links back to the "heap" where the remainder of the associated row can be found. With this information, we can gain insight on internal database representations of data. Let's examine an illustrative example.

`CREATE INDEX product_name_index ON product (name);`

In order to create the index, the DBMS must project on the `name` column of table `product` using a sequential scan of the projection of all the values in that column and where they appear in the table. Then, the index creation process stores the values in a specific data structure- a tree. The data from the `name` column is thereby duplicated in the database, but with a singular goal: to make searches for `name` run in less-than-linear time. Where a sequential scan to look for string "oatmeal" takes O(number of tuples in `product`) computational time, scanning a tree for the same value takes O(log(number of tuples in `product`)) making it a less-than-linear search cost. 

Following this quick review of index creation, you should have a better sense that an index is simply an alternative representation of data in the database, organized for specific query access/optimization. SQL users are taught that the table "storage" is the primary source since the table is effectively the default if no other data sources (such as an index) could be used. An index is effectively secondary storage of the same data. The index can be deleted without causing data loss while deleting the table- the "primary" source of the data- deletes not only the data, but the index, too! In SQL, the index can serve no purpose without the "source" table. The index and its contents cannot exist alone.

The SQL relationship between the table and index is completely arbitrary. In a relational algebra math engine, as long as the engine returns the correct answer to a query, how the data is arranged internally is irrelevant to the user. In SQL, this is unfortunately not the case since the user is responsible for adding a potential optimization opportunity (the index) to the database. C.J. Date, a well-known database theorist, calls the principle of decoupling storage considerations from the query semantics "data independence". In any case, why can't the DBMS figure out when it needs alternate representations of data to serve queries better? If all my data fits into an index, why shouldn't the b-tree structure be the "primary"? Why should I, a simple human, care about the representation? Why should I be forced to find database optimization opportunities? 

_Fundamentally, why can't the database organize itself to the serve the queries?_

We know that, because of the combinatorial explosion of potential execution plans, the complexity of finding the _ideal_ database structure to serve queries is NP-hard or unbounded (depending on the definition of "ideal"), but we don't need the ideal structure- we need one that's good enough to serve the queries within a specified time window.

*The goal of a DBMS is to serve the queries within reasonable time constraints.* To achieve this goal, the DBMS must reorganize itself however it can. This can include:

* creating N alternate representations of data where N >= 1 with the goal of supporting disparate queries
* making the best use of the computer host's resources: RAM, long-term storage, CPU time *without* human intervention or heuristic adjustment
* dealing with query priority- e.g. [OLAP](https://en.wikipedia.org/wiki/Online_analytical_processing) queries can be de-prioritized when OLTP queries are running
* identifying "hot" parts of a dataset to serve faster- this could be identified by slicing tables by columns (projection) or rows (restriction)
* using predictive analysis to optimize away future hotspots *before* they become hotspots
* garbage collect data structures that were used to serve past hotspots

Ultimately, the goal of the DBMS should be to adapt to the business.

_The database must adapt to the business, not vice versa._

If database users are forced to apply their own optimizations, then the DBMS is failing at the goal of adaptation.

Historically, SQL users are inclined to treat a table as a unit of optimization. But the "table" is irrelevant. Consider, for example, that any large table will have a subset of data that is more commonly queried than the rest, for example, because the data is more recent or related to a more common application use case. SQL users faced with this conundrum are pushed towards to partitions, but partitions are *also* a leaky abstraction. Typical SQL products don't even allow a table to be partitioned in multiple ways to serve different queries- the user needs to maintain a separate table with the data copied for that. 

Because SQL users can only respond to perceived database slowness, human optimization is typically reactionary. Even when an SQL user anticipates a certain use-case and, for example, adds an index, this is most likely a pre-optimization since the tables likely start out small and grow later. As the SQL user adds new tables and queries, the user is less and less likely to be able to anticipate future optimization needs.

However, the database can analyze queries and determine not only which parts of a table are commonly read and written, but also for the database as a whole. We'll call this the "hotspot" of the database. The DBMS product's purpose then is to optimize around this hotspot, reorganizing itself to serve queries best when they hit the hotspot and not wait for human intervention to address the hotspot.

A database or table doesn't need to be "row-oriented" or "column-oriented" depending on the use-case- the database should choose a data arrangement suitable for serving the queries. If some queries are served better by row-oriented storage, then the database should be free to make that choice without the user's knowledge or intervention.

## How do we get to self-optimizing databases?

This section describes how to make a DBMS which is self-optimizing (without human intervention) while also not leaking abstractions.

### Define the user interface

Database users should engage with the relational algebra to define data models and queries, not quirks of the database such as `autovacuum_naptime` or `work_mem`.

The Antipattern:

Users are forced to adjust heuristics to make the database performant. Consultants must be hired to configure the database to meet business needs. Query hints are eventually added to the SQL dialect.

### Iterate on the optimizations

The DBMS must rearrange or copy data to best serve the queries via "data independence" whereby the relational algebra concepts presented to the user are separate from storage and optimization concerns- the ideal set of data representations which serve the queries which could include:

* creating and dropping indexes and other less-than-linear optimizations
* duplicating data in different formats
* selecting which data to cache in RAM, disk, or slower storage
* retrieving data from other participants in a database cluster
* profiling queries to identify hotspots
* analyzing query plans to anticipate hotspots
* running important/common queries in the background to ensure good performance and to keep caches up-to-date (equivalent to running experiments to confirm behavior)

The Antipattern:

With each new software version, the DBMS provides new configuration options tied to heurstics which must be manually adjusted. The DBMS maps tables to one or more files forever. The DBMS never reevaluates its assumptions about query performance, instead preferring to add minor performance improvements when users complain about it.

### Provide User Feedback

Just because the database should reorganize itself doesn't mean we can't provide meaningful insight into how it works. This is useful for debugging, validation, or understanding how the database is choosing to use resources. Therefore, visibility into the databases choices should be presented to specific users such as administrators who can decide whether more physical hardware such as RAM, CPUs, or cluster machines should be added to the system to serve the queries. The DBMS could even make such recommendations or plot a resource usage trajectory for administrators.

The Antipattern:

The DBMS exposes meaningless factors of a complex heuristic to allow DBAs to subtlely tweak performance-related algorithms. Because the values are exposed to the user, new version of the DBMS cannot change these algorithms. The interaction between various heuristics is unknown and there is no human-maintainable way to determine the best configuration due to the combinatorial explosion of values across configuration options. Configuring a database becomes akin to dark magic and requires hiring consultants or expensive third-party "tuner" software.

## Conclusion

The future of the DBMS means going back to fundamentals: expose data modeling concepts to the user and leave everything else to the DBMS which has the statistics to make solid choices about data organization without human input. One such effort is [Project:M36](https://github.com/agentm/project-m36) which implements:

* proper data independence as envisioned by C.J. Date with no promise of row-oriented or column-oriented storage for any relation as well as a self-organizing cache
* a faithful representation of the relational algebra
* support for algebraic data types allowing users to model reality more effectively
* no support for any configurable heuristics, preferring instead to automatically experiment with alternative query plans
* integration with the Haskell programming language

If you're interested in pursuing cutting-edge database research, please join the [project](https://github.com/agentm/project-m36)!