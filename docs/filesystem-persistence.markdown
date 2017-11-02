# Project-M36 Filesystem Persistence

## Introduction

Existing database persistent storage is not suitable for Project:M36 storage for several reasons:

* typical SQL storage doesn't support relation-valued attributes
 * Project:M36 supports nested relations, relations as values- a useful feature of the relational algebra
* typical SQL storage relies on writing and rewriting files to save space
 * Project:M36 uses a write-one-read-many strategy to support database branching, concurrency, and compression
* typical SQL storage maps each table to one or more files
 * Project:M36 obviates the one-to-one mapping between relation variables and files and allows for multiple representations of a single relation variable and evaluated expressions on disk
* typical SQL storage maintains only the latest state of the database
 * Project:M36 stores a graph of transactions and supports multiple heads of contention
* typical key-value storage is poorly typed or even schemaless
 * Project:M36 is strongly-typed which enforces correctness and can benefit compression schemes and performance

## Requirements

* storage of an append-at-leaves (transaction) graph with highly concurrent read access
* database [durability](https://en.wikipedia.org/wiki/ACID#Durability)
* extensibility
 * examples include future compression types, dynamic atom (value) types, database context extensions, new relation types (such as recurrence relations), backwards-compatibility for atom values
* platform-agnosticism
 * postgresql's C-struct-to-disk design means that switching platforms requires a dump-and-restore, even if the database directory can be physically moved to the new machine- it's not always clear what constitutes a platform changes (float/int timestamps)
* upgradeability
 * postgresql software upgrades require painful database dump-and-restores or pg_upgrade, causing downtime
 * be able to read older formats to prevent costly data-rewrites
* backwards-compatibility
 * no one wants to rewrite their database files during upgrades, even if it is on-the-fly like with pg_upgrade
* multiple remote readers and writers to the storage
 * mutliple servers can read/write the same storage over networked filesystems as long as there are some basic locking guarantees
 * multiple servers can read the same storage without locking guarantees
 * we can protect WORM storage with fine-grained locking and still maintain high-concurrency
* singular format for data export/import (dump/restore)
 * we won't need a pseudo-cross-platform dump format (like pg_dump's output) because the file format is already cross-platform
 * sharing the format for internal and external usage means we can share the same tools
 * the dump format can be a subset of the internal format
* existence of a Haskell-based serialization library
* POSIX filesystem semantics and atomicity guarantees

## Current, Insufficient Design

### Filesystem Requirements

* POSIX, journaled filesystem
 * ext3/4, xfs, etc.
 * simulate POSIX semantics on Windows possibly with locking

The first pass of the on-disk storage for Project:M36 was implemented with a focus on correctness, but not performance or intelligent disk storage usage. As such, it is valuable as a baseline for the new implementation.

* uses Data.Binary to serialize entire Haskell data structures to disk
* includes lots of redundancy
 * for example, each new transaction writes all current relation variables to disk- instead, we should be able to reference unchanged relvars from previous transactions (they are already safely on disk)
* loses the in-memory data structure sharing that was present when writing the data (one of the benefits of immutable data structures)
* writes a platform-specific format
* is not extensible, backwards-, or forwards-compatible
* cannot be used as a portable, exportable format
* compression is all-or-nothing

### Directory Layout

* /m36v*x* - includes a human-readable transaction graph listing. The *x* references the version of persistence format.
* /lockFile - an empty file used for synchronizing multiple project-m36 server contending for the transaction graph
* /heads - a human-readable mapping of head names to transaction ids
* /<transaction id> - a directory containing all data needed to reconstruct a transaction
 * /atomfuncs - a Binary-serialized listing of `AtomFunction`s
 * /dbcfuncs - a Binary-serialized listing of `DatabaseContextFunction`s
 * /incdeps - a Binary-serialized listing of `InclusionDependency`s
 * /info - a Binary-serialized listing of `TransactionInfo` (current and parent transaction ids)
 * /relvars - a directory containing files named after the relvars with each file's contents being a serialized `Relation`
  * dynamic names should not appear in the filesystem as the filesystem naming requirements will be different from Project:M36 naming requirements
 * /schemas - a Binary-serialized listing of alternative isomorphic schemas
 * /typecons - a Binary-serialized listing of `TypeConstructor`s (data types)

## New, Shiny, Proposed Design

### Directory Layout

Most of the existing directory layout remains useful. The one-transaction-per-directory layout maximizes read performance using a WORM strategy. Later transactions can refer to past transactions to cut down on immediate IO. Due to the idempotence of the `DatabaseContextExpr`, we can optionally serialize the expression itself to further reduce and defer IO. Deferred updates can be resolved on-demand or in the background by replacing transaction directories, individual files, or by adding supplementary relation representations (more on these below).

Improvements:

* wrap up small objects together
 * We experienced unnecessarily high disk usage when saving atom functions in individual files. Wrap up smaller data structures (especially when less than 4KB) into one file.
* use [cbor](https://github.com/well-typed/cborg) binary format to allow external tools to be able to read the on-disk format
  * the cbor format allows for forwards and backwards compatibility for atom types and includes relevant built-in types

### Relation representations

One area where almost every DBMS has fallen flat is in *data independece*, the separation between the logical database state representation and the on-disk format. Project:M36 aims to remedy this with "relation representations" which acknowledges that every relation (including the results of relational expresssions) can have multiple on-disk or in-memory representations. The purpose of a query planner is to choose the optimal representation for the given request.

Most DBMSes do make an inadvertent attempt to support multiple relation representations via indexes. An index can be viewed as an independent representation of a relation in a different format (optimized for certain queries), but these same DBMSes fail to hide this abstraction in the logical layer, instead forcing users to create and maintain indexes as a separate entity.

Relational representations, however, are never presented to the user of the logical layer of the database. Such representations can be created automatically. For example, if a certain query result (always a relation) is requested often, the DBMS may choose to cache the result as a relation representation mapped to the query itself. Thereafter, if the query is requested again (within an unchanged database state, of course), the planner can choose to send the pre-computed result back. This is known as a "query cache", but we can extend this same model to any other possible representation.

Another representation type includes alternative representations of a relation variable's value designed for specific projections or restrictions. For example, a relation may be stored on disk as a btree with the nodes representing a relation's key. This is obviously equivalent to an index (either populated with the remainder of the tuple attributes or not, if the query is merely checking for existence).

Note that if two relation representations are isomorphic, as in the example of a btree containing the key and remaining tuple values, then all other representations of the same relation are redundant. Under storage pressure, we can delete all but one representation.  Which one the DBMS chooses to delete is arbitrary. When we have adequate space, we can create additional representations in the background without affecting correctness.

The on-disk representation need not correspond to the logical relation variables at all. Besides query caches, the DBMS can freely store multiple relations in the same file as long as there is a relational expression to distinguish between the logical relation variables. Conversely, a single relation variable's value could span multiple files, simulating what other DBMSes call sharding but without exposing the implementation details to the users.

Considering that user involvement in the creation of relation representations is the norm, how can the DBMS create such representations and maintain them automatically? This will likely be an area of research, but we can start simply with a query cache and automatic cost-based indexing based on actual queries executed.

When broken free from user control, the DBMS is free to create arbitrary representations with the only consideration being available IO and memory resources.

### Deferred updates

Virtually all DBMSes execute database updates against existing files which store tuples. However, because Project:M36 updates are idempotent, we can save expressions to disk for *deferred updates*. Database update expressions saved to disk can drastically increase the rate at which we can execute updates because the expression is likely far smaller than the number of tuples which the expression will change. Because Project:M36 uses per-transaction directories, once a deferred update is actually evaluated, the directory can be atomically replaced with the evaluated result. Of course, to save disk storage and IO, the DBMS can also choose to discard the evaluated result and continue to store the update. This is nothing novel- git does this by storing code diffs between commits. The only novelty is in recognizing the value of idempotent updates.

### Linked relations

Every database update creates a new state based on an old state and an update expression. In the previous section, we demonstrated how the expression along with the previous state is sufficient to create the new state. Alternatively, the DBMS can also store a "diff" (difference) in the form of a relational expr with a link to the previous state.

For example, if we have a relation variable of employees' names and we insert another employee name, the new state is the union of the previous state of the relation variable and a relation containing the new employee's name. The evaluated expression which requests the new state is merely replaced with the union- similar to a macro.

This strategy can also be viewed as a form of compression. In the general case where a relation variable has not changed from one transaction to the next transaction, we can merely store what amounts to a symbolic link to the relation variable in the previous state.

### Risks of Redesign

* cborg module has had only one release, is probably less than ready for production use, but the initial results are promising and is developed and backed by well-known Haskellers and a Haskell shop
 * are there alternatives we should consider?
* increased code complexity
 * executor needs to be separated from current relational EDSL to support scanning various types of relation representations
 * the new EDSL may not leak its representations to the user-level EDSL
 * query executor will need access to the full transaction graph to cover relation links and deferred updates; currently, all state is wrapped up into the `DatabaseContext`
* how and when to create new relation representations and when to defer updates will be an active area of research
* requires more consideration in reducing IO and storage requirements; this was not a major driver of the initial design
