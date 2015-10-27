# ACID Database Properties

## Introduction

[ACID](https://en.wikipedia.org/wiki/ACID) properties are the most important guarantees any database must make to the database user. ACID is an acronym which refers to how a database handles:

* **Atomicity** - state changes from committed transactions must never be partially visible; transactions are *atomically* applied to the database state
* **Consistency** - database constraints hold at all times
* **Isolation** - new transactions can only see state from previously committed transactions; transactions are *isolated* from each other, even if there are multiple, fresh transactions in flux
* **Durability** - committed transactions must be accessible even after sudden power loss or corruption

### Importance

The importance of these properties cannot be understated. Any single component missing could make a database unsuitable for high-value data.

## A.C.I.D. Assessment for Project:M36

### Atomicity

Through normal use of functional programming's immutable data structures, it is impossible to compile  Project:M36 to reference any transactions which are not already part of the committed transaction graph.

Compliance: 100%

### Consistency

While user may create and destroy arbitrary database constraints, there is no feature to relax the constraints, even temporarily. Project:M36 supports all possible, definable database constraints in the form of inclusion dependencies.

Compliance: 100%

### Isolation

Each transaction can only reference data from its parent transactions. It is impossible to query data from uncommitted transactions. Software transactional memory is used to ensure that the transaction graph is updated consistently even under concurrent-use conditions.

Compliance: 100%

### Durability

Project:M36 uses write-once files and directories coupled with fdatasync()/fsync() and journaled filesystems to avoid the need of double-writing with the write-ahead-log method. When a transaction is committed, its changes are written to a temporary directory, the directory's files are fdatasync()'d, then the temporary directory is atomically rename()'d into the database directory and the directory is fsync()'d.

However, Project:M36 allows the user to disable fsync()'ing, for example, for the purposes of creating temporary, disposable databases or for in-memory-only databases.

In addition, Project:M36 relies on the user to use Project:M36 databases on ordered, journaled metadata filesystems only. A future improvement could be to detect scenarios under which Project:M36 is not offering guaranteed durability. Write-ahead-logs do not suffer from this requirement.

We feel that these potential pitfalls warrant removing an arbitrary 10% from the compliance scale because there is room for further improvement here. It is, however, possible to use Project:M36 with full durability guarantees.

Compliance: 90%