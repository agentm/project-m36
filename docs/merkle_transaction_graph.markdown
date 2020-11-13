# Project:M36: Merkle Hashing for Transaction Graph

## Introduction

Project:M36 supports an append-only transaction graph model (similar to git) whereby new transactions can be added to the graph but not removed. As with any graph, each node can contain a Merkle hash which ensures not only that it's state since being added to the graph has not changed but also that none of its parent nodes all the way to the root node (initial state) have not been modified. This validation has a number of use cases.

## Usage

Project:M36 automatically adds Merkle hashes to every transaction. All data which represents the database state at that transaction is included in the hash including (but not limited to) relation variable definitions, atom and database context functions, constraints, and types.

Project:M36 validates all merkle hashes at database startup. For users who need validation after the database is started, the user can issue `:validatemerklehashes` at the `tutd` command line.

### Audit Logging

Project:M36 supports audit logging by allowing users to travel to transactions in the past and recreate the database state at that past point-in-time. To ensure that those past states cannot be modified either by malicious on-disk or in-memory changes, each transaction is protected by its own Merkle hash. If the calculated hash does not match the stored hash, Project:M36 can immediately report which transaction triggered the tamper-proof log.

### Reproducibility

Since a Merkle tree is effectively immutable except to add more nodes, it can be used to save and restore a whole database with very high confidence in the results. This feature could also be used to restore a database partially; for example, to duplicate just the latest database state on a second machine for OLAP analysis.

### High-Availability

Merkle tree hashes can be used to assert database integrity across high-availibility or database replication. Since the hash covers all database state, it can be used to validate log-shipping-based replication. Corrupted cache tuple data can be easily detected and flushed.

## Merkle Hashes in Other DBMSes

While Merkle hashes have become prevalent in blockchain-style databases, they have not made many appearances in typical SQL databases. This is likely due to the fact that such databases consider any additional operations against the large tuple stores as prohibitively expensive. Project:M36 is able to skirt this cost by only hashing database update expressions rather than the result of these executed expressions. This also means that the Merkle hash can be computed against transactions which have been created but never executed.

## Conclusion

Merkle hashes are an obvious improvement to any graph-based transaction log and are a low-cost method of ensuring data integrity.

A further improvement could allow users to specify an initial key provided by an admin user to salt the hash used to create the database. Additionally, a multi-user system could allow individual users to sign their committed transactions. The signature would then be included in the Merkle hash.
