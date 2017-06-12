# Project:M36 Multi-Node Replication

## Introduction

The stored transaction graph feature makes Project:M36 naturally more suitable for replication. Just like with git, having access to historical transactions makes merging transactional data much more logical and unambiguous.

## Current Multi-Master Support

Project:M36 plans to support logical replication in the future. In the meantime, multi-master replication is supported through shared database directories. Using NFS or SMB shares where file locking is properly supported, one can run multiple ```project-m36-server``` instances on the same database directory. This is a shared-storage form of multi-master operation.

Multi-master replication is enabled and reasonably fast due to the intentional design decision to rewrite as few files as possible. Typical databases write and rewrite the same files which back relation variables (or tables) directly which necessitates heavy use of file locking or even locks on file content regions. Project:M36 operates on a write-once, read-many (WORM) principle whereby transaction data can be written without holding locks so only the transaction graph itself needs synchronization.

Multi-master support can also be useful for simple databases where one wishes to avoid running and maintaining a ```project-m36-server```. Instead, one can create in-process database instances which share the same database directory. This is similar to the use-case for SQLite. However, Project:M36 seamlessly supports server and in-process modes which makes moving from a small, local database to a larger server installation trivial; the storage format is identical.

To use the simple multi-master mode, start two instances of ```project-m36-server``` which point at the same local or remote database directory over NFS or SMB. No further configuration is required.

## Caveats

Project:M36 does not and cannot detect if the filesystem's implementation of file locking is actually functioning.

On Linux and macOS, any recent on-disk filesystems and NFS should support POSIX advisory locking, as required. However, sharing the database over samba (SMB) shares or mixed operating systems may result in database transaction graph corruption. The symptom will be that arbitrary, committed transactions may be missing from the transaction graph.

On Windows, locking is implemented using mandatory locking controls which any supported version of Windows offers. Cross-platform use of locking (such as with samba server on Linux) is probably *not* safe.

If a commit happens behind the back of another ```project-m36-server``` and one server attempts to commit, two possibilities can occur:

1. The commits occurred on two disparate branches, so the transaction graph can be updated without conflict.
1. The commits occurred on the same branch, so one transaction commit will be rejected with a ```TransactionIsNotAHead``` error. This means that the head (leaf node) of the transaction graph for that branch has moved on, so the transaction needs to be re-executed and committed.

To avoid ```TransactionIsNotAHead``` errors, branch the transaction graph and merge it to the head when ready to commit.
