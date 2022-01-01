# Introduction to Project:M36

## What is Project:M36?
Project:M36 is a relational-algebra-based database management system. It can be used as a production-level database backend or for learning more about the relational algebra.

The principled mathematical approach to Project:M36's design sets it apart from competing DBMSs. Specifically, Project:M36 adheres strongly to the mathematics of the relational algebra and makes no compromises nor apologies in doing so. To that end, Project:M36 is implemented in the programming language "Haskell" which is itself based on the lambda calculus.

## The Relational Algebra Connection

The relational algebra is the mathematical model underpinning Project:M36. When compared to other "relational" DBMSs, by strictly adhering to the relational algebra, Project:M36 offers:

* predictable, type-safe composability of relational operators
* more opportunities for interesting optimizations
* elimination of odd and easy-to-forget corner cases (such as found when dealing with SQL's NULL)

## Binary Releases

The quickest and preferred method of installation is via docker, which works on Linux, macOS, and Windows. Docker downloads and runs preconfigured software in secure, sandboxed containers, preventing third-party software from altering the host system without your permission.

### Install Docker:

* [Windows 10 Docker Installation](https://docs.docker.com/docker-for-windows/wsl/)
* [MacOS Docker Installation](https://docs.docker.com/docker-for-mac/install/)
* [Linux Docker Installation](https://docs.docker.com/engine/install/#server)

### Run TutorialD from Docker Image

This downloads the docker image tagged "latest" and runs `tutd`, the TutorialD command line interface. Try the [15-Minute TutorialD Tutorial](15_minute_tutorial.markdown) and try some expressions.

```bash
docker run -it projectm36/project-m36 tutd
```

### Run Project:M36 Server

The default port for the project-m36 server is 6543.

```bash
docker run --network host projectm36/project-m36 project-m36-server -n <database_name>
```

### Connect to Project:M36 Server

```bash
docker run --network host projectm36/project-m36
```

## Source Installation

Requirements:

To build with stack:

* [GHC 8.6.5 or greater](https://www.haskell.org/downloads)
* [Haskell stack](https://docs.haskellstack.org/en/stable/README/)
* Linux, macOS, or Microsoft Windows

Compilation steps:

* ```git clone https://github.com/agentm/project-m36```
* ```cd project-m36```
* ```stack --stack-yaml stack.ghc8.6.yaml build```

At this point, the TutorialD interactive interpreter can be run using ```stack exec tutd```.

Alternative building with GHC 8.6.5 or greater:

* ```cabal new-build```

The resultant binaries can be found in ```./dist-newstyle/build/project-m36-<version>/build```.

## Accessing Project:M36 Databases

Project:M36 implements a generic relational algebra DBMS backend with multiple data access frontends. Such frontends currently include:

* TutorialD: Chris Date's interactive language for learning about the relational algebra
* ProjectM36.Client: a Haskell library for native access

More frontends are expected to be supported in the future.

## Beyond the Relational Algebra

In addition to being a relational algebra engine, Project:M36 supports features which one has come to expect from a modern DBMS.

### Transaction Graph

Most DBMSs provide a way to allow the user to specify atomic database state changes. These changes are often wrapped up as "transactions" which, when applied to the database state, offer the illusion of sequential and grouped, atomic changes applied to the databases. Often the only state which can be queried is the snapshot at the time the transaction is opened.

Project:M36 extends this transaction model with the "transaction graph". This feature can be adequately described as version control for transactions. In the legacy DBMS model, the transactions are added to a linear transaction stream where each client is contending over the "head" of the stream- the latest transactions. In the transaction graph model, clients can add named branches and add transactions to the branches, then merge changes back, if necessary.

This feature is useful, for example:
* during testing- create a testing branch which production will never see. This obviates the need for a separate production data set.
* during A/B testing- deploy a feature to some users, but not all.
* for implementation experimentation- try new features against production data without fear of data destruction
* for time travel- jump to transactions in the past and examine the state of the database
* for point-in-time recovery- undo an unlimited number of transactions to a known good state
* for multi-master replication- merge potentially-conflicting databases

Naturally, this design implies that data is never deleted because all transactions are saved, but the graph can be compressed and pruned. The cost in complexity of this feature is merely in defining merge strategies and is an area of ongoing research.

### Fine-grained Durability

In any application, some data is more important than other data. Many DBMSs, however, make it difficult to separate such data and, thus, all data is equally expensive to save and retain. This often leads the database designer to leave certain data out of the database or store it in a second DBMS.

Project:M36 allows the database client to specify what level of durability safety is required on a per-transaction basis. For example, if a specific transaction has less essential data, the transaction can be marked and committed to the database at a later time. However, further transactions on that branch must be marked with the same low durability level or the entire branch's durability is increased.

Decreasing a transaction's durability can reduce IO contention at the cost of reduced certainty that the transaction will be restored if the DBMS software or hardware fails.

### Native Haskell Compatibility

Any Haskell data type which implements the ```Atomable``` typeclass can be manipulated as a value in the database.

Haskell functions which operate on those values can also be added to the database.

### Parallelism

Project:M36 benefits from Haskell's excellent support for parallelism. Because every relational algebra query is logically pure in the referential transparency sense, every query can take advantage of multiple CPU cores. In the future, distributed query execution will also be possible.

### Function-based Relations

Relations can also be defined by functions, thereby taking effectively no storage to represent and no IO to generate or query. Any function which  generates supported values without duplicates can be represented as a relation, including functions which generate an potentially infinite number of values.

### Static- and Cost-based Optimizers

The relational algebra allows all optimizations to be written as algebraic transformations. The query execution graph is represented by the same algebraic constructs as the original query.

### CSV support

Though the CSV ("comma-separated values") format cannot represent all possible relations, it is nonetheless a basic format for exchange of tabular data. Project:M36 supports import and export of relations as CSV data whenever possible.

### Relations as Plots

N-attributed relations can be visualized as plots where each tuple represents a point in the plot. Project:M36 can generate gnuplot graphs from 2- and 3-attributed relations.
