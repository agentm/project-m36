# SQLegacy: SQL on a Relational Algebra Engine

## Introduction

Project:M36 is a relational algebra engine supporting Haskell, TutorialD, and SQL interfaces. While SQL purports to based on the relational algebra, SQL [takes liberties](why_sqlegacy.markdown) which cause it to stray from the mathematics of the algebra. Regardless, SQL is a popular and well-supported language for database interaction, so Project:M36 supports its own dialect of SQL called "SQLegacy" to emphasize that it is not the preferred interface to the relational algebra. This document explains how to use the SQLegacy console for legacy SQL usage.

## Setting Up

To run the SQLegacy interactive console:

```
docker run -it projectm36/project-m36 sqlegacy
```

results in:

```
Project:M36 SQLegacy Interpreter 0.9.9
SQL does not support the complete relational algebra. To access the complete relational algebra, use the bundled "tutd" interpreter.
Type "help" for more information.
SQLegacy (master/main):
```

## SQL Commands

The following commands are supported:

* `SELECT`
* `UPDATE`
* `DELETE`
* `INSERT`
* `CREATE TABLE`
* `DROP TABLE`
* `BEGIN`
* `COMMIT`
* `ROLLBACK`
* `IMPORT EXAMPLE CJDATE;` to load your database with tables "s", "sp", and "p" from C.J. Date's books on the relational algebra

More information on how to use SQL can be found at existing [PostgreSQL tutorials](https://www.postgresqltutorial.com/).


## Limitations

SQLegacy does not support duplicate rows in any context. To create "duplicate" rows, create a surrogate primary key column such as with a UUID.