# Project:M36 Relational Algebra Engine

## Introduction

Project:M36 implements a relational algebra engine as inspired by the writings of Chris Date.

## Description

Unlike most database management systems (DBMS), Project:M36 is opinionated software which adheres strictly to the mathematics of the relational algebra. The purpose of this adherence is to prove that software which implements mathematically-sound design principles reaps benefits in the form of code clarity, consistency, performance, and future-proofing.

Project:M36 can be used as an in-process or remote DBMS.

Project:M36 is written entirely in the [Haskell programming language](https://www.haskell.org/).

## Use-Cases

Project:M36 supports multiple frontends which target different audiences.

* learn about the relational algebra via TutorialD
* store and manipulate databases
* use Project:M36 as a native Haskell database backend

## Documentation

1. [Installation and Introduction to Project:M36](docs/introduction_to_projectm36.markdown)
1. [Introduction to the Relational Algebra](docs/introduction_to_the_relational_algebra.markdown)
1. [TutorialD Tutorial](docs/tutd_tutorial.markdown)
1. [15 Minute Tutorial](docs/15_minute_tutorial.markdown)
1. [Transaction Graph Operators](docs/transaction_graph_operators.markdown)
1. [On NULL (in SQL)](docs/on_null.markdown)
1. [Persistent Library Driver](docs/persistent_library_driver.markdown)
1. [ProjectM36.Client Library](docs/projectm36_client_library.markdown)
1. [Adding New Data Types](docs/new_datatypes.markdown)
1. [ACID Database Properties](docs/acid_assessment.markdown)
1. [Serving Remote ProjectM36 Databases](docs/server_mode.markdown)
1. [Using Notifications](docs/using_notifications.markdown)
1. [Reaching "Out of the Tarpit" with Project:M36](docs/reaching_out_of_the_tarpit.markdown)

## Development

Project:M36 is developed in Haskell and compiled with GHC 7.8 or later.

## Related Projects

* [The Third Manifesto](http://thethirdmanifesto.com/): the philosophical basis for relational algebra engines
* [Rel](http://reldb.org/): a TutorialD implementation against a BerkeleyDB backend
* [Andl](http://andl.org/): a new database language with SQLite and PostgreSQL backends

## Suggested Reading

* [Out of the Tarpit](http://shaffner.us/cs/papers/tarpit.pdf): a proposed software architecture which minimizes state and complexity. Project:M36 implements the requirements of this paper.
* [Database Design & Relational Theory: Normal Forms and All That Jazz](http://shop.oreilly.com/product/0636920025276.do): mathematical foundations for the principles of the relational algebra
* [Database Explorations: Essays on the Third Manifesto and Related Topics](http://bookstore.trafford.com/Products/SKU-000177853/Database-Explorations.aspx): additional essays and debates on practical approaches to relational algebra engine design
