# Ξ Project:M36 Relational Algebra Engine

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](https://www.haskell.org)
[![Public Domain](https://img.shields.io/badge/license-Public%20Domain-brightgreen.svg)](http://unlicense.org)
[![Hackage](https://img.shields.io/hackage/v/project-m36.svg)](http://hackage.haskell.org/package/project-m36)
[![Hackage dependency status](https://img.shields.io/hackage-deps/v/project-m36.svg)](http://packdeps.haskellers.com/feed?needle=project-m36)
[![Github Workflow status](https://github.com/agentm/project-m36/workflows/CI/badge.svg)](https://github.com/agentm/project-m36/actions?query=workflow%3ACI)


*Software can always be made faster, but rarely can it be made more correct.*

## Introduction

Project:M36 implements a relational algebra engine as inspired by the writings of Chris Date.

## Quick Install

Project:M36 can be downloaded and run via docker, which supports Windows 10, macOS, and Linux.

Run `docker run -it projectm36/project-m36 tutd` to start the TutorialD command line interface.

## Description

Unlike most database management systems (DBMS), Project:M36 is opinionated software which adheres strictly to the mathematics of the relational algebra. The purpose of this adherence is to prove that software which implements mathematically-sound design principles reaps benefits in the form of code clarity, consistency, performance, and future-proofing.

Project:M36 can be used as an in-process or remote DBMS.

Project:M36 is written entirely in the [Haskell programming language](https://www.haskell.org/).

## Sample Session

[![asciicast](https://asciinema.org/a/3syu35c8cydm403292a74l1n5.png)](https://asciinema.org/a/3syu35c8cydm403292a74l1n5)

## Use-Cases

Project:M36 supports multiple frontends which target different audiences.

* learn about the relational algebra via TutorialD
* store and manipulate databases
* use Project:M36 as a native Haskell database backend

## Community

* [Developer's Blog](https://agentm.github.io/project-m36/)
* [Mailing List/Discussion Group](https://groups.google.com/d/forum/project-m36)
* IRC Channel: irc.libera.chat #project-m36 -- [Chat via Web Client](http://kiwiirc.com/nextclient/irc.libera.chat:+6697/#project-m36)
* [Hackage](https://hackage.haskell.org/package/project-m36)
* [Diogo Biazus' Project:M36 Video Tutorial](https://www.youtube.com/watch?v=_GC_lxlVEnE)

## Documentation

### Introductory Materials

1. [Installation and Introduction to Project:M36](docs/introduction_to_projectm36.markdown)
1. [Introduction to the Relational Algebra](docs/introduction_to_the_relational_algebra.markdown)
1. [TutorialD Cheatsheet](docs/tutd_cheatsheet.markdown)
1. [TutorialD via Jupyter Notebook Walkthrough](jupyter/TutorialD%20Notebook%20Walkthrough.ipynb)
1. [TutorialD Tutorial](docs/tutd_tutorial.markdown)
1. [15 Minute Tutorial](docs/15_minute_tutorial.markdown)
1. [Developer's Change Log](Changelog.markdown)
1. [Simple Client API](docs/simple_api.markdown)
1. [CSV Import/Export](docs/import_export_csv.markdown)
1. [Developer Setup](docs/dev_setup.markdown)

### Database Comparisons

1. [ACID Database Properties](docs/acid_assessment.markdown)
1. [On NULL (in SQL)](docs/on_null.markdown)
1. [Reaching "Out of the Tarpit" with Project:M36](docs/reaching_out_of_the_tarpit.markdown)
1. [An Architecture for Data Independence](docs/data_independence.markdown)

### Advanced Features

1. [Transaction Graph Operators](docs/transaction_graph_operators.markdown)
1. [ProjectM36.Client Library](docs/projectm36_client_library.markdown)
1. [Data Frames for Sorting](docs/dataframes.markdown)
1. [Adding New Data Types](docs/new_datatypes.markdown)
1. [Database-Manipulating Functions](docs/database_context_functions.markdown)
1. [Serving Remote ProjectM36 Databases](docs/server_mode.markdown)
1. [Using Notifications](docs/using_notifications.markdown)
1. [Merge Transactions](docs/merge_transactions.markdown)
1. [Atom (Value) Functions](docs/atomfunctions.markdown)
1. [Trans-Graph Relational Expressions](docs/transgraphrelationalexpr.markdown)
1. [Isomorphic Schemas](docs/isomorphic_schemas.markdown)
1. [Replication](docs/replication.markdown)
1. [Basic Operator Benchmarks](https://rawgit.com/agentm/project-m36/master/docs/basic_benchmarks.html)
1. [Merkle Transaction Hashes](docs/merkle_transaction_graph.markdown)
1. [Handling DDL Changes](docs/Handling_DDL_Changes.markdown)

### Integrations

1. [WebSocket Server](docs/websocket_server.markdown)
1. [Jupyter Notebook Kernel](docs/jupyter_kernel.markdown)
1. [Persistent Driver](https://github.com/YuMingLiao/persistent-project-m36)

## Development

Project:M36 is developed in Haskell and compiled with GHC 9.2 or later.

## Related Projects

* [The Third Manifesto](http://thethirdmanifesto.com/): the philosophical basis for relational algebra engines
* [Rel](http://reldb.org/): a TutorialD implementation against a BerkeleyDB backend
* [Andl](http://andl.org/): a new database language with SQLite and PostgreSQL backends
* [Coddie](https://github.com/scvalencia/Coddie): a python-based relational algebra interpreter

## Suggested Reading

* [Out of the Tarpit](https://github.com/papers-we-love/papers-we-love/blob/2eb8d21/design/out-of-the-tar-pit.pdf): a proposed software architecture which minimizes state and complexity. Project:M36 implements the requirements of this paper.
* [Database Design & Relational Theory: Normal Forms and All That Jazz](http://shop.oreilly.com/product/0636920025276.do): mathematical foundations for the principles of the relational algebra
* [Database Explorations: Essays on the Third Manifesto and Related Topics](http://bookstore.trafford.com/Products/SKU-000177853/Database-Explorations.aspx): additional essays and debates on practical approaches to relational algebra engine design
