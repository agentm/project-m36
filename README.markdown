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
1. [Transaction Graph Operators](docs/transaction_graph_operators.markdown)
1. [Persistent Library Driver](docs/persistent_library_driver.markdown)
1. [ProjectM36.Client Library](docs/projectm36_client_library.markdown)
1. [Adding New Data Types](docs/new_datatypes.markdown)
  
## Development

Project:M36 is developed in Haskell and compiled with GHC. 

Important modules:

1. Relation - represents a relation in the relational algebra and implements the relational operators
2. Relational Expressions - represents a composable abstract syntax tree for the relational algebra
3. Tutorial D Interpreter - one available frontend to evaluate relational expressions
4. ProjectM36.Client- user-facing native Haskell library

Other modules include:

* displaying a relation as HTML
* displaying a relation in the terminal
* relational expression static optimizer
* relational expression cost-based optimizer
* relational error enumeration
* import of CSV as a relation and export of a relation to CSV
* import of TutorialD from files