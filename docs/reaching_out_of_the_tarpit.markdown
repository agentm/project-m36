# Reaching Out of the Tarpit with Project:M36

## Introduction

[Project:M36](https://github.com/agentm/project-m36) is a relational algebra database management system (RDBMS) whose purpose is to implement the foundation for functional relational programming as described in the excellent ["Out of the Tarpit"](http://shaffner.us/cs/papers/tarpit.pdf) paper. This document describes how Project:M36 meets the design goals laid out in the paper. It is recommended to read the paper before reading this document.

## Why is "Out of the Tarpit" Important?

The "Out of the Tarpit" paper, written in 2006, consolidates decades of software development research and experience to explain why software written today is chock-full of software bugs. The paper categorizes sources of complexity and then proceeds to propose a software architecture which promises to reduce unnecessary software complexity so that the resultant software is maximally focused on actual business rules.

## Laying the Foundations for Reduced Complexity

After analyzing sources of software complexity (primarily unnecessarily mutable state and unintended order-of-execution), the not-so-radical proposal of the "Out of the Tarpit" paper is "functional-relational programming", not to be confused with "[functional reactive programming](https://wiki.haskell.org/Functional_Reactive_Programming)". According to the paper, the ideal language would minimize mutable state (such as in functional programming) and eliminate unintended order-of-execution by providing declarative instead of imperative constructs.

The central software component of functional-relational programming is a relational algebra engine/database management system (RDBMS). Project:M36 purports to meet the software requirements of such a functional-relational system, perhaps being the first RDBMS to actually target the paper's proposed design.

## The Importance of the Relational Model

"Out of the Tarpit" recommends the relational model not only in its customary backend role, but throughout the application because the relational model is:

* **declarative**: thus removing accidental complexity (such as accidental ordering)
* **restricted**: the paper makes it clear that if a programming language allows a programmer to do something less-than-savory, then that misfeature will be used

and provides ([p.37](http://shaffner.us/cs/papers/tarpit.pdf)):

* **structure**: a closed world system whereby all data is represented by relations
* **manipulation**: a means to simulate mutation of the application state over time
* **data integrity**: constraints which prevent illogical data constructions
* **data independence**: the logical and physical representations of state are clearly separated

The relational model is a mathematical model for storing and manipulating "essential state" ([p.25](http://shaffner.us/cs/papers/tarpit.pdf)). As a mathematical model, it not beholden to any specific implementation or quirks.

Unfortunately, due to the quirks of legacy implementations, much of what developers learn about the relational model is wrong. Project:M36 aims to rectify this situation by providing a mathematically coherent implementation of the relational model. Contrast the mathematical approach to existing databases which sacrifice or elide a mathematical model in lieu of "performance". Refer to [Knuth on premature optimization](https://en.wikiquote.org/wiki/Donald_Knuth) for additional details.

## Chris Date's Contribution

Chris Date is a prolific author, researcher, and lecturer on relation algebra and database design in general. Project:M36 would not exist without his writings on the mathematical underpinnings of the relational algebra. Project:M36 does not however implement all Date's recommendations. In particular, Project:M36 implements:

* **algebraic data types** inspired by Haskell's types in lieu of Date's convoluted data types based on object-oriented type systems
* **transactions in a graph** in lieu of Date's temporal constructs
* **a TutorialD dialect** which is not 100% compatible with Date's proposal
* **isomorphic schemas** (not yet implemented) in lieu of Date's updateable views

## Project:M36 In Practice

Values which Project:M36 can store are typed by the powerful functional programming language Haskell. Any Haskell data value can become a database value by implementing a simple typeclass. Thus, it provides a native and seamless interface between Haskell and the relational algebra engine.

Project:M36 does not, however, implement a completely new programming language for functional-relational programming, as suggested in the paper. The benefits of a fully-restricted programming language are described in the paper and this feature may be implemented in the future.

Project:M36 implements multiple interfaces:

* an interactive console implementing [TutorialD](https://en.wikipedia.org/wiki/D_%28data_language_specification%29)
* a native Haskell API
* a database driver for the [persistent framework](http://www.yesodweb.com/book/persistent)

### Real Estate Database Example

To understand how Project:M36 stacks up against the fictional programming language shown in the paper, we have implemented the [example from the paper](examples/out_of_the_tarpit.tutd). The language used in the script is a dialect of Chris Date's TutorialD language.

### Feeders and Observers

The "Out of the Tarpit" paper distinguishes between pushing data to the database and pulling data out of the database ([p.46](http://shaffner.us/cs/papers/tarpit.pdf)): "feeders" continually update the state of the database to best reflect reality and "observers" request notifications on relational state changes and take action on them.

SQL developers are already intimately familiar with "feeders"- these are merely INSERT and UPDATE statements issues to the database via some middleware.

However, most database do not support and adequate notion of observers. The purpose of an observer is to notify interested parties about relevant changes to the state of the database so that clients relying on the database are viewing the most "up-to-date" information from the database. Project:M36 implements this requirement by allowing clients to register relational expressions by name and return relational expression. When the result of the relational expression changes in a tracked state (branch) of the database, an asynchronous notification including the name and the result of return relational expression is sent to the client. The client can then use this information to update its own user interface or take further automated actions.

Some SQL databases include a similar feature; in PostgreSQL, this is activated by [LISTEN](http://www.postgresql.org/docs/9.0/static/sql-listen.html)/[NOTIFY](http://www.postgresql.org/docs/9.0/static/sql-notify.html). However, PostgreSQL offers no provision for triggering notifications based on the state of the database- this crucial feature is left to the database developer. Furthermore, the return payload can only be a string, not the result of a relational expression evaluation.

## Good Riddance to Old Baggage

Project:M36 is a clean-room implementation of a relational algebra engine; it does not implement SQL or any other legacy standard. While this means that Project:M36 is incompatible with other RDBMS, it means that Project:M36 can avoid the substantial pitfalls of previous implementations, such as:

* [three-valued logic (NULL)](docs/on_null.markdown)
* non-relational database constructs such as row and column ordering or OUTER JOINs
* linear transaction flow and conflict resolution
* direct table-to-file mapping which create perverse model-data dependencies
* key-value storage: a key-value mapping is valued merely for its performance characteristics, not because it models reality well
* restrictive, legacy type systems which impede modeling
* unnecessarily complex SQL grammar

## Future Directions

Project:M36 is just getting started as an ambitious open-source project- please join us! Immediate future features include:

* transaction graph branch merging (never bother with "master-to-master" conflicts again)
* making maximum use of parallelization
* static and statistics-based optimizations
* multiple, simultaneous storage representations for relations
* more network access methods, including a WebSocket driver
* support for relations with infinitely many tuples

## Try It!

To learn more about Project:M36, try the [15 minute tutorial](docs/15_minute_tutorial.markdown).
