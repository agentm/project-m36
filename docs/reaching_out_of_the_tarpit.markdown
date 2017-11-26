# Reaching Out of the Tarpit with Project:M36

## Introduction

[Project:M36](https://github.com/agentm/project-m36) is a relational algebra database management system (RDBMS) written in the Haskell programming language whose purpose is to implement the foundation for functional relational programming as described in the excellent paper ["Out of the Tarpit"](https://github.com/papers-we-love/papers-we-love/blob/2eb8d21/design/out-of-the-tar-pit.pdf). This document describes how Project:M36 meets the design goals laid out in the paper.

## Why is "Out of the Tarpit" Important?

The "Out of the Tarpit" paper, written in 2006, consolidates decades of software development research and experience to explain why software written today is chock-full of software bugs. The paper categorizes sources of complexity and then proceeds to propose a software architecture which promises to reduce unnecessary software complexity so that the resultant software is maximally focused on actual business rules. It is recommended to read [the paper](https://github.com/papers-we-love/papers-we-love/blob/2eb8d21/design/out-of-the-tar-pit.pdf) before reading the remainder of this document.

## Laying the Foundations for Reduced Complexity

After analyzing sources of software complexity (primarily unnecessarily mutable state and unintended order-of-execution), the not-so-radical proposal of the "Out of the Tarpit" paper is "functional-relational programming", not to be confused with "[functional reactive programming](https://wiki.haskell.org/Functional_Reactive_Programming)". According to the paper, the ideal language would minimize mutable state (such as in functional programming) and eliminate unintended order-of-execution by providing declarative instead of imperative constructs.

The central software component of functional-relational programming is a relational algebra engine/database management system (RDBMS). Project:M36 meets the software requirements of such a functional-relational system, perhaps being the first RDBMS to actually target the paper's proposed design.

## The Importance of the Relational Model

"Out of the Tarpit" recommends the relational model not only in its customary backend role, but throughout the application because the relational model is:

* **declarative**: thus removing accidental complexity (such as accidental ordering)
* **restricted**: removing mistaken features; if a programming language allows a programmer to do something less-than-savory, then that misfeature will be used

and provides ([p.37](https://github.com/papers-we-love/papers-we-love/blob/2eb8d21/design/out-of-the-tar-pit.pdf)):

* **structure**: a closed world system whereby all data is represented by relations
* **manipulation**: a means to simulate mutation of the application state over time
* **data integrity**: constraints which prevent illogical data constructions
* **data independence**: logical and physical representations of state which are clearly separated

The relational model is a mathematical model for storing and manipulating "essential state" ([p.25](https://github.com/papers-we-love/papers-we-love/blob/2eb8d21/design/out-of-the-tar-pit.pdf)). As a mathematical model, it not beholden to any specific implementation or quirks.

Unfortunately, due to the quirks of legacy implementations, much of what developers learn about the relational model is [wrong](/docs/on_null.markdown). Project:M36 aims to rectify this situation by providing a mathematically coherent implementation of the relational model. Contrast the mathematical approach to existing databases which sacrifice or elide a mathematical model in lieu of "performance". Refer to [Knuth on premature optimization](https://en.wikiquote.org/wiki/Donald_Knuth) for additional details.

## Chris Date's Contribution

Chris Date is a prolific author, researcher, and lecturer on the relational algebra and database design in general. Project:M36 would not exist without his writings on the mathematical underpinnings of the relational algebra. Project:M36 does not however implement all Date's recommendations. In particular, Project:M36 implements:

* **algebraic data types** inspired by Haskell's types in lieu of Date's convoluted data types based on object-oriented type systems
* **transactions in a graph** in lieu of Date's temporal constructs
* **a TutorialD dialect** which is not 100% compatible with Date's proposal
* **isomorphic schemas** (not yet implemented) in lieu of Date's updateable views

## Project:M36 In Practice

Values which Project:M36 can store are typed by the powerful functional programming language Haskell. Any Haskell data value can become a database value by implementing a simple typeclass. Thus, Project:M36 provides a native and seamless interface between Haskell and the relational algebra engine.

Project:M36 does not, however, implement a completely new programming language for functional-relational programming, as suggested in the paper. The benefits of a fully-restricted programming language are described in the paper and this feature may be implemented in the future.

Project:M36 implements multiple interfaces:

* an interactive console implementing [TutorialD](https://en.wikipedia.org/wiki/D_%28data_language_specification%29), a small language for learning about the relational algebra
* a native Haskell API

### Real Estate Database Example

To understand how Project:M36 stacks up against the fictional programming language shown in the paper, we have implemented the [example from the paper](/examples/out_of_the_tarpit.tutd). The language used in the script is a dialect of Chris Date's TutorialD language. It can be loaded into an interactive ```tutd``` session like so:

```
TutorialD (master/main): :importexample date
TutorialD (master/main): :showexpr p
┌───────────────────┬────────────┬───────────────────┬────────────┬────────────┐
│address::Address   │agent::Agent│dateRegistered::Day│photo::Text │price::Price│
├───────────────────┼────────────┼───────────────────┼────────────┼────────────┤
│Address "Main St." │Agent "Bob" │Day 2014-01-01     │"photo1.jpg"│Price 1000.0│
│Address "Elm St."  │Agent "Bob" │Day 2014-01-03     │"photo2.jpg"│Price 1200.0│
│Address "Maple St."│Agent "Sam" │Day 2014-02-05     │"photo3.jpg"│Price 800.0 │
└───────────────────┴────────────┴───────────────────┴────────────┴────────────┘
```

### Infrastructure Requirements

Project:M36 meets or exceeds the enumerated "Infrastructure" ([p.47](https://github.com/papers-we-love/papers-we-love/blob/2eb8d21/design/out-of-the-tar-pit.pdf)) requirements (except one):

* **Infrastructure for Essential State**: relation variable manipulation, storage, and data types
* **Infrastructure for Essential Logic**: relational expression evaluation, functions to operate on values, a language (TutorialD or Haskell), type inference, and constraints
* **Infrastructure for Accidental State and Control**: data independence (how relations are defined need not reflect how they are stored or manipulated)
* **Infrastructure for Feeders and Observers**: insert/update/delete and change notifications (including historical state retrieval)


Derived relations, as described in the paper, are not supported in Project:M36 because it is not clear that they are mathematically coherent. Regardless, it is clear that, while handy, derived relations (also known in SQL as "views") are not a requirement of the relational algebra.

### Feeders and Observers

The "Out of the Tarpit" paper distinguishes between pushing data to the database and pulling data out of the database ([p.46](https://github.com/papers-we-love/papers-we-love/blob/2eb8d21/design/out-of-the-tar-pit.pdf)): "feeders" continually update the state of the database to best reflect reality and "observers" request notifications on relational state changes and take action on them.

SQL developers are already intimately familiar with "feeders"- these are merely INSERT and UPDATE statements issued to the database via some middleware.

However, most databases do not support an adequate notion of observers. The purpose of an observer is to notify interested parties about relevant changes to the state of the database so that clients relying on the database are viewing the most "up-to-date" information from the database. Project:M36 implements this requirement by allowing clients to register trigger relational expressions and relational expressions to return alongside the notification. When the result of the relational expression changes in a tracked state (branch) of the database, an asynchronous notification including the name and the result of the return relational expression is sent to the client. The client can then use this information to update its own user interface or take further automated actions.

Some SQL databases include a similar feature; in PostgreSQL, this is activated by [LISTEN](http://www.postgresql.org/docs/9.0/static/sql-listen.html)/[NOTIFY](http://www.postgresql.org/docs/9.0/static/sql-notify.html). However, PostgreSQL offers no provision for triggering notifications based on the state of the database- this crucial feature is left to the database developer. Furthermore, the return payload can only be a string, not the result of a relational expression evaluation.

## Good Riddance to Old Baggage

Project:M36 is a clean-room implementation of a relational algebra engine; it does not implement SQL or any other legacy standard. While this means that Project:M36 is incompatible with other RDBMSs, it means that Project:M36 can avoid the substantial pitfalls of previous implementations, such as:

* [three-valued logic (NULL)](/docs/on_null.markdown)
* non-relational database constructs such as row and column ordering or OUTER JOINs
* linear transaction flow and conflict resolution
* direct table-to-file mappings which create perverse model-data dependencies
* key-value storage: a key-value mapping is valued merely for its performance characteristics, not because it models reality well
* restrictive, legacy type systems which impede modeling
* unnecessarily complex SQL grammar

## Summary of Benefits of This Architecture

The "Out of the Tarpit" paper summarizes the benefits of functional relational programming, but here is a summary of the summary:

### Benefits For State

* useless accidental state is avoided
* constraints prevent entering any "bad" or unexpected states
* referential transparency (functional purity)

### Benefits For Control

* declarative programming opens greater doors for optimizations such as automatic parallelization

### Benefits for Code Volume

* declarative programming can only specify modeling of business rules (not accidental or tangential complexity) and is thus naturally terse
* declarative programming eliminates code for state and control, reducing complexity

### Benefits for Data Abstraction

* the relational algebra reduces reliance on specific "code paths" for data access since data "groupings" are often arbitrary
 * for example: the same relation variable can be restricted/projected/joined in order to present the most relevant information for a given user interface

Project:M36 does, however, implement product types, which the paper discourages. The reasons for this are out-of-scope for this essay.

## Future Directions

Project:M36 is just getting started as an ambitious open-source project- please join us! Immediate future features include:

* transaction graph branch merging
* making maximum use of parallelization
* decentralized databases (like git source control)
* static and statistics-based optimizations
* multiple, simultaneous storage representations for relations
* more network access methods, including a WebSocket driver
* support for relations with infinitely many tuples

## Try It!

To learn more about Project:M36, try the [15 minute tutorial](/docs/15_minute_tutorial.markdown).
