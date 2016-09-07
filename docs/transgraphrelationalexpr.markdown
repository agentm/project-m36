# TransGraphRelational Expressions

## Introduction

A standard relational expression queries the state of the database at a single point in time. This point-in-time is represented by an atomic transaction.

In contrast, a trans-graph relational expression can refer to any transaction's state in the entire history of the database. For example, a trans-graph relational expression could be used to determine how a relation variable has changed over time.

## Motivation

Common SQL DBMSs contend all database changes (INSERTs, UPDATEs, DELETEs) on a single state. The DBMS is responsible for simulating serialized access to this every-changing state. Access to previous states are impossible- the only state visible is the one which is available when the client initiated the transaction.

In constrast, Project:M36 allows queries against current *and* past transactions. This is useful for learning how a database has changed over time. One essential use for this feature is for "auditing", whereby the database can be queried to explain how, when, and by whom certain changes were made.

## TutorialD Usage

In practice, trans-graph relational expressions are very similar to normal relational expressions except that the database context for trans-graph relational expressions is **not** implied. This means that any reference to data in the database context must be marked with a transaction marker. This includes data such as relation variables and atom functions.

In ```tutd```, here are two equivalent queries (assuming that ```s``` and ```sp``` are relation variables which have been committed to the ```master``` branch):
```
TutorialD (master): :showexpr s join sp
```
```
TutorialD (master): :showtransgraphexpr s@master join sp@master
```

Note that the only difference is that the second expression explicitly references which transaction the relation variable is referencing. In the first relational expression, this marker is implicit because the query can only reference the current in-flight transaction.

Naturally, the trans-graph relational expression is most useful when querying across multiple transactions. This can be accomplished by specifying the transaction id (UUID) or using head name backtracking which is intentionally similar to the git traversal syntax.

```
:showtransgraphexpr s@master~
```

The transaction marker ```master~``` indicates that we wish to reference the first parent of the transaction at the head of the "master" branch.

Since transactions can have multiple parents due to merge commits, the ```^x``` syntax can be used to specify the ```x```-th parent of the referenced transaction.

These markers can be freely mixed to traverse the graph from a head, as in the following example:

```
TutorialD (master): :showtransgraphexpr s@improveheadlights~^2
```

This query asks to return relation variable ```s``` at the transaction context from the second parent of the first parent of the head of the "improveheadlights" branch.

## Reference

Note that relational expressions can query the current in-flight state (called the "disconnected transaction" because it is not committed), while the trans-graph relational expression can only query against committed transactions.

Note that the caret operator relies on a notion of parent transaction ordering (the n-th parent transaction of a merge transaction). While this ordering is stable, it is arbitrary, so it is up to the user to verify that the correct branch is traversed.

The same ```@``` syntax can be used with relation variables, atom functions and type constructors.

Other than the following markers, trans-graph relational expressions share identical syntax with relational expressions.

| Syntax | Example | Meaning |
|--------|---------|---------|
| &lt;uuid&gt; | ```rv@dc385ccc-387e-407d-b65f-e3681cf221a9``` | The relation variable ```rv``` at the state of transaction referred to by explicit unique identifier.|
| ^[&lt;n-th parent&gt;] | ```rv@addcustomer^2``` | The relation variable ```rv``` at the state of the second parent transaction of the head transaction of the branch ```addcustomer```. |
| ~[&lt;steps&gt;] | ```rv@addcustomer~3``` | The relation variable ```rv``` at the state of the great-grandparent of the head transaction of branch ```addcustomer```.|
