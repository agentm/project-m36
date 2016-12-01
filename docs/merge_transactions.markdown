# Merge Transactions in Project:M36

## Introduction

Common database management systems offer one transaction "stream" whereby multiple database users are simultaneously contending for commits to access or change a single database state. Database management systems implement complex locking and caching in order to simulate concurrent access to this state.

Project:M36 allows for user-defined named "heads" which represent states (branches) onto which users can tack on additional transactions. This feature enables:

* experimental changes which may or may not be merged back to the main state of the database
* reduced contention over a single head
* genuinely transaction-safe DDL (schema definition changes) without locking, caveats, or abstraction leaks
* merge transactions according to business logic
* elimination of "serialization errors"

Programmers are likely familiar with this workflow since this behavior is analogous to source version control branching and merging. Project:M36 uses terminology from [git version control](https://git-scm.com/).

## Command Summary

|Command|Explanation|
|-------|-----------|
|:branch &lt;branch name&gt;|Create a new branch.|
|:jumphead &lt;head name&gt;|Jump to the named head.|
|:jump &lt;transaction id&gt;|Jump to an arbitrarily-identified transaction. This allows the session's context to travel back in time to a part of the graph which is not a head.|
|:showgraph|Display the transaction graph as a relation.|
|:mergetrans &lt;merge strategy&gt; &lt;branchA&gt; &lt;branchB&gt;|Merge one branch onto another. This requires that the current context is on one of the heads. The "merge strategy" is one of "union", "unionpreferbranch &lt;branch name&gt;" , or "selectedbranch".|

## Merge Strategies

### Selected Branch Merge

The naivest merge strategy is to pull in the database context of one branch while discarding the context of a second branch. This merge strategy is useful for marking the end of an experimental branch that is to be considered discarded or a dead end. This merge strategy always succeeds.

### Union Merge

The union merge strategy attempts to merge the components of the two branches' database contexts. These components are:

* inclusion dependencies (constraints)
* atom functions
* notifications
* types
* relation variables

If both branches have a atom function, etc. of the same name, then the merge fails with an unresolvable conflict.

As a special case for relation variables, if both branches have a relation variable of the same name and type (same attributes), then the resultant merged relation variable is a union of both branches' relation variables.

### Union Prefer Branch Merge

The union-prefer-branch merge is identical to union merge except that, when encountering a conflict case as described above, the strategy chooses a component from the preferred branch. Thus, this merge strategy always succeeds.

## Additional Strategies

Project:M36 will support more complex merge strategies in the future. In particular, business logic will likely dictate that Project:M36 support user-defined strategies, though this is not yet supported.

## TutorialD Usage

Branching and merging are built-in to the Project:M36 ```tutd``` client.

First, let's create a new branch for some new sales data.
```
TutorialD (master/main): :branch add_sales_feature
TutorialD (add_sales_feature):
```

Note how the name in parentheses changes as we create and implicitly jump to the new "add_sales_feature" branch.

Next, we will add the sales data.

```
TutorialD (add_sales_feature): sales := relation{tuple{amount 45, product "peanuts"},tuple{amount 20, product "onions"}}
TutorialD (add_sales_feature): :showexpr sales
┌───────────┬─────────────┐
│amount::Int│product::Text│
├───────────┼─────────────┤
│45         │"peanuts"    │
│20         │"onions"     │
└───────────┴─────────────┘
TutorialD (add_sales_feature): :commit
```

Now, the relation variable "sales" is available on the "add_sales_feature" branch, but *not* the "master" branch. Let's verify this.

```
TutorialD (add_sales_feature): :jumphead master
TutorialD (master/main): :showexpr sales
ERR: RelVarNotDefinedError "sales"
```

Now that we are on the "master" branch, let's add a relation variables unrelated to sales.

```
TutorialD (master/main): employees := relation{tuple{name "Steve"},tuple{name "Cindy"}}
TutorialD (master/main): :commit
```

Note that the "employees" relation variable is only available on the "master" branch and not on the "add_sales_feature" branch.

Let's look at the transaction graph:

```
TutorialD (master/main): :showgraph
┌─────────────┬───────────────────┬──────────────────────────────────────┬────────────────────────────────────────┐
│current::Bool│head::Text         │id::Text                              │parents::relation {id::Text}            │
├─────────────┼───────────────────┼──────────────────────────────────────┼────────────────────────────────────────┤
│False        │""                 │"0d3bf914-1695-4f95-ab93-6c543d7b3dfa"│┌──────────────────────────────────────┐│
│             │                   │                                      ││id::Text                              ││
│             │                   │                                      │├──────────────────────────────────────┤│
│             │                   │                                      ││"0e51d95b-5578-4190-802b-9ef280a7f386"││
│             │                   │                                      │└──────────────────────────────────────┘│
│False        │"add_sales_feature"│"8b4d4eaf-31a3-4e48-85d3-bf6e8ac101ea"│┌──────────────────────────────────────┐│
│             │                   │                                      ││id::Text                              ││
│             │                   │                                      │├──────────────────────────────────────┤│
│             │                   │                                      ││"0d3bf914-1695-4f95-ab93-6c543d7b3dfa"││
│             │                   │                                      │└──────────────────────────────────────┘│
│False        │""                 │"0e51d95b-5578-4190-802b-9ef280a7f386"│┌────────┐                              │
│             │                   │                                      ││id::Text│                              │
│             │                   │                                      │└────────┘                              │
│True         │"master"           │"e4228cb8-e792-48c9-b987-7d78fb1abe04"│┌──────────────────────────────────────┐│
│             │                   │                                      ││id::Text                              ││
│             │                   │                                      │├──────────────────────────────────────┤│
│             │                   │                                      ││"0e51d95b-5578-4190-802b-9ef280a7f386"││
│             │                   │                                      │└──────────────────────────────────────┘│
└─────────────┴───────────────────┴──────────────────────────────────────┴────────────────────────────────────────┘

```

```:showgraph``` shows us which database context is "current" and which parents are linked to each transaction.

Now, imagine that the work on the "add_sales_merge" branch is complete, so we will merge the "add_sales_feature" branch back into the "master" branch. We say we are merging "back" because both branches share a common ancestor transaction.

```
TutorialD (master/main): :mergetrans union add_sales_feature master
TutorialD (master/main): :showexpr employees
┌──────────┐
│name::Text│
├──────────┤
│"Steve"   │
│"Cindy"   │
└──────────┘
TutorialD (master/main): :showexpr sales
┌───────────┬─────────────┐
│amount::Int│product::Text│
├───────────┼─────────────┤
│45         │"peanuts"    │
│20         │"onions"     │
└───────────┴─────────────┘
```

After the merge, both relation variables appear in resultant merged transaction. This is because we used a "union merge strategy" which tries to mash both database contexts together.
