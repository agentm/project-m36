# Automatic Merge Notes

These notes present cases where two diverging transaction branches can be automatically merged. Later, we cover transaction branches which cannot be automatically merged without user intervention (either in advance or in real-time).

In the following diagrams, "A" is the divergence point (parent) for two branches "B1" and "B2". "B1" and "B2" are subsequently merged to "C".

## 1. No-op

![graph](http://g.gravizo.com/g?
  digraph G {
  node [shape=rect];
  A -> B1 [label="no change"];
  A -> B2 [label="no change"];
  B1 -> C [label="merge"];
  B2 -> C [label="merge"];
  }
)

In this base case, we see that the merge is always successful if the two branches since the parent transaction A is equivalent to B1, B2, and C.

## 2. One transaction changes

![graph](http://g.gravizo.com/g?
digraph G {
node [shape=record];
A [label="A|x|a Int|[]"];
A -> B1 [label="insert x [4]"];
A -> B2 [label="no change"];
B1 -> C [label="merge"];
B2 -> C [label="merge"];
C [label="C|x|a Int|[4]"];
}
)

The resultant merge is equivalent to A->B1->C ignoring B2. Since B2 adds nothing to the transaction stream, an equivalent graph would be the flat A->B1->B2->C. Such an application is similar to how git "merges" uses the fast-forward technique.

## 3. Both transactions make equivalent changes

![graph](http://g.gravizo.com/g?
digraph G {
node [shape=record];
A [label="A|x|a Int|[]"];
A -> B1 [label="insert x relation[tuple[a 4]]"];
A -> B2 [label="insert x relation[tuple[a 4]]"];
B1 -> C [label="merge"];
B2 -> C [label="merge"];
C [label="C|x|a Int|[[4]]"];
}
)

Both branches add the "4" tuple, thus the result must also contain "4". There is no conflict here because, unlike in SQL, relations may not contain duplicate tuples, so the merge is unambiguous. Because both branches agree on the "truth", the merge succeeds regardless of the actual changes (insert, delete, update). Thus, the result C is equivalent to executing just one branch of changes.

## 4. Two transactions add/remove different tuples

![graph](http://g.gravizo.com/g?
  digraph G {
  node [shape=record];
  A [label="A|x|a Int|[]"];
  A -> B1 [label="insert x relation[tuple[a 4]]"];
  A -> B2 [label="insert x relation[tuple[a 5]]"];
  B1 -> C [label="merge"];
  B2 -> C [label="merge"];
  C [label="C|x|a Int|[[4],[5]]"];
  }
)

Two relations add two unrelated tuples to the relation. We know they are unrelated because the merge does not trigger any constraint violations. A constraint violation during the merge would necessitate conflict resolution. In this case, the result is equivalent to the application of the branches in either order: A->B1->B2->C or A->B2->B1->C.

## 5. Two transactions update a tuple with the same key

![graph](http://g.gravizo.com/g?
digraph G {
node [shape=record];
A [label="A|employee|id Int,name Text|[[303,\"Dr. Garbez\"]]"];
A -> B1 [label="update employee where id=303 [name:=\"Jim Garbez\"]"];
A -> B2 [label="update employee where id=303 [name:=\"James Garbez\"]"];
B1 -> C [label="merge"];
B2 -> C [label="merge"];
B1 [label="B1|employee|id Int,name Text|[[303,\"Jim Garbez\"]]"];
B2 [label="B2|employee|id Int,name Text|[[303,\"James Garbez\"]]"];
C [label="C|employee|id Int,name Text|[[303,???]]"];
}
)

This type of merge will always result in a key constraint violation. Logically, the branches are referring to the same tuple, so the key constraint is logically violated as if the merge engine tried to insert both changed tuples into the result.

There are multiple possible conflict resolutions:

### 5a. Prefer a branch name

If a specifically-named branch is "preferred", the merge can proceed automatically with the other branch being completely discarded. This would be used, for example, when it is known that data flowing into a specific branch is authoritative. Note that this strategy (called "ours/theirs" merging in git) should discard all of a branches changes and not just the conflicted tuple changes.

### 5b. Perform some logic to determine how to merge conflicting tuple atoms

A stored procedure could determine which branch to choose or even how to merge values at the tuple or atom level. For example, the stored procedure could determine the "name" atom in the merged tuple result to be "Jim or James Garbez" (product) or "J. Garbez" (commonality) or even just "Garbez" (commonality) or the procedure could claim that the longer name is more likely to be correct.

In general, the stored procedure could inspect (reading only) the two branches in the tree and make any deterministic decision it wishes to make based solely on those branches (maintaining the purity of the function).

### 5c. Use the earlier/later changed

The chosen branch is determined by its commit timestamp. The other branch's changes are discarded entirely.

### 5d. Revert to shared ancestor

Due to ambiguity, the rule could be to revert to the parent ancestor state which is the first knwon consistent state prior to the branching.

### 6. User intervention

If none of the above strategies result in a non-constraint-violating merge transaction, then the merge must fail, perhaps marking some state for a human to intervene and take action (which may include no action).

# Schema changes

## 1. Adding/removing a relation

![graph](http://g.gravizo.com/g?
digraph G {
node [shape=record];
A;
A -> B1 [label="x:=true"];
A -> B2 [label="y:=true"];
B1 -> C [label="merge"];
B2 -> C [label="merge"];
C [label="C|x:=true and y:=true"];
}
)

If a relation has no cross-constraints with another relation, then adding either at any point in the timeline cannot interfere with the other.

## 2. Changing constraints

![graph](http://g.gravizo.com/g?
digraph G {
node [shape=record];
A [label="A|employee[id Int, name Text]|[[3, \"Jim Garbez\"]]"];
A -> B1 [label="+ key employee[id]"];
A -> B2 [label="+ key employee[name]"];
B1 -> C [label="merge"];
B2 -> C [label="merge"];
C [label="C"];
}
)

From all the examples, this is likely the most interesting since detecting when constraints conflict is not immediately obvious. Consider, for example, if A->B2 added a constraint "+ key employee[id,name]". This would not create a conflict under the conditions that both constraints can be added to the merge without causing a constraint violation, but is that the genuine intention of the user?

Consider this example:

![graph](http://g.gravizo.com/g?
digraph G {
node [shape=record];
A [label="A|integers[i Int]|[]"];
A -> B1 [label="+ constraint i<0"];
A -> B2 [label="+ constraint i>0"];
B1 -> C [label="merge"];
B2 -> C [label="merge"];
C [label="C"];
}
)

The end result is a relation "i" which cannot contain any tuples. Clearly, neither branch intended this to be the case, so does it make sense to merely concatenate the constraints?

## In general

The function to perform the merge should be of type:

```haskell
mergeBranches :: TransactionGraph -> BranchName -> BranchName -> TransactionGraph
mergeBranches subGraph branchNameA branchNameB = ...
```

The subGraph argument is a subGraph of the entire database transaction graph starting at the common ancestor and including all transactions from both branches until the potential merge point. This is a sub graph because I have not been able to think of a circumstance where more context would be necessary. For convenience, the merge system should also include a transaction diff function of type:

```haskell
diffTransactions :: Transaction -> Transaction -> DatabaseContextExpr
```

This function should make a best effort to compress the actual differences into as few actions as possible, though further research will likely discover a canonical representation. Specifically, when diff'ing two transaction in sequence, this function should hopefully return the minimal statements needed to get from one transaction state to the next. This diff function's result can then be analyzed by the merge function in its decision-making.

Each merge strategy can be implemented using separate functions of the above type, but deciding which to apply or which strategy should win automatically is likely an open research problem. Likely, this decision-making must be left up to the database developer who has the full context needed in order to make the decision.

In addition, it would seem valuable to the algorithms to determine which constraints would be violated without executing the action. Even if this were determinable some of the time, it would appear to be a valuable optimization.

The parallels between transaction merging and parallel lock management should not be underestimated. However, instead of critical sections, constraints appear to be able to represent additional complexity not usually attributed to locking.
