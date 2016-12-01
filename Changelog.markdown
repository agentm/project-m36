# 2016-11-30

## add functional dependency macro

Date demonstrates two ways to implement functional dependencies as constraints on page 21 in "Database Design and Relational Theory". A similar macros is now implemented in the tutd interpreter.

```funcdep sname_status (sname) -> (status) s```

[Documentation](/docs/tutd_tutorial.markdown#functional-dependencies)

# 2016-09-07

## add TransGraphRelationalExpr

The TransGraphRelationalExpr allows queries against all past states of the database.

The following example executes a query against two different committed transactions using syntax similar to that of git for graph traversal:
```:showtransgraphexpr s@master~ join sp@master```

[Documentation](/docs/transgraphrelationalexpr.markdown)
