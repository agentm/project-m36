# Inline TutorialD

## Introduction

Interacting with a database typically involves writing queries in a language specific to the database such as SQL. Project:M36 offers both SQL and TutorialD language interpreters, but, unlike typical databases, the parsed structure is provided to the client-side as a Haskell data structure which is then submitted to the database executor. This has benefits such as:

* supporting multiple user-facing interfaces (SQL, TutorialD, or straight Haskell) without backend changes
* allowing the client to manipulate the abstract syntax tree of the compiled expression
* eliminating injection attack vectors by forbidding SQL strings used as remote procedure calls (RPC)

Project:M36 supports inlining TutorialD within a Haskell program. Basic familiarity with Haskell is needed to use this feature.

## Using QuasiQuotes

By leveraging Template Haskell, Haskell programmers can use TutorialD inline to generate the structure needed to query the Project:M36 database.

```
{-# LANGUAGE QuasiQuotes #-}
...
queryResult <- executeRelationalExpr sessionId conn [relationalExpr|s where city="London"|]
```

First, we enable the "QuasiQuotes" Haskell language extension. Then, we can drop the TutorialD `s where city="London"` into a position where a `RelationalExpr` is expected.

Note that the compiled result does *not* include the TutorialD string. We can see this by instructing GHC to dump the Template Haskell result with `-ddump-splices`:

```
    Restrict
      (AttributeEqualityPredicate
         (Data.Text.unpackCStringLen# "city"# 4)
         (NakedAtomExpr (TextAtom (Data.Text.unpackCStringLen# "London"# 2))))
      (RelationVariable (Data.Text.unpackCStringLen# "s"# 1) ())
```

which is similar to what we might write in standard Haskell.

We can also use inline TutorialD with "database context expressions" which modify a transactional state:

```
executeDatabaseContextExpr sessionId conn [databaseContextExpr|insert s relation{tuple{city "Adelaide", s# "S6", sname "Jacobs", status 10}}|]
```

Here, we insert a tuple into relation variable `s`.

## Parameterized Queries

Naturally, you may want to integrate arguments from Haskell into the query. In SQL, this is done with "parameterized queries" which look like this:

```
db.execute('SELECT * FROM s WHERE city=?', ('London',))
```

We can achieve something similar with TutorialD but, since the query is compiled to a data value on the client side, we replace the value client side.

```
  let london_suppliers =
        replaceTextAtom "$1" "London" [relationalExpr|s where city="$1"|]
  queryResult <- executeRelationalExpr sessionId conn london_suppliers
```

Note how we can replace the text atom "$1" with whatever Haskell value we wish. The server is not made aware that we have modified the data structure of the query. There is also nothing special about "$1".

```
  let london_suppliers =
        replaceTextAtom "Adelaide" "London" [relationalExpr|s where city="Adelaide"|]
  queryResult <- executeRelationalExpr sessionId conn london_suppliers
```

Replacing "Adelaide" with "London" is totally fine, too. The dollar markers are merely a convention.

Again, the parsing of the TutorialD happens at Haskell compile time and the "London" replacement happens on the client side.

Replacement functions include:

```
replaceIntegerAtom :: AtomReplacer a => Integer -> Integer -> a -> a
replaceAtoms :: AtomReplacer a => [(Atom, Atom)] -> a -> a
replaceTextAtom :: AtomReplacer a => T.Text -> T.Text -> a -> a
```

## More Transformations

Since these transformations occur client-side, any developer can write additional value transformations on the database queries.

```
  let notlondon =
        case [relationalExpr|s where city="London"|] of
          Restrict yeslondon@(AttributeEqualityPredicate "city" "London") relExpr ->
            Restrict (NotPredicate yeslondon)	relExpr
```

With a pattern match against the resultant query, we can invert the boolean logic filter. The developer can write arbitrary transformations suitable for an application such as:

* renaming relation variables on a per-customer basis
* adding restriction filters based on which user is connected

## Conclusion

By parsing database-specific languages in the client, the client has immediate access to the underlying data structure used to represent queries. This enables the client to manipulate the query data structure before sending the query to the database. Such transformations can include:

* query parameterization
* query composition of two or more queries
* query validation
* query modification by pattern matching

Finally, by not needing the concept of string-based RPC in the form of SQL sent to the server, we close a major security hole involving string injection attacks.