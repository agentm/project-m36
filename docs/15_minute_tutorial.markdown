# Project:M36 in 15 Minutes

Thank you for evaluating Project:M36, a new relational algebra engine written in Haskell. This tutorial will give you a general overview of the features of the database management system (DBMS). This tutorial assumes basic knowledge of your terminal's shell.

## Prerequisites

* any Linux
* GHC 7.10 or later version (Haskell compiler)
* cabal (package downloader) installed

## Download and Install

```bash
git clone https://github.com/agentm/project-m36.git
cabal sandbox init
cabal install --dependencies-only
cabal run tutd
```

##  Some Basic TutorialD

When you run the ```tutd``` interpreter, you are interacting with an in-memory database using a dialect of the TutorialD language which is described in numerous book by Chris J. Date. It is a language specifically designed for human interaction with the relational algebra.

Note that TutorialD is strongly-typed- no type coercions take place automatically. For example, an integer will never be interpreted as a string or vice versa.

### Load the Canonical Chris Date Example

```
TutorialD (master): :importtutd "scripts/DateExamples.tutd"
```

```:importtutd``` (do not skip typing the preceding colon), loads a file which contains a script written in the TutorialD dialect used by Project:M36 and executes it to change the state of the database. In this case, the relations "s" (supplier), "p" (product), and "sp" (suppliers-products) are now loaded.

### Execute Relational Expressions

```
TutorialD (master): :showexpr s
┌──────────┬────────┬───────────┬───────────┐
│city::Text│s#::Text│sname::Text│status::Int│
├──────────┼────────┼───────────┼───────────┤
│"Paris"   │"S2"    │"Jones"    │10         │
│"Paris"   │"S3"    │"Blake"    │30         │
│"London"  │"S1"    │"Smith"    │20         │
│"London"  │"S4"    │"Clark"    │20         │
│"Athens"  │"S5"    │"Adams"    │30         │
└──────────┴────────┴───────────┴───────────┘
```

Use ```:showexpr``` (note the preceding colon again) followed by a TutorialD relational expression to execute the expression against the database's current state.

A relational expression operates on the current database state (also known as "database context") and returns a relation containing the result. Note that relational expressions do not modify the state of the database.

You will need to [learn TutorialD](/docs/tutd_tutorial.markdown) to create relational expressions here, but here are a few quick, simple examples:

#### Joins

```
TutorialD (master): :showexpr s join sp
┌──────────┬────────┬────────┬────────┬───────────┬───────────┐
│city::Text│p#::Text│qty::Int│s#::Text│sname::Text│status::Int│
├──────────┼────────┼────────┼────────┼───────────┼───────────┤
│"London"  │"P3"    │400     │"S1"    │"Smith"    │20         │
│"London"  │"P4"    │300     │"S4"    │"Clark"    │20         │
│"Paris"   │"P2"    │200     │"S3"    │"Blake"    │30         │
│"Paris"   │"P1"    │300     │"S2"    │"Jones"    │10         │
│"London"  │"P5"    │400     │"S4"    │"Clark"    │20         │
│"London"  │"P6"    │100     │"S1"    │"Smith"    │20         │
│"London"  │"P5"    │100     │"S1"    │"Smith"    │20         │
│"Paris"   │"P2"    │400     │"S2"    │"Jones"    │10         │
│"London"  │"P4"    │200     │"S1"    │"Smith"    │20         │
│"London"  │"P1"    │300     │"S1"    │"Smith"    │20         │
│"London"  │"P2"    │200     │"S1"    │"Smith"    │20         │
└──────────┴────────┴────────┴────────┴───────────┴───────────┘
```

All joins in TutorialD are similar to "NATURAL JOIN" in SQL.

#### Restriction

```
TutorialD (master): :showexpr s where status=20
┌──────────┬────────┬───────────┬───────────┐
│city::Text│s#::Text│sname::Text│status::Int│
├──────────┼────────┼───────────┼───────────┤
│"London"  │"S1"    │"Smith"    │20         │
│"London"  │"S4"    │"Clark"    │20         │
└──────────┴────────┴───────────┴───────────┘
```

Restrictions are similar to the "WHERE" clause in SQL.

#### Projection

```
TutorialD (master): :showexpr p{color,p#,weight}
┌───────────┬────────┬───────────┐
│color::Text│p#::Text│weight::Int│
├───────────┼────────┼───────────┤
│"Red"      │"P6"    │19         │
│"Red"      │"P1"    │12         │
│"Blue"     │"P5"    │12         │
│"Green"    │"P2"    │17         │
│"Red"      │"P4"    │14         │
│"Blue"     │"P3"    │17         │
└───────────┴────────┴───────────┘
```

Projection is equivalent to "SELECT <columns>" in SQL.

#### Attribute Rename

```
TutorialD (master): :showexpr s rename {city as town}
┌────────┬───────────┬───────────┬──────────┐
│s#::Text│sname::Text│status::Int│town::Text│
├────────┼───────────┼───────────┼──────────┤
│"S2"    │"Jones"    │10         │"Paris"   │
│"S3"    │"Blake"    │30         │"Paris"   │
│"S4"    │"Clark"    │20         │"London"  │
│"S1"    │"Smith"    │20         │"London"  │
│"S5"    │"Adams"    │30         │"Athens"  │
└────────┴───────────┴───────────┴──────────┘
```

Renaming attributes is equivalent to "SELECT city AS town..." in SQL.

#### Relational "true" and "false"

The relational algebra includes two fundamental named relations: "true" and "false". (Chris Date calls them "TABLE_DUM" and "TABLE_DEE" which are poor names.)

"true" is the name of the relation with no attributes and a set of one tuple of no attributes.

```
TutorialD (master): :showexpr true
┌┐
││
├┤
└┘
```

"false" is the name of the relation with no attributes and an empty tuples set.

```
TutorialD (master): :showexpr false
┌┐
││
└┘
```

These relations cannot be represented in SQL databases but are fundamental to the mathematics of the relational algebra. Any relation projected against no attributes will return "true" if the relation contained any tuples or "false" otherwise.

### Execute Database Context Expressions

Database context expressions change the state of the database, much like "CREATE TABLE", "INSERT", "UPDATE", or "DELETE" in SQL. Database expressions in the interpreter are *not* prefixed by a colon because they are the default operation type.

#### Create a Relation Variable

```
TutorialD (master): x:=relation{name Text, age Int}{tuple{name "Steve", age 40},tuple{name "Mike", age 31}}
TutorialD (master): :showexpr x
┌────────┬──────────┐
│age::Int│name::Text│
├────────┼──────────┤
│40      │"Steve"   │
│31      │"Mike"    │
└────────┴──────────┘
```

This is similar to SQL's "CREATE TABLE" operator.

#### Insert Additional Tuples

```
TutorialD (master): x:=relation{name Text, age Int}{tuple{name "Steve", age 40},tuple{name "Mike", age 31}}
TutorialD (master): :showexpr x
┌────────┬──────────┐
│age::Int│name::Text│
├────────┼──────────┤
│40      │"Steve"   │
│31      │"Mike"    │
└────────┴──────────┘
TutorialD (master): insert x relation{tuple{name "Bob", age 24}}
TutorialD (master): :showexpr x
┌────────┬──────────┐
│age::Int│name::Text│
├────────┼──────────┤
│40      │"Steve"   │
│24      │"Bob"     │
│31      │"Mike"    │
└────────┴──────────┘
```

Note that the relational insertion is identical to "x:=x union relation{...}".

#### Update Tuples in a Relation Variable

```
TutorialD (master): :showexpr s
┌──────────┬────────┬───────────┬───────────┐
│city::Text│s#::Text│sname::Text│status::Int│
├──────────┼────────┼───────────┼───────────┤
│"Paris"   │"S2"    │"Jones"    │10         │
│"Paris"   │"S3"    │"Blake"    │30         │
│"London"  │"S1"    │"Smith"    │20         │
│"London"  │"S4"    │"Clark"    │20         │
│"Athens"  │"S5"    │"Adams"    │30         │
└──────────┴────────┴───────────┴───────────┘
TutorialD (master): update s where city="London" (status:=35)
TutorialD (master): :showexpr s
┌──────────┬────────┬───────────┬───────────┐
│city::Text│s#::Text│sname::Text│status::Int│
├──────────┼────────┼───────────┼───────────┤
│"London"  │"S4"    │"Clark"    │35         │
│"London"  │"S1"    │"Smith"    │35         │
│"Paris"   │"S2"    │"Jones"    │10         │
│"Paris"   │"S3"    │"Blake"    │30         │
│"Athens"  │"S5"    │"Adams"    │30         │
└──────────┴────────┴───────────┴───────────┘
```
#### Deleting Tuples from a Relation Variable

```
TutorialD (master): :showexpr s
┌──────────┬────────┬───────────┬───────────┐
│city::Text│s#::Text│sname::Text│status::Int│
├──────────┼────────┼───────────┼───────────┤
│"Paris"   │"S2"    │"Jones"    │10         │
│"Paris"   │"S3"    │"Blake"    │30         │
│"London"  │"S1"    │"Smith"    │20         │
│"London"  │"S4"    │"Clark"    │20         │
│"Athens"  │"S5"    │"Adams"    │30         │
└──────────┴────────┴───────────┴───────────┘
TutorialD (master): delete s where sname="Adams"
TutorialD (master): :showexpr s
┌──────────┬────────┬───────────┬───────────┐
│city::Text│s#::Text│sname::Text│status::Int│
├──────────┼────────┼───────────┼───────────┤
│"Paris"   │"S2"    │"Jones"    │10         │
│"Paris"   │"S3"    │"Blake"    │30         │
│"London"  │"S1"    │"Smith"    │20         │
│"London"  │"S4"    │"Clark"    │20         │
└──────────┴────────┴───────────┴───────────┘
```

### Execute Transaction Graph Expressions

Unlike typical SQL DBMSs, Project:M36 maintains a graph of transactions, similar to how git stores source patches. This allows the database context to be branched (with a name), merged, and enables recalling the database context from past transactions.

#### Committing

```
TutorialD (master): :commit
```

#### Rollback

```
TutorialD (master): :rollback
```

#### Branching

```
TutorialD (master): :branch experimental
TutorialD (experimental): employee:=relation{name Text, salary Int}
TutorialD (experimental): :commit
```

Note how the prompt changes to reflect the current branch's name.

#### Jump to Branch

```
TutorialD (experimental): :jumphead master
TutorialD (master): :showexpr employee
ERR: RelVarNotDefinedError "employee"
```

Now we have jumped back to the master branch and we can confirm that the "employee" relation variable is not present here despite being committed. It was, in fact, committed only on the "experimental" branch.

#### Other operations

Of course, Project:M36 supports [aggregate operators](/docs/tutd_tutorial.markdown#aggregate-queries), [value functions](/docs/tutd_tutorial.markdown#extend), [database constraints](/docs/tutd_tutorial.markdown#constraints) including candidate and foreign keys, and other operators not mentioned in this brief tutorial.

While this tutorial only covered an in-memory database, Project:M36 also supports ACID-compliant filesystem persistence as well as [server-based operation](/docs/server_mode.markdown).

## Conclusion

The TutorialD dialect used by Project:M36 is ideal for learning about the relational algebra because it is quick to write and grammatically unambiguous.

The TutorialD language is not, however, suitable for production use because such use would be susceptible to the same string injection attacks which plague SQL. For integration with middleware, please try the [persistent library driver](/docs/persistent_library_driver.markdown) or [the native Haskell interface](/docs/projectm36_client_library.markdown).