# TutorialD Tutorial for Project:M36

TutorialD is an logic language designed by Chris Date for interacting with any relational algebra engine. Project:M36 supports this language for the purposes of learning about the relational algebra. TutorialD is not recommended for use in production systems.

It is presumed that the audience for this tutorial has at least some minimal background with an SQL-based DBMS.

### Starting the TutorialD Interpreter

From the project-m36 source directory, execute ```cabal run tutd``` to start the interactive TutorialD interpreter. You will be greeted by the interactive prompt:

```
TutorialD (master/main):
```

"master" refers to the name of the branch to which the current transaction will be potentially committing.

### Date's Sample Relations

The examples in this tutorial can be executed after loading Chris Date's sample relations using:

```
:importexample cjdate
```

Each section assumes a fresh instance of the Date examples. The relation variables created in the script are:

|Relation Variable Name|Description|
|----------------------|-----------|
|s|suppliers|
|p|parts|
|sp|contains mapping of which suppliers supply which parts|

### Importing TutorialD from Other Sources

The `tutd` console can also run TutorialD from a local file or from a web server.

To import TutorialD from a file:

```
:importtutd "file:///home/agentm/project-m36/scripts/emp.tutd"
```

or from a web server:

```
:importtutd "https://raw.githubusercontent.com/agentm/project-m36/master/scripts/DateExamples.tutd" "db9f3d9fe06d0a29b8355e045b89ec94d6428e3a3de93def7ca77bf0298b7010"
```

The second argument is an optional SHA256 hash in hexadecimal form. This can be used to validate that the script has not changed since the last import. Importing TutorialD from the web is in general safe because it is effectively sandboxed with "Safe Haskell" for functions, so the script is contained security-wise, however, nothing prevents the script from running forever. Downloading a schema from the web is a great way to share schemas and to kick start your project without starting from scratch.

### Types

TutorialD is strongly-typed. The basic built-in types are:

|Type Name|Explanation|Example|
|---------|-----------|-------|
|Text|arbitrary text|"The Old Man and the Sea"|
|Integer|arbitarily-sized integer|-4|
|Int|machine word integer|int(10)|
|DateTime|timestamp UTC|dateTimeFromEpochSeconds(1502304846)|
|Date|calendar date|fromGregorian(2017,05,30)|
|Double|floating point number|3.1459|
|Bool|boolean value|t|
|Bytestring|arbitrary-length string of bytes- input is base64-encoded|bytestring("dGVzdGRhdGE=")|
|Interval x|interval/range type for ints, doubles, datetimes, and dates|interval(3,5,f,f)|

With regards to boolean values, be sure not to conflate ```t``` or ```f``` as a boolean value with ```true``` and ```false``` which are relation variables.

The ```interval``` function last two arguments are boolean values indicating whether the interval is open at the start point and end point respectively.

Project:M36 will complain loudly if the expected types do not match. Automatic type coercion does not exist.

```
TutorialD (master/main): :showexpr s:{more:=add(10,@sname)}
ERR: AtomFunctionTypeError "add" 2 IntAtomType StringAtomType
```

The integer "10" cannot be added to the string sname value.

### Using Relation Variables

Relation Variables are named cells which reference relations within a transaction state. The relation can be replaced with another relation through assignment.

Definition of a relation variable:
```
products :: {name Text, age Integer}
```
This expression creates a new relation variable in the current context which can refer to any relation with the header of "name" of type "char" and "age" of type int.

Assignment to a relation variable:
```
products := relation{tuple{name "Mike",age 6},tuple{name "Sam",age 10}}
```
This expression assigns a relation containing two tuples to the previously-defined "products" relation.

```tutd``` includes two relations by default:
* ```true```: the relation with no attributes and a body of one empty-attributed tuple
* ```false```: the relation with no attributes and a body of no tuples

Chris Date refers to these relations as "TABLE_DUM" and "TABLE_DEE" but such arbitrary names are not so recognizable. ```true``` and ```false``` can be easily remembered as the answer to the question: "Does this relation have any tuples?" which can be formulated in TutorialD as the projection of a relation asking for no attributes:
```
products{}
```

### Relational Expressions

A relational expression combines relational operators to create a new relation. ```tutd``` can display the result of executing a relational expression with ":showexpr".

```
TutorialD (master/main): :showexpr p
┌──────┬─────┬──┬─────┬──────┐
│city  │color│p#│pname│weight│
├──────┼─────┼──┼─────┼──────┤
│London│Red  │P6│Cog  │19    │
│Paris │Blue │P5│Cam  │12    │
│London│Red  │P1│Nut  │12    │
│London│Red  │P4│Screw│14    │
│Oslo  │Blue │P3│Screw│17    │
│Paris │Green│P2│Bolt │17    │
└──────┴─────┴──┴─────┴──────┘
```

To show all relation variables in the current session, use ```:showrelvars```:

```
TutorialD (master/main): :showrelvars
┌─────────────────────────────────────────────────┬──────────┐
│attributes::relation {attribute::Text,type::Text}│name::Text│
├─────────────────────────────────────────────────┼──────────┤
│┌───────────────┬──────────┐                     │"sp"      │
││attribute::Text│type::Text│                     │          │
│├───────────────┼──────────┤                     │          │
││"p#"           │"Text"    │                     │          │
││"qty"          │"Int"     │                     │          │
││"s#"           │"Text"    │                     │          │
│└───────────────┴──────────┘                     │          │
│┌───────────────┬──────────┐                     │"true"    │
││attribute::Text│type::Text│                     │          │
│└───────────────┴──────────┘                     │          │
│┌───────────────┬──────────┐                     │"p"       │
││attribute::Text│type::Text│                     │          │
│├───────────────┼──────────┤                     │          │
││"weight"       │"Int"     │                     │          │
││"p#"           │"Text"    │                     │          │
││"color"        │"Text"    │                     │          │
││"city"         │"Text"    │                     │          │
││"pname"        │"Text"    │                     │          │
│└───────────────┴──────────┘                     │          │
│┌───────────────┬──────────┐                     │"s"       │
││attribute::Text│type::Text│                     │          │
│├───────────────┼──────────┤                     │          │
││"status"       │"Int"     │                     │          │
││"s#"           │"Text"    │                     │          │
││"sname"        │"Text"    │                     │          │
││"city"         │"Text"    │                     │          │
│└───────────────┴──────────┘                     │          │
│┌───────────────┬──────────┐                     │"false"   │
││attribute::Text│type::Text│                     │          │
│└───────────────┴──────────┘                     │          │
└─────────────────────────────────────────────────┴──────────┘
```

### Using Relational Operators

Relational operators generate new relations from existing relations. The operators form the closed algebra against relations.

#### Rename

The unary rename operator outputs a new relation with chosen attributes renamed. In SQL, the equivalent is to use "AS" in the column name list of SELECTS.

```
TutorialD (master/main): :showexpr s rename {city as town}
┌────────┬───────────┬───────────────┬──────────┐
│s#::Text│sname::Text│status::Integer│town::Text│
├────────┼───────────┼───────────────┼──────────┤
│"S3"    │"Blake"    │30             │"Paris"   │
│"S4"    │"Clark"    │20             │"London"  │
│"S5"    │"Adams"    │30             │"Athens"  │
│"S1"    │"Smith"    │20             │"London"  │
│"S2"    │"Jones"    │10             │"Paris"   │
└────────┴───────────┴───────────────┴──────────┘
```

#### Projection

Projection is applied by adding curly braces and attribute names following a relational expression. Projection in SQL is applied by adding a list of column names to a SELECT statement.

For example:

```
TutorialD (master/main): :showexpr p{color,city}
┌──────────┬───────────┐
│city::Text│color::Text│
├──────────┼───────────┤
│"London"  │"Red"      │
│"Paris"   │"Green"    │
│"Oslo"    │"Blue"     │
│"Paris"   │"Blue"     │
└──────────┴───────────┘

```

Projection attributes can also be inverted using `all but`:

```
TutorialD (master/main): :showexpr s{all but city}
┌────────┬───────────┬───────────────┐
│s#::Text│sname::Text│status::Integer│
├────────┼───────────┼───────────────┤
│"S2"    │"Jones"    │10             │
│"S3"    │"Blake"    │30             │
│"S1"    │"Smith"    │20             │
│"S5"    │"Adams"    │30             │
│"S4"    │"Clark"    │20             │
└────────┴───────────┴───────────────┘
```

Projection attributes can be derived from a relational expression using `all from`:

```
TutorialD (master/main): :showexpr (s join sp){all from s}
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"London"  │"S1"    │"Smith"    │20             │
│"London"  │"S4"    │"Clark"    │20             │
│"Paris"   │"S2"    │"Jones"    │10             │
│"Paris"   │"S3"    │"Blake"    │30             │
└──────────┴────────┴───────────┴───────────────┘
```

Above, we join `s` and `sp` but project the result back onto the attributes of `s`.

Projection attributes can be unioned from multiple sources using `union of`:

```
TutorialD (master/main): :showexpr (s join sp){union of {all from s} {all but p#}}
┌──────────┬────────────┬────────┬───────────┬───────────────┐
│city::Text│qty::Integer│s#::Text│sname::Text│status::Integer│
├──────────┼────────────┼────────┼───────────┼───────────────┤
│"Paris"   │400         │"S2"    │"Jones"    │10             │
│"London"  │400         │"S1"    │"Smith"    │20             │
│"Paris"   │200         │"S3"    │"Blake"    │30             │
│"London"  │200         │"S4"    │"Clark"    │20             │
│"London"  │200         │"S1"    │"Smith"    │20             │
│"Paris"   │300         │"S2"    │"Jones"    │10             │
│"London"  │100         │"S1"    │"Smith"    │20             │
│"London"  │300         │"S4"    │"Clark"    │20             │
│"London"  │300         │"S1"    │"Smith"    │20             │
│"London"  │400         │"S4"    │"Clark"    │20             │
└──────────┴────────────┴────────┴───────────┴───────────────┘
```

Here, after joining `s` and `p`, the union of all attributes from `s` and all attributes from `p` except for `p#` are returned.all

Finally, projection attributes can be intersected using `intersection of`:

```
TutorialD (master/main): :showexpr (s join sp){intersection of {all from s} {all from p}}
┌──────────┐
│city::Text│
├──────────┤
│"London"  │
│"Paris"   │
└──────────┘
```

Here, after joining `s` and `p`, we project on the intersection of attributes from `s` and `p` which happens to be `city`.

#### Restriction

Restriction is applied using a "where" clause, much like in SQL.

```
TutorialD (master/main): :showexpr p where color="Blue" and city="Paris"
┌──────────┬───────────┬────────┬───────────┬───────────────┐
│city::Text│color::Text│p#::Text│pname::Text│weight::Integer│
├──────────┼───────────┼────────┼───────────┼───────────────┤
│"Paris"   │"Blue"     │"P5"    │"Cam"      │12             │
└──────────┴───────────┴────────┴───────────┴───────────────┘
```

The restriction predicate can be built from "and", "not", and "or". Boolean atom functions can also appear in a restriction as long as they are preceded by "^". For example:

```
TutorialD (master/main): :showexpr s where ^lt(@status,20)
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Paris"   │"S2"    │"Jones"    │10             │
└──────────┴────────┴───────────┴───────────────┘
```

#### Join

Joins are binary operators and are applied with the "join" keyword. The equivalent in SQL would be a "NATURAL JOIN". To join attributes which are not identically-named, use the `rename` operator.

```
TutorialD (master/main): :showexpr s join sp
┌──────────┬────────┬────────────┬────────┬───────────┬───────────────┐
│city::Text│p#::Text│qty::Integer│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼────────────┼────────┼───────────┼───────────────┤
│"London"  │"P2"    │200         │"S1"    │"Smith"    │20             │
│"London"  │"P6"    │100         │"S1"    │"Smith"    │20             │
│"London"  │"P3"    │400         │"S1"    │"Smith"    │20             │
│"Paris"   │"P2"    │400         │"S2"    │"Jones"    │10             │
│"London"  │"P5"    │400         │"S4"    │"Clark"    │20             │
│"Paris"   │"P1"    │300         │"S2"    │"Jones"    │10             │
│"London"  │"P1"    │300         │"S1"    │"Smith"    │20             │
│"Paris"   │"P2"    │200         │"S3"    │"Blake"    │30             │
│"London"  │"P4"    │300         │"S4"    │"Clark"    │20             │
│"London"  │"P2"    │200         │"S4"    │"Clark"    │20             │
│"London"  │"P4"    │200         │"S1"    │"Smith"    │20             │
│"London"  │"P5"    │100         │"S1"    │"Smith"    │20             │
└──────────┴────────┴────────────┴────────┴───────────┴───────────────┘
```

As a convenience, `tutd` also supports semijoin (`matching`) and antijoin (`not matching`) syntax.

`s matching sp` or `s semijoin sp` is equivalent to `(s join sp){all from s}` which returns all tuples in `s` which appear in the join of `s` and `sp`.

`s not matching sp` or `s antijoin sp` is equivalent to `s minus (s matching sp)` which returns all tuples in `s` which do not appear in the join of `s` and `sp`. Thus, semjoin and antijoin are inverses of each other.

#### Union

The binary union operator merges tuples from both relations if-and-only-if the attributes of both relations are identical. The SQL equivalent is the "UNION" operator. Unlike SQL, however, duplicate tuples are not tolerated, so there is no "UNION ALL" equivalent in the relational algebra.

```
TutorialD (master/main): :showexpr s union s
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Paris"   │"S3"    │"Blake"    │30             │
│"Athens"  │"S5"    │"Adams"    │30             │
│"London"  │"S1"    │"Smith"    │20             │
│"Paris"   │"S2"    │"Jones"    │10             │
│"London"  │"S4"    │"Clark"    │20             │
└──────────┴────────┴───────────┴───────────────┘
```

The union of any relation with itself is itself.

#### Extend

The extend unary operator adds an attribute to a relation's header and body and is represented by a colon ":". The equivalent in SQL is a subselect in a SELECT column list. Typically, extend is used to add information derived from relation.

```
TutorialD (master/main): :showexpr s:{status2:=add(10,@status)}
┌──────────┬────────┬───────────┬───────────────┬────────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│status2::Integer│
├──────────┼────────┼───────────┼───────────────┼────────────────┤
│"Paris"   │"S2"    │"Jones"    │10             │20              │
│"Athens"  │"S5"    │"Adams"    │30             │40              │
│"London"  │"S4"    │"Clark"    │20             │30              │
│"Paris"   │"S3"    │"Blake"    │30             │40              │
│"London"  │"S1"    │"Smith"    │20             │30              │
└──────────┴────────┴───────────┴───────────────┴────────────────┘
```

The "@" is required in the above query in order to distinguish the attribute's name from a relation's name.

Supported atom functions include:

|Function|Purpose|
|--------|-------|
|add(int,int)|Return the sum of two integers.|
|not(bool)|Invert a boolean expression.|
|lt(int,int)|Returns boolean true atom if he first argument is less than the second argument.|
|lte(int,int)|Returns boolean true atom if he first argument is less than or equal to the second argument.|
|gt(int,int)|Returns boolean true atom if he first argument is greater than the second argument.|
|gte(int,int)|Returns boolean true atom if he first argument is greater than or equal to the second argument.|

#### Group

The unary group operator groups the argument attributes into subrelations for each ungrouped set of attributes. SQL does not support tables as values, though SQL does support a notion of grouping by equal values. However, SQL's "GROUP BY" relies on tuple ordering in a table. A relations' tuples are never ordered.

Grouping is useful for aggregate operations (see below) or for summarizing data against a set of attributes; for example, "display all employees grouped by boss name".

The group operator can also be used to emulate functionality similar to SQL's OUTER JOIN but without relying on overloading the meaning of NULL.

```
TutorialD (master/main): :showexpr s group ({sname,status,s#} as subrel)
┌──────────┬───────────────────────────────────────────────────────┐
│city::Text│subrel::relation {s#::Text,sname::Text,status::Integer}│
├──────────┼───────────────────────────────────────────────────────┤
│"Athens"  │┌────────┬───────────┬───────────────┐                 │
│          ││s#::Text│sname::Text│status::Integer│                 │
│          │├────────┼───────────┼───────────────┤                 │
│          ││"S5"    │"Adams"    │30             │                 │
│          │└────────┴───────────┴───────────────┘                 │
│"Paris"   │┌────────┬───────────┬───────────────┐                 │
│          ││s#::Text│sname::Text│status::Integer│                 │
│          │├────────┼───────────┼───────────────┤                 │
│          ││"S2"    │"Jones"    │10             │                 │
│          ││"S3"    │"Blake"    │30             │                 │
│          │└────────┴───────────┴───────────────┘                 │
│"London"  │┌────────┬───────────┬───────────────┐                 │
│          ││s#::Text│sname::Text│status::Integer│                 │
│          │├────────┼───────────┼───────────────┤                 │
│          ││"S4"    │"Clark"    │20             │                 │
│          ││"S1"    │"Smith"    │20             │                 │
│          │└────────┴───────────┴───────────────┘                 │
└──────────┴───────────────────────────────────────────────────────┘
```

#### Ungroup

The unary ungroup operator "unwraps" subrelations in a relation-valued attribute. There is no equivalent in SQL.

```
TutorialD (master/main): :showexpr (s group ({sname,status,s#} as subrel)) ungroup subrel
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Paris"   │"S2"    │"Jones"    │10             │
│"Paris"   │"S3"    │"Blake"    │30             │
│"London"  │"S4"    │"Clark"    │20             │
│"London"  │"S1"    │"Smith"    │20             │
│"Athens"  │"S5"    │"Adams"    │30             │
└──────────┴────────┴───────────┴───────────────┘
```

#### Minus

The binary minus operator takes two relation variables of the same type and returns the tuples which appear in the first relation variable but not in the second relation.

```
TutorialD (master/main): x:=relation{tuple{name "Steve"},tuple{name "Bob"},tuple{name "Jim"},tuple{name "Bart"}}
TutorialD (master/main): y:=relation{tuple{name "Jim"},tuple{name "Bart"}}
TutorialD (master/main): :showexpr x minus y
┌──────────┐
│name::Text│
├──────────┤
│"Steve"   │
│"Bob"     │
└──────────┘
```


## State Operators

While relational operators compose to relational expressions representing queries of the database, state operators change the state of the database.

### Relation Variable State Changes

#### Define and Assign

The relation variable assignment mentioned above certainly changes the state of the database.

```
TutorialD (master/main): newrelvar:=relation{tuple{age 3}}
TutorialD (master/main): :showexpr newrelvar
┌────────────┐
│age::Integer│
├────────────┤
│3           │
└────────────┘
```

All further operators are convenience operators and could be implemented with simple assignment. SQL has no equivalent to relational assignment; instead, one must issue "CREATE TABLE" commands and insert rows into the resultant tables.

#### Undefine

To signal that a relation variable should be forgotten, undefine it.

```
TutorialD (master/main): undefine s
TutorialD (master/main): :showexpr s
ERR: RelVarNotDefinedError "s"
```

#### Insert

The insert operators accepts a relation variable and a relation of the same type (a relation having an identical header), and replaces the relation referenced by the relation variable with the union of the previous relation value and the argument's relation value.

```
TutorialD (master/main): insert s relation{tuple{city "Boston",s# "S10",sname "Gonzalez",status 10}}
TutorialD (master/main): :showexpr s
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Athens"  │"S5"    │"Adams"    │30             │
│"Paris"   │"S2"    │"Jones"    │10             │
│"Boston"  │"S10"   │"Gonzalez" │10             │
│"London"  │"S4"    │"Clark"    │20             │
│"London"  │"S1"    │"Smith"    │20             │
│"Paris"   │"S3"    │"Blake"    │30             │
└──────────┴────────┴───────────┴───────────────┘
```

This insertion operation is equivalent to:

```
s:=s union relation{tuple{city "Boston",s# "S10",sname "Gonzalez",status 10}}
```
#### Update

The update operator accepts a relation argument and a predicate, optionally filters and creates new tuples based on the original relation.

```
TutorialD (master/main): update s where status=20 (sname:="Mr. Twenty")
TutorialD (master/main): :showexpr s
┌──────────┬────────┬────────────┬───────────────┐
│city::Text│s#::Text│sname::Text │status::Integer│
├──────────┼────────┼────────────┼───────────────┤
│"Paris"   │"S2"    │"Jones"     │10             │
│"London"  │"S1"    │"Mr. Twenty"│20             │
│"Paris"   │"S3"    │"Blake"     │30             │
│"London"  │"S4"    │"Mr. Twenty"│20             │
│"Athens"  │"S5"    │"Adams"     │30             │
└──────────┴────────┴────────────┴───────────────┘
```

This is logically equivalent to:

```
TutorialD (master/main): s:=(((s where status=20){city,s#,status}):{sname:="Mr. Twenty"}) union (s where not status=20)
TutorialD (master/main): :showexpr s
┌──────────┬────────┬────────────┬───────────────┐
│city::Text│s#::Text│sname::Text │status::Integer│
├──────────┼────────┼────────────┼───────────────┤
│"Paris"   │"S2"    │"Jones"     │10             │
│"London"  │"S1"    │"Mr. Twenty"│20             │
│"Paris"   │"S3"    │"Blake"     │30             │
│"London"  │"S4"    │"Mr. Twenty"│20             │
│"Athens"  │"S5"    │"Adams"     │30             │
└──────────┴────────┴────────────┴───────────────┘
```

This query means: "Drop the sname attribute from S filtered where status equals 20, extend the resultant relation by sname with value 'Mr. Twenty', then union the resultant relation with S filtered by status not equaling 20, and assign that to S."

#### Delete

The delete operator accepts a relation argument and a filtering predicate, creates a new relation with only the tuples which do not match the predicate, and stores the result in the relation variable argument.

```
TutorialD (master/main): delete sp where s#="S4"
TutorialD (master/main): :showexpr sp
┌────────┬────────────┬────────┐
│p#::Text│qty::Integer│s#::Text│
├────────┼────────────┼────────┤
│"P2"    │200         │"S3"    │
│"P2"    │400         │"S2"    │
│"P1"    │300         │"S1"    │
│"P5"    │100         │"S1"    │
│"P4"    │200         │"S1"    │
│"P1"    │300         │"S2"    │
│"P3"    │400         │"S1"    │
│"P2"    │200         │"S1"    │
│"P6"    │100         │"S1"    │
└────────┴────────────┴────────┘
```

This is logically equivalent to:

```
TutorialD (master/main): sp:=sp where not s#="S4"
```

Note the inverted predicate.

## Constraints

Database constraints are user-specified relational algebra predicates which must be true at all times. If a database expression attempts to violate a database constraint, then the DBMS must reject the expression and ensure that the previous, non-violating state remains.

Chris Date identified that all constraints can be represented as inclusion dependencies, but Project:M36 offers some convenience operators.

#### Uniqueness Constraints

Uniqueness constraints ensure that a set of attributes' values are unique throughout a relational expression. For example, it is useful to ensure that all products' identifiers are unique. The equivalent in SQL is the UNIQUE expression found in table creation.

```
TutorialD (master/main): key s_key_constraint {s#} s
```

This expression creates a constraint named "S_key_constraint" and ensure that the values of the "s#" attribute are unique in the "S" relational expression.

#### Foreign Key Constraints

Foreign keys are constraints which require that certain values appearing in one relation variable must appear in another. The equivalent in SQL is the FOREIGN KEY() construction in table creation.

```
TutorialD (master/main): foreign key s#_in_sp sp{s#} in s{s#}
```

This expression ensure that any sp{s#} must also appear as a value in s{s#}. Note that the sub-expression (on the left of "in") and the super-expression (on the right) must be of the same relation type- the generated relation values' attribute names and types must be equal. Use `rename` to make attribute names identical.

#### Functional dependencies

Unlike SQL databases, Project:M36 includes first-class support for functional dependencies. A functional dependency for an attribute set ```A``` to attribute set ```B``` ensures that for every unique appearance of a value for ```A``` there is only one possible value for ```B```.

To create a functional dependency:

```
TutorialD (master/main): funcdep sname_status (sname) -> (status) s
```

In this example, the functional dependency is named "sname_status" and creates a constraint which ensures that the "sname" value is functionally determines "status" value in the relation variable "s", though the final argument can be any relational expression. Multiple attribute names are separated by a comma.

#### Other Constraints

All other constraints can be represented by a set of inclusion dependencies.

```
TutorialD (master/main): constraint s_status_less_than_50 (s where ^lt(50,@status)){} in false
```

This expression ensures that the relation variable's relation can never include a tuple with status greater than 50.

Internally, *all* constraints are converted and stored as inclusion dependencies.

## Aggregate Queries

Aggregate queries in TutorialD are able to provide much more information than SQL queries due to the support for relation-valued attributes. For example, a single query can return aggregate results alongside its constituent tuples.

SQL supports aggregate queries using aggregate functions (SUM(), COUNT(), AVERAGE(), etc.) and GROUP BY while the aggregate functions in the relational algebra simply accept relation values as arguments.

```
TutorialD (master/main): :showexpr s group ({s#,sname,status} as subrel):{citycount:=count(@subrel)}
┌──────────┬──────────────────┬───────────────────────────────────────────────────────┐
│city::Text│citycount::Integer│subrel::relation {s#::Text,sname::Text,status::Integer}│
├──────────┼──────────────────┼───────────────────────────────────────────────────────┤
│"Athens"  │1                 │┌────────┬───────────┬───────────────┐                 │
│          │                  ││s#::Text│sname::Text│status::Integer│                 │
│          │                  │├────────┼───────────┼───────────────┤                 │
│          │                  ││"S5"    │"Adams"    │30             │                 │
│          │                  │└────────┴───────────┴───────────────┘                 │
│"Paris"   │2                 │┌────────┬───────────┬───────────────┐                 │
│          │                  ││s#::Text│sname::Text│status::Integer│                 │
│          │                  │├────────┼───────────┼───────────────┤                 │
│          │                  ││"S2"    │"Jones"    │10             │                 │
│          │                  ││"S3"    │"Blake"    │30             │                 │
│          │                  │└────────┴───────────┴───────────────┘                 │
│"London"  │2                 │┌────────┬───────────┬───────────────┐                 │
│          │                  ││s#::Text│sname::Text│status::Integer│                 │
│          │                  │├────────┼───────────┼───────────────┤                 │
│          │                  ││"S4"    │"Clark"    │20             │                 │
│          │                  ││"S1"    │"Smith"    │20             │                 │
│          │                  │└────────┴───────────┴───────────────┘                 │
└──────────┴──────────────────┴───────────────────────────────────────────────────────┘
```

In this query result, we can simultaneously answer how many suppliers are located in each city and which suppliers they are. The expression is constructed by first grouping to isolate the city, then extending the query to add a attribute containing the tuple count for each subrelation.

|Function Name|Description|
|-------------|-----------|
|count(relation{any tuple types})|return the number of tuples in each relation value|
|sum(relation{int})|return the int sum of the relation's values|
|max(relation{int})|return the maximum int from all the relation's values|
|min(relation{int})|return the minimum int from all the relation's values|

#### Arbitrary Relation Variables

For testing purposes, it can be useful to populate a database with randomly generated data. Project:M36 can automatically create such data based solely on the type of the relation.

```
TutorialD (master/main): createarbitraryrelation employee {name Text, empid Int, hired DateTime} 3-100
TutorialD (master/main): :showexpr employee
┌──────────┬───────────────────────┬──────────┐
│empid::Int│hired::DateTime        │name::Text│
├──────────┼───────────────────────┼──────────┤
│23        │1858-11-04 10:50:14 UTC│"LLL"     │
│22        │1858-12-10 14:53:37 UTC│"LLL"     │
│25        │1858-11-07 01:14:15 UTC│"SSS"     │
│-12       │1858-11-06 06:20:57 UTC│"VVV"     │
│-6        │1858-11-01 16:26:38 UTC│"SSS"     │
│-1        │1858-10-31 14:41:26 UTC│"DDD"     │
│-15       │1858-11-26 04:14:03 UTC│"VVV"     │
│-18       │1858-12-17 00:01:09 UTC│"AAA"     │
│-9        │1858-10-31 08:36:53 UTC│"XXX"     │
│4         │1858-11-23 01:48:04 UTC│"VVV"     │
└──────────┴───────────────────────┴──────────┘
```

The `createarbitraryrelation` syntax specifies the name of the relation variable, the type for the relation, and then an acceptable range for the tuple count.
