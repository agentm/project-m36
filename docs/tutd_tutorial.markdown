# TutorialD Tutorial for Project:M36

TutorialD is an logic language designed by Chris Date for interacting with any relational algebra engine. Project:M36 supports this language for the purposes of learning about the relational algebra. TutorialD is not recommended for use in production systems.

It is presumed that the audience for this tutorial has at least some minimal background with an SQL-based DBMS.

### Starting the TutorialD Interpreter

From the project-m36 source directory, execute ```cabal run tutd``` to start the interactive TutorialD interpreter. You will be greeted by the interactive prompt:

```
TutorialD (master):
```

"master" refers to the name of the branch to which the current transaction will be potentially committing.

### Types

TutorialD is strongly-typed. The basic built-in types are:

|Type Name|Explanation|Example|
|---------|-----------|-------|
|char|arbitrary text|"The Old Man and the Sea"|
|int|any integer|-4|
|datetime|timestamp UTC|"2015-02-02 04:05:02"::datetime|
|double|floating point number|3.1459|
|bool|boolean value|t|
|bytestring|arbitrary-length string of bytes- input is base64-encoded|"dGVzdGRhdGE="::bytestring|

With regards to boolean values, be sure not to conflate ```t``` or ```f``` as a boolean value with ```true``` and ```false``` which are relation variables.

Project:M36 will complain loudly if the expected types do not match. Automatic type coercion does not exist.

```
TutorialD (master): :showexpr S:{more:=add(10,@SNAME)}
ERR: AtomFunctionTypeError "add" 2 IntAtomType StringAtomType
```

The integer "10" cannot be added to the string SNAME value.

### Using Relation Variables

Relation Variables are named cells which reference relations within a transaction state. The relation can be replaced with another relation through assignment.

Definition of a relation variable:
```
products :: {name char, age int}
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

### Date's Sample Relations

The examples in this tutorial can be executed after loading Chris Date's sample relations using:

```
:importtutd "scripts/DateExamples.tutd"
```

Each section assumes a fresh instance of the Date examples. The relation variables created in the script are:

|Relation Variable Name|Description|
|----------------------|-----------|
|S|suppliers|
|P|parts|
|SP|contains mapping of which suppliers supply which parts|

### Relational Expressions

A relational expression combines relational operators to create a new relation. ```tutd``` can display the result of executing a relational expression with ":showexpr". 

```
TutorialD (master): :showexpr P
┌──────┬─────┬──┬─────┬──────┐
│CITY  │COLOR│P#│PNAME│WEIGHT│
├──────┼─────┼──┼─────┼──────┤
│London│Red  │P6│Cog  │19    │
│Paris │Blue │P5│Cam  │12    │
│London│Red  │P1│Nut  │12    │
│London│Red  │P4│Screw│14    │
│Oslo  │Blue │P3│Screw│17    │
│Paris │Green│P2│Bolt │17    │
└──────┴─────┴──┴─────┴──────┘
```

### Using Relational Operators

Relational operators generate new relations from existing relations. The operators form the closed algebra against relations. 

#### Rename

The unary rename operator outputs a new relation with chosen attributes renamed. In SQL, the equivalent is to use "AS" in the column name list of SELECTS.

```
TutorialD (master): :showexpr S rename {CITY as TOWN}
┌──┬─────┬──────┬──────┐
│S#│SNAME│STATUS│TOWN  │
├──┼─────┼──────┼──────┤
│S3│Blake│30    │Paris │
│S4│Clark│20    │London│
│S5│Adams│30    │Athens│
│S1│Smith│20    │London│
│S2│Jones│10    │Paris │
└──┴─────┴──────┴──────┘
```

#### Projection

Projection is applied by adding curly braces and attribute names following a relational expression. Projection in SQL is applied by adding a list of column names to a SELECT statement.

For example:

```
TutorialD (master): :showexpr P{COLOR,CITY}
┌──────┬─────┐
│CITY  │COLOR│
├──────┼─────┤
│London│Red  │
│Paris │Blue │
│Oslo  │Blue │
│Paris │Green│
└──────┴─────┘
```

#### Restriction

Restriction is applied using a "where" clause, much like in SQL.

```
TutorialD (master): :showexpr P where COLOR="Blue" and CITY="Paris"
┌─────┬─────┬──┬─────┬──────┐
│CITY │COLOR│P#│PNAME│WEIGHT│
├─────┼─────┼──┼─────┼──────┤
│Paris│Blue │P5│Cam  │12    │
└─────┴─────┴──┴─────┴──────┘
```

The restriction predicate can be built from "and", "not", and "or". Boolean atom functions can also appear in a restriction as long as they are preceded by "^". For example:

```
TutorialD (master): :showexpr S where ^lt(@STATUS,20)
┌─────┬──┬─────┬──────┐
│CITY │S#│SNAME│STATUS│
├─────┼──┼─────┼──────┤
│Paris│S2│Jones│10    │
└─────┴──┴─────┴──────┘
```

#### Join

Joins are binary operators and are applied with the "join" keyword. The equivalent in SQL would be a "NATURAL JOIN". To join attributes which are not identically-named, use the rename operator

```
TutorialD (master): :showexpr S join SP
┌──────┬──┬───┬──┬─────┬──────┐
│CITY  │P#│QTY│S#│SNAME│STATUS│
├──────┼──┼───┼──┼─────┼──────┤
│London│P6│100│S1│Smith│20    │
│London│P3│400│S1│Smith│20    │
│London│P5│400│S4│Clark│20    │
│London│P1│300│S1│Smith│20    │
│Paris │P2│200│S3│Blake│30    │
│Paris │P1│300│S2│Jones│10    │
│London│P5│100│S1│Smith│20    │
│London│P4│300│S4│Clark│20    │
│Paris │P2│400│S2│Jones│10    │
│London│P2│200│S1│Smith│20    │
│London│P4│200│S1│Smith│20    │
└──────┴──┴───┴──┴─────┴──────┘
```

#### Union

The binary union operator merges tuples from both relations if-and-only-if the attributes of both relations are identical. The SQL equivalent is the "UNION" operator. Unlike SQL, however, duplicate tuples are not tolerated, so there is no "UNION ALL" equivalent in the relational algebra.

```
TutorialD (master): :showexpr S union S
┌──────┬──┬─────┬──────┐
│CITY  │S#│SNAME│STATUS│
├──────┼──┼─────┼──────┤
│Paris │S3│Blake│30    │
│London│S4│Clark│20    │
│Athens│S5│Adams│30    │
│London│S1│Smith│20    │
│Paris │S2│Jones│10    │
└──────┴──┴─────┴──────┘
```

The union of any relation with itself is itself.

#### Extend

The extend unary operator adds an attribute to a relation's header and body and is represented by a colon ":". The equivalent in SQL is a subselect in a SELECT column list. Typically, extend is used to add information derived from relation.

```
TutorialD (master): :showexpr S:{STATUS2:=add(10,@STATUS)}
┌──────┬──┬─────┬──────┬───────┐
│CITY  │S#│SNAME│STATUS│STATUS2│
├──────┼──┼─────┼──────┼───────┤
│Paris │S2│Jones│10    │20     │
│Athens│S5│Adams│30    │40     │
│Paris │S3│Blake│30    │40     │
│London│S1│Smith│20    │30     │
│London│S4│Clark│20    │30     │
└──────┴──┴─────┴──────┴───────┘
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

```
TutorialD (master): :showexpr S group ({SNAME,STATUS,S#} as subrel)
┌──────┬─────────────────┐
│CITY  │subrel           │
├──────┼─────────────────┤
│Athens│┌──┬─────┬──────┐│
│      ││S#│SNAME│STATUS││
│      │├──┼─────┼──────┤│
│      ││S5│Adams│30    ││
│      │└──┴─────┴──────┘│
│London│┌──┬─────┬──────┐│
│      ││S#│SNAME│STATUS││
│      │├──┼─────┼──────┤│
│      ││S1│Smith│20    ││
│      ││S4│Clark│20    ││
│      │└──┴─────┴──────┘│
│Paris │┌──┬─────┬──────┐│
│      ││S#│SNAME│STATUS││
│      │├──┼─────┼──────┤│
│      ││S3│Blake│30    ││
│      ││S2│Jones│10    ││
│      │└──┴─────┴──────┘│
└──────┴─────────────────┘
```

#### Ungroup

The unary ungroup operator "unwraps" subrelations in a relation-valued attribute. There is no equivalent in SQL.

```
TutorialD (master): :showexpr (S group ({SNAME,STATUS,S#} as subrel)) ungroup subrel
┌──────┬──┬─────┬──────┐
│CITY  │S#│SNAME│STATUS│
├──────┼──┼─────┼──────┤
│Paris │S3│Blake│30    │
│London│S4│Clark│20    │
│Athens│S5│Adams│30    │
│London│S1│Smith│20    │
│Paris │S2│Jones│10    │
└──────┴──┴─────┴──────┘
```

## State Operators

While relational operators compose to relational expressions representing queries of the database, state operators change the state of the database.

### Relation Variable State Changes

#### Define and Assign

The relation assignment mentioned above certainly changes the state of the database.

```
TutorialD (master): newrel:=relation{tuple{age 3}}
TutorialD (master): :showexpr newrel
┌───┐
│age│
├───┤
│3  │
└───┘
```

All further operators are convenience operators and could be implemented with simple assignment. SQL has no equivalent to relational assignment; instead, one must issue "CREATE TABLE" commands and insert rows into the resultant tables.

#### Undefine

To signal that a relation variable should be forgotten, undefine it.

```
TutorialD (master): undefine S
TutorialD (master): :showexpr S
ERR: RelVarNotDefinedError "S"
```

#### Insert

The insert operators accepts a relation variable and a relation of the same type (a relation having an identical header), and replaces the relation referenced by the relation variable with the union of the previous relation value and the argument's relation value.

```
TutorialD (master): insert S relation{tuple{CITY "Boston",S# "S10",SNAME "Gonzalez",STATUS 10}}
TutorialD (master): :showexpr S
┌──────┬───┬────────┬──────┐
│CITY  │S# │SNAME   │STATUS│
├──────┼───┼────────┼──────┤
│Paris │S3 │Blake   │30    │
│London│S4 │Clark   │20    │
│Athens│S5 │Adams   │30    │
│London│S1 │Smith   │20    │
│Paris │S2 │Jones   │10    │
│Boston│S10│Gonzalez│10    │
└──────┴───┴────────┴──────┘
```

This insertion operation is equivalent to:

```
S:=S union relation{tuple{CITY "Boston",S# "S10",SNAME "Gonzalez",STATUS 10}}
```
#### Update

The update operator accepts a relation argument and a predicate, optionally filters and creates new tuples based on the original relation.

```
TutorialD (master): update S where STATUS=20 (SNAME:="Mr. Twenty")
TutorialD (master): :showexpr S
┌──────┬──┬──────────┬──────┐
│CITY  │S#│SNAME     │STATUS│
├──────┼──┼──────────┼──────┤
│Paris │S3│Blake     │30    │
│London│S1│Mr. Twenty│20    │
│Athens│S5│Adams     │30    │
│Paris │S2│Jones     │10    │
│London│S4│Mr. Twenty│20    │
└──────┴──┴──────────┴──────┘
```

This is logically equivalent to:

```
TutorialD (master): S:=(((S where STATUS=20){CITY,S#,STATUS}):{SNAME:="Mr. Twenty"}) union (S where not STATUS=20)
TutorialD (master): :showexpr S
┌──────┬──┬──────────┬──────┐
│CITY  │S#│SNAME     │STATUS│
├──────┼──┼──────────┼──────┤
│Paris │S3│Blake     │30    │
│London│S1│Mr. Twenty│20    │
│Athens│S5│Adams     │30    │
│Paris │S2│Jones     │10    │
│London│S4│Mr. Twenty│20    │
└──────┴──┴──────────┴──────┘
```

This query means: "Drop the SNAME attribute from S filtered where STATUS equals 20, extend the resultant relation by SNAME with value 'Mr. Twenty', then union the resultant relation with S filtered by STATUS not equaling 20, and assign that to S."

#### Delete

The delete operator accepts a relation argument and a filtering predicate, creates a new relation with only the tuples which do not match the predicate, and stores the result in the relation variable argument.

```
TutorialD (master): delete SP where S#="S4"
TutorialD (master): :showexpr SP
┌──┬───┬──┐
│P#│QTY│S#│
├──┼───┼──┤
│P3│400│S1│
│P4│200│S1│
│P2│400│S2│
│P2│200│S1│
│P6│100│S1│
│P1│300│S1│
│P2│200│S3│
│P1│300│S2│
│P5│100│S1│
└──┴───┴──┘
```

This is logically equivalent to:

```
TutorialD (master): SP:=SP where not S#="S4"
```

Note the inverted predicate.

## Constraints

Database constraints are user-specified relational algebra predicates which must be true at all times. If a database expression attempts to violate a database constraint, then the DBMS must reject the expression and ensure that the previous, non-violating state remains.

Chris Date identified that all constraints can be represented as inclusion dependencies, but Project:M36 offers some convenience operators.

#### Uniqueness Constraints

Uniqueness constraints ensure that a set of attributes' values are unique throughout a relational expression. For example, it is useful to ensure that all products' identifiers are unique. The equivalent in SQL is the UNIQUE expression found in table creation.

```
TutorialD (master): key S_key_constraint {S#} S
```

This expression creates a constraint named "S_key_constraint" and ensure that the values of the "S#" attribute are unique in the "S" relational expression.

#### Foreign Key Constraints

Foreign keys are constraints which require that certain values appearing in one relation variable must appear in another. The equivalent in SQL is the FOREIGN KEY() construction in table creation.

```
TutorialD (master): foreign key S#_in_SP SP{S#} in S{S#}
```

This expression ensure that any SP{S#} must also appear as a value in S{S#}.

#### Other Constraints

All other constraints can be represented by a set of inclusion dependencies.

```
TutorialD (master): constraint S_status_less_than_50 (S where ^lt(50,@STATUS)){} in false
```

This expression ensures that the relation variable's relation can never include a tuple with STATUS greater than 50.

## Aggregate Queries

Aggregate queries in TutorialD are able to provide much more information than SQL queries due to the support for relation-valued attributes. For example, a single query can return aggregate results alongside its constituent tuples. 

SQL supports aggregate queries using aggregate functions (SUM(), COUNT(), AVERAGE(), etc.) and GROUP BY while the aggregate functions in the relational algebra simply accept relation values as arguments. 

```
TutorialD (master): :showexpr S group ({S#,SNAME,STATUS} as subrel):{citycount:=count(@subrel)}
┌──────┬─────────┬─────────────────┐
│CITY  │citycount│subrel           │
├──────┼─────────┼─────────────────┤
│Paris │2        │┌──┬─────┬──────┐│
│      │         ││S#│SNAME│STATUS││
│      │         │├──┼─────┼──────┤│
│      │         ││S3│Blake│30    ││
│      │         ││S2│Jones│10    ││
│      │         │└──┴─────┴──────┘│
│London│2        │┌──┬─────┬──────┐│
│      │         ││S#│SNAME│STATUS││
│      │         │├──┼─────┼──────┤│
│      │         ││S1│Smith│20    ││
│      │         ││S4│Clark│20    ││
│      │         │└──┴─────┴──────┘│
│Athens│1        │┌──┬─────┬──────┐│
│      │         ││S#│SNAME│STATUS││
│      │         │├──┼─────┼──────┤│
│      │         ││S5│Adams│30    ││
│      │         │└──┴─────┴──────┘│
└──────┴─────────┴─────────────────┘
```

In this query result, we can simultaneously answer how many suppliers are located in each city and which suppliers they are. The expression is constructed by first grouping to isolate the city, then extending the query to add a attribute containing the tuple count for each subrelation.

|Function Name|Description|
|-------------|-----------|
|count(relation{any tuple types})|return the number of tuples in each relation value|
|sum(relation{int})|return the int sum of the relation's values|
|max(relation{int})|return the maximum int from all the relation's values|
|min(relation{int})|return the minimum int from all the relation's values|