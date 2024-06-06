# Why SQLegacy?

### Prelude

Project:M36 is a complete implementation of the relational algebra. However, the most popular database interaction language, SQL, diverges significantly from the relational algebra for historical reasons. This document examines these divergencies and explains the motivation for Project:M36 to implement SQL but also offer an alternative: TutorialD.

## Introduction

The benefits of sticking to the mathematics of the relational algebra are many:

* any possible data modeling the world's state can be normalized and retained
* any possible query against such data can be represented and processed
* the data is independent of any specific format
* queries are independent of any execution means

### On the Value of Having Options

The advantage of having no specific behavior proscribed by the math of the relational algebra enables database management systems to choose from an ever-increasing swath of technologies, especially as hardware and algorithms develop, but without affecting queries and data manipulation.

Conversely, we *can* lock ourselves into specific implementations and data arrangements and formats such as with key-value databases. The cost, however, is that we can only make specific queries on a specific data arrangement (keys) and the algorithms are often tuned for specific hardware. As a business reliant on a key-value database evolves, the brittleness of the database becomes apparent, made especially obvious if the database cannot reasonably answer queries that the developers and database did not anticipate. Oops!

The solution is to rely on an algebra designed for complete data management which naturally implies permanent and unchanging APIs to access the data. Math does not change. The hardware and software limitations practitioners face can and should be pushed down and away from the top-level database access use-cases.

While SQL, as a declarative language, does better on the "options" axis than key-value databases, it still fails the mathematical cohesiveness test. We will examine these failings in this document.

### Project:M36+TutorialD vs. SQL

Project:M36 is a ground-up reimaginging of what a relational algebra engine could be. It was born out of the frustrations of dealing with SQL so it aims to avoid the pitfalls of SQL. However, any serious, production DBMS nowadays supports SQL out of necessity- Project:M36 is no exception. Deliberate care was taken to ensure that SQL is bolted on top of a relational algebra core. To achieve this, a database interaction language apart from SQL was required: TutorialD. TutorialD has been developed over decades by database expert C.J. Date, an author who also recognized the irreconciable flaws of SQL.

Project:M36's SQL shim over a relational algebra core makes SQL flaws more obvious through comparative implementation. Unfortunately, it is not even true that SQL supports a strict subset of the relational algebra.

Fortunately, Project:M36 implements TutorialD: an interactive console language which unlocks the full power of the relational algebra.

## SQL Deviations from the Relational Algebra

The following is non-exhaustive list of SQL deviations from the relational algebra. For the purposes of demonstration, PostgreSQL is used a reference SQL implementation. Wikipedia can be used as the [definition of the relational algebra](https://en.wikipedia.org/wiki/Relational_algebra), if necessary.

### Duplicate Rows vs. Set of Tuples

The relational algebra defines a relation as a set of attributes (header) with a set of tuples (body) mapping values to the header's attributes. SQL intentionally fails to implement this constraint.

```SQL
create table employee as select * from (values (1,'Steve'),(1,'Steve')) as x(id,name);
SELECT 2

table employee;
 id | name  
----+-------
  1 | Steve
  1 | Steve
```

If we consider each row as a proposition modeling the state of the world, what does it mean to say truth twice? It's not only meaningless, it actively harmful as developers have to contend with the possibility of receiving duplicate rows in query results. Thus SQL queries are peppered with `DISTINCT` and `GROUP BY` even though the natural language query assumes that the result would be naturally distinct:

```
select distinct * from employee;
 id | name  
----+-------
  1 | Steve
```

Still, the underlying source of data (the table) is ambiguous: do we have two different Steves with the same employee id or one Steve mentioned twice? It's not clear and ambiguity in modeling the world is undesired, stated mildly. 

One could pose the counterargument that the database designer is responsible for eliminating such ambiguity, but then why does the database enable ambiguity at all?

Could we force SQL to enforce sets of tuples? Yes, but only on base tables using constraints. Queries can still generate arbitrary duplicates.

The relational algebra demands that a relation contain a set of tuples, but is there a penalty for diverging? Yes. Consider the following query:

```SQL  
SELECT x.a FROM x JOIN (SELECT DISTINCT a FROM y) as y2 ON x.a = y2.a;
```

A simple query planner has two options: it can execute the join first or the projection on `x.a` first. However, SQL allows column `a` in either table to contain duplicates and only the JOINed table is deduplicated. So, the execution engine *must* run the deduplicating join execution step first! (See "[23.2.3 Ordering of Relational Operators](https://dl.acm.org/doi/pdf/10.5555/77708.C1065772)") Pushing projections to run first can often reduce the amount of data being pushed between execution steps- that cannot happen here. Thus, "allowing" duplicate rows in relations reduces the relational expression optimization space.

### Inconsistent Aggregations vs. Folds

One of the benefits of math is its context-independent consistency. Developers rely on this fact, even unknowingly. For example, no developer expects a `sine` function to return different values depending on its calling function. Consistent function operation enables a developer to reason about his program and avoid bugs. 

SQL aggregations are unfortunately not consistent. 

```
create table agg as select * from (values (1),(1),(2),(NULL)) as x(a);
table agg;
 a 
---
 1
 1
 2
  
(4 rows)

```

How many rows are there in the table?

```
SELECT COUNT(*) FROM agg;
 count 
-------
     4
(1 row)
```

Ok, but how many non-null values of `a` are there?

```
SELECT count(a) from agg;
 count 
-------
     3
(1 row)
```

Oh, ok. But how many distinct values of `a` are there?

```
SELECT count(distinct a) from agg;
 count 
-------
     2
(1 row)
```

Wait. Is NULL not considered a distinct value? How many distinct values including NULL are there?

```
SELECT COUNT(*) FROM (SELECT DISTINCT a FROM agg) as x;
 count 
-------
     3
(1 row)
```

So, NULL is DISTINCT in a projection but ignored by `COUNT(a)`? If your head is spinning, you are not alone. The mix of aggregations ignoring NULL and handling duplicates is difficult to reason about. In the relational algebra, none of these issues arise- ternary logic (logic involving NULL- discussed later) is not required and aggregations are completely predictable.

```
TutorialD (master/main): agg:=relation{tuple{a 1},tuple{a 1},tuple{a 2}}
TutorialD (master/main): :showexpr agg
┌──────────┐
│a::Integer│
├──────────┤
│2         │
│1         │
└──────────┘
TutorialD (master/main): :showexpr agg group({a} as g):{c:=count(@g)}
┌──────────┬────────────────────────┐
│c::Integer│g::relation {a::Integer}│
├──────────┼────────────────────────┤
│2         │┌──────────┐            │
│          ││a::Integer│            │
│          │├──────────┤            │
│          ││1         │            │
│          ││2         │            │
│          │└──────────┘            │
└──────────┴────────────────────────┘
```
Yes, that is a relation as a value. Nested relations (discussed below) are a critical part of the relational algebra, but not supported in SQL.

Writing predictable and correct queries doesn't need to be difficult. We just need to follow the math.

### Lossy Results in SQL Aggregates vs. Nested Relations

The power and promise of relational algebra is to be able to pose any query we could possible desire on the data. The algebra does this by providing composable building blocks (functions) to process and alter the data in a set of relations. However, SQL falls flat here, too. Consider the following query:

Show me the employees' average salary alongside their salaries.

Oops, in SQL, we have to fake this using non-standard array aggregations or by writing two queries.

```
table employee;
 name  | salary 
-------+--------
 Steve |  10000
 Bob   |  15000
 Maria |  17000
(3 rows)
select avg(salary) from employee;
        avg         
--------------------
 14000.000000000000
(1 row)
select avg(salary),array_agg(ROW(name,salary)) from employee;
        avg         |                    array_agg                    
--------------------+-------------------------------------------------
 14000.000000000000 | {"(Steve,10000)","(Bob,15000)","(Maria,17000)"}
(1 row)
```
Of course, with arrays, we have now left the relational algebra- suddenly the set of tuples has become a list of tuples; relational algebra operators cannot run on arrays, etc.

The relational algebra has this solved:

```
TutorialD (master/main): employee:=relation{tuple{name "Steve", salary 10000},tuple{name "Bob",salary 15000},tuple{name "Maria",salary 17000}}
TutorialD (master/main): :showexpr employee{salary} group({salary} as g):{avg:=mean(@g)}
┌────────────┬─────────────────────────────┐
│avg::Integer│g::relation {salary::Integer}│
├────────────┼─────────────────────────────┤
│14000       │┌───────────────┐            │
│            ││salary::Integer│            │
│            │├───────────────┤            │
│            ││10000          │            │
│            ││15000          │            │
│            ││17000          │            │
│            │└───────────────┘            │
└────────────┴─────────────────────────────┘
```

By leveraging nested relations, we can see both the mean and its constituent salaries.

### Unnamed Columns

SQL supports expressions like this:

```
SELECT 1+3;
 ?column? 
----------
        4
(1 row)
```

Clearly, no table is queried here, so a column-less table with one row is assumed (into which we can place our result). PostgreSQL is generous enough to generate a name "?column?" on our behalf, but how are clients supposed to access it? In fact, the column name is completely arbitrary:

```
test=# select 1+3 union select 4 as name;
 ?column? 
----------
        4
(1 row)
```

What about if we have two unnamed columns?

```
select 1+3,2+3;
 ?column? | ?column? 
----------+----------
        4 |        5
(1 row)
```

The relational algebra states simply that the names of the columns/attributes must exist and be unique. Otherwise, how can we be expected to execute further queries on these columns?

```
select ?? + 10 from (select 1+3) as x;
select a+10, b+11 from (select 1+3,2+3) as x(a,b);
 ?column? | ?column? 
----------+----------
       14 |       16
(1 row)

```

In this case, SQL forces us to name the columns to be able to use them. Why shouldn't it simply do that up-front?

### Duplicate Column Names

SQL allows column names to be repeated:

```
SELECY 1 AS a, 2 AS a;
 a | a 
---+---
 1 | 2
(1 row)

```

even in a subquery:

```
select * from (select 1 as a, 2 as a) as x;
 a | a 
---+---
 1 | 2

```

but then we're punished if we want to use the name:

```
select a from (select 1 as a, 2 as a) as x;
ERROR:  column reference "a" is ambiguous
LINE 1: select a from (select 1 as a, 2 as a) as x;
```

That means duplicate column names are footguns if we try to use the names. What is the point of this?

The relational algebra defines the header of a relation to be a set of attributes. Attributes cannot have duplicates, so there is no ambiguity possible.

### Projection vs. Extension

SQL allows us to define expressions like this:

`select a+4 as a from x;`

whereby each value of `a` in table `x` is added to four. The resultant column is also called `a`. However, this construction conflates two relational operators: extension and projection. 

Projection is used to trim columns from a table or other relational expression. Here is pure projection in SQL:

`select a from x;`

Extension is used to derive new values from values in a relation. Here is pure extension in SQL:

`select *,a+4 as new_val from x;`

By conflating the two relational operators, SQL creates ambiguity. For example, does the following SQL apply `abs()` before or after `DISTINCT`?

`select distinct abs(a) from (values (1),(-1)) as x(a);`

Because abs(a) is both projection and extension, it is not obvious that the `abs()` extension is applied before projection. When is the `DISTINCT` applied (it turns out it's applied last)? SQL certainly does not make any of this clear.

TutorialD makes projection and extension obvious and unambiguous:

```
TutorialD (master/main): :showexpr (relation{tuple{a 1,b 2},tuple{a -1,b 3}}:{a2:=abs(@a)}){a2}
┌───────────┐
│a2::Integer│
├───────────┤
│1          │
└───────────┘
```

First, the extension with `abs()` occurs, then we project on the new attribute's name. Done.

### NULL: A Billion Dollar Boondoggle

NULLs are absolutely not a component of the relational algebra. After all, the relational algebra does not require any specific sort of types. However, NULLs are so pervasive in SQL, they are worth mentioning in terms of how NULLs negatively affect query writing, query optimization, and query execution.

First and foremost, the presence of NULLs make it difficult for an SQL developer to feel confident about his queries. Consider:

```
SELECT AVG(age) FROM citizen;
```

If there is a NULL age, that does not affect the average. (AVG() ignores NULLs.) However, this "rule" only applies to aggregate functions. but aggregate functions are indistinguishable from standard, value-altering functions:

```
SELECT ABS(age) FROM citizen;
```

One cannot know by looking at the syntax that one query creates one row as an answer and the other creates as many rows as there are in `citizen`. In TutorialD, this is unambiguous. Aggregate function applications can only be applied to nested relations:

```
TutorialD (master/main): :importexample cjdate
TutorialD (master/main): :showexpr s
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Paris"   │"S2"    │"Jones"    │10             │
│"Paris"   │"S3"    │"Blake"    │30             │
│"London"  │"S4"    │"Clark"    │20             │
│"London"  │"S1"    │"Smith"    │20             │
│"Athens"  │"S5"    │"Adams"    │30             │
└──────────┴────────┴───────────┴───────────────┘
TutorialD (master/main): :showexpr s:{m:=mean(@status)}
ERR: AtomFunctionTypeError "mean" 1 (RelationAtomType (attributesFromList [(Attribute "_" IntegerAtomType)])) IntegerAtomType
```

which indicates a type mismatch error- an aggregate function can only operate on relations containing integers, not integers.

When used correctly, the aggregate function operates on a nested relation.
```
TutorialD (master/main): :showexpr (s{status} group({status} as s)):{m:=mean(@s)}
┌──────────┬─────────────────────────────┐
│m::Integer│s::relation {status::Integer}│
├──────────┼─────────────────────────────┤
│20        │┌───────────────┐            │
│          ││status::Integer│            │
│          │├───────────────┤            │
│          ││20             │            │
│          ││30             │            │
│          ││10             │            │
│          │└───────────────┘            │
└──────────┴─────────────────────────────┘
```

Next, NULLs intentionally introduce ambiguity. This is somewhat intentional, but consider if we have the very common case of representing missing data in a table:

```
CREATE TABLE dog(id SERIAL PRIMARY KEY, name TEXT NOT NULL, age INTEGER);
INSERT INTO dog(name,age) VALUES ('Sparky', NULL);
table dog;
 id |  name  | age 
----+--------+-----
  1 | Sparky |    
(1 row)

```

We create a table for a veterinary office with three columns, but only age can contain a NULL which we promptly insert with the first row. Now imagine you are developer who sees this table for the first time. What does a NULL `age` mean? Here are some options:

* the veterinarian forgot to ask for the dog's age
* the veterinarian forgot to type in the dog's age
* the dog's owner doesn't know the dog's age
* the dog's age was anonymized
* the dog's information has not yet been filled out
* the dog has not yet visited the office
* the dog is dead
* the dog was never born

There are likely more valid interpretations, yet ambiguity is a property of data we would wish to avoid in a database. Consider that some of the NULL states could result in different business logic; for example, the veterinarian should be reminded to ask for the dog's age at the next visit.

To disambiguate these options, a developer could add a new, explanatory column:

```
ALTER TABLE dog ADD COLUMN age_null_reason TEXT;
UPDATE dog SET age_null_reason='awaiting pet visit' WHERE id=1;
```

Now we can differentiate between the ambiguous NULLs. But we actually want `age_null_reason` to be `NOT NULL` only when `age IS NULL`, so we have to create a table constraint.

```
ALTER TABLE dog ADD CONSTRAINT age_reason_nullability check ((age IS NULL AND age_null_reason IS NOT NULL) OR (age IS NOT NULL AND age_null_reason IS NULL));
INSERT INTO dog(name,age,age_null_reason) VALUES ('Barky', NULL, NULL);
ERROR:  new row for relation "dog" violates check constraint "age_reason_nullability"
DETAIL:  Failing row contains (4, Barky, null, null).
```

But these are just workarounds for a faulty type system in SQL. What we really want is a type which can encompass all possible states of the dog's age. This is achievable with an algebraic data type.

```
TutorialD (master/main): data Age = AgeInYears Integer | VetShouldAskAge | OwnerDoesntKnowAge | UnknownAge Text
TutorialD (master/main): dog:=relation{id Integer, name Text, age Age}
TutorialD (master/main): insert dog relation{tuple{id 1, name "Sparky", age VetShouldAskAge}}
TutorialD (master/main): :showexpr dog
┌───────────────┬───────────┬──────────┐
│age::Age       │id::Integer│name::Text│
├───────────────┼───────────┼──────────┤
│VetShouldAskAge│1          │"Sparky"  │
└───────────────┴───────────┴──────────┘
```

Algebraic data types enable arbitrary type composition. In the above Age type, not only can we encode knowledge about specific, missing data, but we even include an open-ended missing value `UnknownAge` which is a catch-all for any other reason why the age may be missing from the database. As a side benefit, these types are directly comparable using equality, so we can drop the needless complication of ternary logic.

Let us presume that, as an SQL developer, you are now prepared to ban NULL from your database. If we constrain all columns to ensure that all data is marked as NOT NULL, then we can stop worrying about them, right? Unfortunately, NULL can still be introduced by:

OUTER JOINs:

```
SELECT dog.name, owner.name FROM dog LEFT OUTER JOIN owner ON dog.owner_id=owner.id;
  name  | name 
--------+------
 Sparky | 
 Slappy | 
 Smokey | 
(3 rows)

```

aggregate functions:

```
select sum(a) from no_rows;
 sum 
-----
    
(1 row)

```

CASE WHEN without ELSE clause:

```
SELECT CASE WHEN 1=0 THEN 'false cond' END;
 case 
------
 
(1 row)
```

SAFE_CAST/TRY_CAST (BigQuery/MSSQL):

```
SELECT SAFE_CAST('nope' AS DATE);
safe_cast
---------

(1 row)
```

There are effectively an unlimited number of ways NULLs could be reintroduced into queries, forcing SQL developers to reason about them. Even if we can wrap every conceivable value with `COALESCE()`, aren't we simply fighting a losing, up-hill battle?

The relational algebra neither requires nor recommends SQL ternary logic with NULL. Algebraic data types better capture what NULL was supposed to mean, so can we finally drop NULL?

### Sets Can Be Empty

A relation as defined by the relational algebra is set of attributes (called the "header") and a set of tuples (the "body") with data and matching attributes. Therefore, empty sets for both attributes and/or the tuple set are valid and meaningful. Empty tuple sets are obviously useful for representing a starting state (we don't know anything yet), but is there a value to supporting an empty attribute set such as with a table with zero columns?

It's rare to see an empty attribute (column set) in SQL, but it is possible.

```
create table emptycolumns();
```

We can insert empty-attributed tuples into the table.

```
insert into emptycolumns select;
insert into emptycolumns select;
table emptycolumns;
--
(2 rows)
select * from emptycolumns;
--
(2 rows)
select distinct * from emptycolumns;
ERROR:  SELECT DISTINCT must have at least one column
```

So, what is the value of this in SQL? Well, it's nominal which is probably why few are aware this is possible. However, a tuple with no attributes can be useful in SQL:

```
create table alldefaults(id SERIAL PRIMARY KEY, name TEXT DEFAULT 'unknown');
insert into alldefaults select;
insert into alldefaults select;
table alldefaults;
 id |  name   
----+---------
  1 | unknown
  2 | unknown
(2 rows)
```

Above, we create a table with two columns which both have default values. Therefore, we can insert rows with default values via an empty `SELECT`.

We can also join on empty attributes because tuples with empty attributes match each other.

```
select * from emptycolumns natural join emptycolumns AS e2;
--
(4 rows)

```

Eliding column names from the `SELECT` allows us to select zero attributes:

```
SELECT FROM emptycolumns;
--
(2 rows)
```

In the same way that empty sets are fundamental to set theory, empty attributes are fundamental to the relational alegbra. In the relational algebra, we have two fundamental relations: "relation true" and "relation false".

Relation true is the empty-attributed relation with one empty-attributed tuple.

```
TutorialD (master/main): :showexpr relation{}{tuple{}}
┌┐
││
├┤
└┘
```

Relation false is the empty-attributed relation with zero empty-attributed tuples.

```
TutorialD (master/main): :showexpr relation{}{}
┌┐
││
└┘
```

Even if these are fundamental, how are they useful?

Both relation true and relation false are the only two results of a relational projection on zero attributes:

```
TutorialD (master/main): :showexpr s
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Athens"  │"S5"    │"Adams"    │30             │
│"Paris"   │"S2"    │"Jones"    │10             │
│"Paris"   │"S3"    │"Blake"    │30             │
│"London"  │"S4"    │"Clark"    │20             │
│"London"  │"S1"    │"Smith"    │20             │
└──────────┴────────┴───────────┴───────────────┘
TutorialD (master/main): :showexpr s{}
┌┐
││
├┤
└┘
TutorialD (master/main): :showexpr (s where false){}
┌┐
││
└┘
```

Thus, after executing a projection on zero attributes, the result of an expression with more than zero tuples is relation true, otherwise relation false. You may also recognize these relations as identity functions.

```
TutorialD (master/main): :showexpr s join true = s
┌┐
││
├┤
└┘
```

`X join true` is always equivalent to `X`.

```
TutorialD (master/main): :showexpr s join false = s where false
┌┐
││
├┤
└┘
```

`X join false` is always equivalent to `X where false`. Any such equivalences can be used for query rewriting and optimization.

Note that SQL does not support relational equality directly.

### SQL Types vs. Algebraic Data Types

Data types to model the world can be complicated. SQL's C-inspired type system falls down under anything other than basic usage.

Imagine we are creating a survey with answers of varying types such as multiple choice and free-form text.

```
CREATE TABLE survey(question TEXT NOT NULL, answer ???);
```

SQL types force our hand to consider "alternative" type designs.

We can cram all the options into one table and use NULL as a placeholder to indicate a value is not relevant.

```
CREATE TABLE survey(question TEXT NOT NULL, answer_a TEXT, answer_b TEXT, answer_c TEXT, answer_d TEXT, freeformtext BOOLEAN);
```

This design hints that we can create up to four multiple-choice options *or* a free form text field for a user to answer the question if `freeformtext` is true. We could also include a convoluted constraint which ensures that `answer_X` columns are NULL if `freeformtext` is true and a constraint that ensures that `answer_b` is NOT NULL only if `answer_a` is filled in and so forth, but we are quickly falling off the rails in terms of complexity- all for a survey question.

Another design is to leverage table joins:

```
CREATE TABLE multi_answer(id INTEGER NOT NULL, answer_a TEXT, answer_b TEXT, answer_c TEXT, answer_d TEXT);
CREATE TABLE survey(question TEXT NOT NULL, multi_answer_id INTEGER REFERENCES multi_answer(id), freeformtext BOOLEAN);
```

This makes some of the constraints easier to write, but is otherwise just shuffling data around for little benefit.

A worse-but-common design is to shoehorn the data into a JSON structure.

```
CREATE TABLE survey(question TEXT NOT NULL, answer JSON NOT NULL);
insert into survey(question,answer) values ('What is your favorite sea animal?', '{"answer_a":"Seahorse", "answer_b":"Clam", "answer_c":"Shark", "answer_d":"Urchin"}');
```

Or maybe the answers should just be a JSON list. That would also be valid. But which format would the application prefer and what is enforcing the JSON format's coherency and relevance to our use-case? 

Consider if we used this format in our application then realized that we want to support an answer within an integer range (such as having the user select the number of children they have). Would we be expected to shoehorn this new requirement into the JSON blob or create a new column to indicate which "version" of the JSON blob format we expect?

In the above examples, we are merely working around the lack of proper, complex types in SQL. In a database that supports algebraic data types, this is a non-issue:

```
TutorialD (master/main): data Answers = MultiChoice (List Text) | FreeformText
TutorialD (master/main): survey := relation{tuple{question "Who is your favorite actor?", answer FreeformText}, tuple{question "How many siblings do you have?", answer MultiChoice (Cons "1" (Cons "2" (Cons "More Than 2" Empty)))}}
TutorialD (master/main): :showexpr survey
┌────────────────────────────────────────────────────────────┬────────────────────────────────┐
│answer::Answers                                             │question::Text                  │
├────────────────────────────────────────────────────────────┼────────────────────────────────┤
│FreeformText                                                │"Who is your favorite actor?"   │
│MultiChoice (Cons "1" (Cons "2" (Cons "More Than 2" Empty)))│"How many siblings do you have?"│
└────────────────────────────────────────────────────────────┴────────────────────────────────┘
```

First, we define a new algebraic data type, enumerating all possible values. Then we use it. The type is both forwards- and backwards-compatible, completely validated by construction, and never ambiguous.

### SQL Window Queries vs. Nested Relations

SQL bolted on the concept of "window functions" which operate on subsets of the tuples of a given query with relation to the "current row" being considered. It is often used in analytical queries.

```
SELECT depname, empno, salary,
       rank() OVER (PARTITION BY depname ORDER BY salary DESC)
FROM empsalary;
  depname  | empno | salary |          avg
-----------+-------+--------+-----------------------
 develop   |    11 |   5200 | 5020.0000000000000000
 develop   |     7 |   4200 | 5020.0000000000000000
 develop   |     9 |   4500 | 5020.0000000000000000
 develop   |     8 |   6000 | 5020.0000000000000000
 develop   |    10 |   5200 | 5020.0000000000000000
 personnel |     5 |   3500 | 3700.0000000000000000
 personnel |     2 |   3900 | 3700.0000000000000000
 sales     |     3 |   4800 | 4866.6666666666666667
 sales     |     1 |   5000 | 4866.6666666666666667
 sales     |     4 |   4800 | 4866.6666666666666667
(10 rows)
```

In the above example, we query employees per-department ranked by their salary. Note that this query is completely unrepresentable in the relational algebra for multiple reasons:

* tuples within a relation have no ordering because sets have no ordering
* there is no concept of a "current row" in the relational algebra
* a set of tuples cannot be placed together (again, there is no ordering)

Window functions in SQL are a workaround for the lack of nested relation support. Nested relations are a fundamental component of the relational algebra.

```
TutorialD (master/main): :showexpr s group ({all but status} as s)
┌─────────────────────────────────────────────┬───────────────┐
│s::relation {s#::Text,sname::Text,city::Text}│status::Integer│
├─────────────────────────────────────────────┼───────────────┤
│┌──────────┬────────┬───────────┐            │30             │
││city::Text│s#::Text│sname::Text│            │               │
│├──────────┼────────┼───────────┤            │               │
││"Athens"  │"S5"    │"Adams"    │            │               │
││"Paris"   │"S3"    │"Blake"    │            │               │
│└──────────┴────────┴───────────┘            │               │
│┌──────────┬────────┬───────────┐            │20             │
││city::Text│s#::Text│sname::Text│            │               │
│├──────────┼────────┼───────────┤            │               │
││"London"  │"S4"    │"Clark"    │            │               │
││"London"  │"S1"    │"Smith"    │            │               │
│└──────────┴────────┴───────────┘            │               │
│┌──────────┬────────┬───────────┐            │10             │
││city::Text│s#::Text│sname::Text│            │               │
│├──────────┼────────┼───────────┤            │               │
││"Paris"   │"S2"    │"Jones"    │            │               │
│└──────────┴────────┴───────────┘            │               │
└─────────────────────────────────────────────┴───────────────┘
```

In the above example, we group suppliers in subrelations based on status value. Note that the subrelations have no inherent ordering internally or relative to each other. We can leverage subrelation groupings to apply aggregate functions, apply rankings, and more. Furthermore, nested relations can contain nested relations, unlike window queries.

## On Building Sandcastles On Beaches

The derisive term "legacy" is not applied to technology merely because it is old. Software can *instantly* become "legacy" code if it fails to address the immediate or imminent use-cases. Developers object to such a definition because, based on this definition, nearly all code is instant legacy code and their egos are often tied to code cleverness or corners cut. Instead of becoming defensive, developers can step back and reassess: how can I stop writing legacy code? Why does most of my code inevitably end up in the trash can?

Such a line of thinking should lead the software developer to rephrase the question: what is reliable and permanent in software? That could be a complex question except for the fact that the [Curry-Howard isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) answers it for us. The permanent concepts that enable permanent software is math itself! (The Curry-Howard isomorphism states that any program can be represented as a mathematical proof and vice versa.) Thus, by creating software which adheres strongly to mathematical concepts, we give ourselves the best chance to write long-lasting code.

For example, column indexing is not included in the relational algebra- why would it be? - so deciding which representations or optimizations of the data is left to the database. Indexes are equivalent to query planner "hints" whereby we provide the query planner a potential execution path. The alternative, however, is to leverage the relational algebra itself to provide the execution paths and relegate human usage of the database to high-level concepts to manipulate data instead of manage how it's queried, which, ideally, is the always the job of a DBMS.

Project:M36 purports to follow the mathematics of the relational algebra closely instead of choosing to cut corners so as to be able to experiment with the complete, mostly unexplored richness of the relational algebra. 

By following the math, we can even find new means of executing queries. For example, there is much excitement around analytics-oriented, columnar-focused DBMSes. Wouldn't it make more sense if the DBMS could choose an arbitrary storage format based on how the data will be queried. A proper DBMS anticipates the needs of the application using heuristics or machine learning to determine how best to execute queries- it makes no sense to bake in limitations so that data can only be sliced and processed in limited ways.

Not all queries should be executed using the same planner strategies. For example, analytics queries could have a lower priority than transactional data manipulations. Such prioritization could limit how much IO, CPU, or network time the analytical queries could use by changing the execution plan. The DBMS could store data in multiple ways to best serve the needs of the application and send different queries through different data representations such as column vs. row stores. The same data could be represented any number of times on disk in various formats to best serve the queries of the application. None of this is possible if we peg tables to files and force users to choose various representations of the data such as with indexes and materialized views.


## Conclusion

For better or worse, SQL will likely be one of the calcified 1960s technologies used into the near future. SQL includes a number of mistaken assumptions and design-by-committee decisions that have not held up over time, but that doesn't mean we can't aspire to better.  

Project:M36 offers an upgrade path to the real relational algebra by offering an SQL frontend bolted onto a relational algebra engine, further underscoring how limiting SQL actually is, especially regarding the lack of nested relations, complex types, and math-derived optimization equivalences. Every SQL product offers a dialect of SQL and Project:M36 is no exception, naming it "SQLegacy" to emphasize that SQL should be demphasized.

The SQLegacy console enables this proposed transition via a relational-algebra-targeted language called TutorialD. The console convers SQL to TutorialD and displays it to the user:

```
SQLegacy (master/main): import example cjdate;
SQLegacy (master/main): select status from s where city='London' order by status;
[Equivalent TutorialD] :showdataframe ((s where sql_coalesce_bool( sql_equals( @city, "London" ) ))){status} orderby {status ascending}  
┌──┬────────────────┐
│DF│status::Integer⬆│
├──┼────────────────┤
│1 │20              │
└──┴────────────────┘

```

Note that SQLegacy always returns dataframes as indicated by "DF" in the top right of the diagram. That's because SQLegacy supports post-processing relational expressions into dataframes which do support ordering. SQL results are always ordered, even when arbitrarily.

The SQLegacy dialect and TutorialD implementations can co-exist and execute queries on the same databases. Since the Project:M36 software is built on the relational algebra on not SQL, other DBMS languages could be added, too.

Relying on the relational algebra simply opens more doors for correctness, cohesiveness, productivity, optimizations, and more.

## Bibliography

Codd, E.F. [*Serious Flaws in SQL*](https://dl.acm.org/doi/pdf/10.5555/77708.C1065772) 
