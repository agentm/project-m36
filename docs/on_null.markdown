# On NULL

## Introduction

There is no doubt that SQL has made NULL pervasive among database products. However, as Chris Date makes clear through numerous counter-examples, NULL can have no place in the relational algebra. This document summarizes Chris Date's rationale for banishing NULL, provides a rationale for why it cannot appear in a mathematically-coherent relational algebra system, and describes some alternative designs.

## NULL is not a value

SQL intentionally differentiates NULL from all other values. NULL is not an integer or text or anything else (though it does carry type information). In fact, NULL is literally equal to nothing else.

```
postgres=# select NULL = NULL;
 ?column? 
----------
 
(1 row)
```

Whether two NULLs are equal is unknown (NULL). Of course, this leads to three-valued logic: a boolean query can true, false, or unknown (NULL).

## Ambiguity

The purpose of a database is to model reality in some form, so what does the NULL represent in reality? Consider a nullable integer column; NULL could represent:

* a unanswered question (How many children does he have? We forgot to ask.)
* a refused answer (How many children does he have? He refused to divulge this.)
* not applicable (How many children does the corporation have? Nonsensical.)
* information to collect in the future (How many children does he have? Ask him tomorrow.)
* the information can be found elsewhere (How many children does he have? Find the children's names with a join on some other table.)
* and any other arbitrary meaning

The problem is that NULL gives no indication which of the above meanings are intended. Indeed, NULL could indicate that one or even more than one of the meanings could hold. The schema's nullable values makes the intention completely ambiguous.


## The Folly of Three-Valued Logic

When dealing with NULLs, one inevitably must encounter the three-valued logic implied by the NULL non-value. This logic often comes as a surprise to newcomers as in the following examples:

Setup: Create an example table with some NULLs.
```
postgres=# create table x (a int, b text);
CREATE TABLE
postgres=# insert into x(a,b) values (1,'one'),(2,'two'),(NULL,'three') returning a,b;
 a |   b   
---+-------
 1 | one
 2 | two
   | three
(3 rows)
```
Example 1: Return all rows where a is not equal to one.
```
postgres=# select * from x where a <> 1;
 a |  b  
---+-----
 2 | two
(1 row)
```
Example 2: Return all rows where a is equal to itself.
```
postgres=# select * from x where a=a;
 a |  b  
---+-----
 1 | one
 2 | two
(2 rows)
```
Example 3: Return all rows where b is from a list of NULL.
```
postgres=# select * from x where b = any(NULL);
 a | b 
---+---
(0 rows)
```
Example 4: Return all rows where the (a,b) tuple are in the set of x's tuples.
```
postgres=# select * from x where (a,b) = any(select * from x);
 a |  b  
---+-----
 1 | one
 2 | two
(2 rows)
```
Example 5: Return the natural join of x to itself.
```
postgres=# select * from x natural join x as y;
 a |  b  
---+-----
 1 | one
 2 | two
(2 rows)
```
Example 6: Return rows in x where the same row appears in x.
```
postgres=# select * from x where exists (SELECT * FROM x as y where (a,b) = (y.a,y.b));
 a |   b   
---+-------
 1 | one
 2 | two
   | three
(3 rows)
````

All of the above query examples conform to the SQL standard with regards to NULL handling. 


## Inconsistencies Leading to Errors

If the difference between NULL handling in ```ANY``` versus ```EXISTS``` didn't surprise you, perhaps some NULL-in-aggregate-function usage will.

Example 7: Aggregate a sum over a in x where one a value is NULL.
```
postgres=# select sum(a) from x; 
 sum 
-----
   3
(1 row)
```
Example 8: Aggregate a sum over two NULLs.
```
postgres=# select sum(a) from (values (NULL::int),(NULL)) as y(a); --sum of only NULLs is NULL
 sum 
-----
    
(1 row)
```
Example 9: Count the number of a values in x. Count the number of rows in x.
```
postgres=# select count(a) from x;
 count 
-------
     2
(1 row)
postgres=# select count(*) from x;
 count 
-------
     3
(1 row)
```
Example 10: Aggregate the sum of an empty table.
```
postgres=# create table z(a int);
CREATE TABLE
postgres=# select sum(a) from z; -- sum of empty table is NULL
 sum 
-----
    
(1 row)
```
Example 11: NULL multipled by zero plus three.
```
postgres=# select 0*NULL + 3;
 ?column? 
----------
         
(1 row)
```

This logic follows into arrays:

Example 12: Is NULL equal to any element in the array?
```
postgres=# select NULL = ANY(array[1,2,3,NULL]);
 ?column? 
----------
 
(1 row)
```

and row-valued expressions:

Example 13: Is the (NULL,1) tuple equal to NULL? Is it not equal to NULL?
```
postgres=# select (NULL,1) is null;
 ?column? 
----------
 f
(1 row)

postgres=# select (NULL,1) is not null;
 ?column? 
----------
 f
(1 row)
```

Are you certain that your application handles NULL correctly? Would you notice if some rows disappeared due to NULL?

## Multiple NULL Nonsense

SQL does not enforce the relational model constraint that rows (tuples) be unique in a table (relation). So what does the following table mean?

Example 14: Create multiple NULLs in a result set.
```
postgres=# select * from (values (34),(50),(NULL),(NULL)) as employee_age(val);
 val 
-----
  34
  50
    
    
(4 rows)
```

What could this construct model in reality? In general, what does a row which is completely NULL mean?

## Ordering NULLs

Since NULLs are not valued, they don't have a logical sort order. Luckily, SQL allows one to arbitrarily sort NULLs to come before all real values or after all real values. But... so what? Invariably, the receiver of a result set containing NULLs will require special handling for the NULLs.

## NOT NULL

Suppose you are convinced NULLs in SQL are too convoluted to be used safely. Even if all columns in your schema are marked ```NOT NULL```, NULLs can still make an unwanted appearance. We have seen this with some aggregate functions on empty tables, but any usage of ```OUTER JOIN``` could generate  NULLs, too.

## Outer Join Ambiguity

Example 15: A table with one valued-row and one completely NULL row. Construct NULL-aware left outer join.
```
postgres=# create table z(a int,b text);
CREATE TABLE
postgres=# insert into z(a,b) values (2,'two'),(NULL,NULL) returning a,b;
 a |  b  
---+-----
 2 | two
   | 
(2 rows)
postgres=# select x.a as xa,x.b as xb,z.a as za,z.b as zb from x left join z on x.a is not distinct from z.a;
 xa |  xb   | za | zb  
----+-------+----+-----
  1 | one   |    | 
  2 | two   |  2 | two
    | three |    | 
(3 rows)

```
Does the double-NULL which appears twice reference the actual double-NULL row in ```z``` or is it a missing row which is actually missing in ```z```? There is no way to know from this query. In outer joins, NULLs are overloaded with at least two possible meanings.


## Reversed Logic

Inevitably, when one attacks the notion of NULL is SQL, database developers become defensive:

* You can't have a database properly model reality without NULLs. Missing information is a fact-of-life.
* Even if NULLs don't fit the mathematics, they are too valuable to dismiss.
* Getting rid of NULLs is a purely academic concern. NULLs don't bother me.

However, the onus should be on those holding these viewpoints to justify the existence of a construct which:

* adds substantial complexity in the fundamental logic
* adds ambiguity to query results
* reduces confidence that queries are logically-sound
* fails to adequately model the intention of missing information (more on this below)

Undoubtedly, representing missing or unknown values *is* a useful construct, but unfortunately SQL's NULL fails to adequately represent this notion.

If the SQL designers had wanted to be mathematically rigorous with regards to NULL, every query would return two tables (result sets): one where the query's predicate certainly holds, and a second where the predicate might hold if NULL is interpreted as unknown- a sort of logical superposition.

## What was the goal of NULL?

Clearly, the notion of NULL would not have been introduced if it served no valid purpose. NULL came from a time where all base database types could fit in CPU registers: integers, ASCII bytes (text), floats. Most programming languages includes these data types in some form, so to include them in SQL was a logical step.

Then, someone must have realized that it can be useful to have a tuple/row record an employee's name, date-of-hire, and salary, even if his age, an otherwise required attribute/column, is unknown. This is certainly a fair need to fulfill, so why not put a placeholder in the place of age, indicating "fill this in later". Some programming languages include the notion of null references which literally reference nothing. (The first relational-model-based DBMS was [System R](http://www.cs.berkeley.edu/~brewer/cs262/SystemR.pdf) written in PL/I and included NULL). So, the inclusion of this concept in SQL seemed logical. However, in those same programming languages, attempting to dereference or use a NULL reference in calculations results in a fatal error, terminating the program. Obviously, we don't want to fail to answer queries with missing/unknown information, because it's not logically a fatal condition. Thus, the three-valued logic was born.

Unfortunately, the consequences of including three-valued logic were not fully considered. There is no shame in this, as long as we learn from mistakes. Even Codd, the original IBM-based author of the seminal paper describing the relational model, tried to shoehorn in the concept of NULL.

## What is the alternative to NULL?

Chris Date devotes several chapters to exactly this question in his "[Database Explorations](http://bookstore.trafford.com/Products/SKU-000177853/Database-Explorations.aspx)". Some of his suggestions are not teneable, but are considered for the purpose of discussion. Date's options for representing missing data, summarized, are:

* "The Decomposition Approach": normalize/decompose relations horizontally or vertically so that missing data is implied by missing tuples
* "The Multirelational Approach": a structure which includes multiple relations with differing headers. This approach is mathematically untenable, but listed for dicussion.
* "An Inheritance Approach": use vaguely object-oriented type hierarchies to build new types which may include a missing value. Note that "missing" is indeed a value in this model.
* "An Approach Using Relation Valued Attributes": use single- or empty-tupled relation-values to represent present or missing data

Date, however, fails to generalize the "inheritance approach" which leads to perhaps the easiest solution. [Algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type) are essential in functional programming but make only rare appearances in other programming languages. The core of algebraic data types is that types themselves can be composed using either products or sums of types.

Defining a new type can involve taking the product of two other types, as in:
```haskell
data BookType = Book BookTitle ISBN
```
where ```BookTitle``` and ```ISBN``` are pre-existing types. Here, the new BookType can be considered a sort of new two-tuple holding two other types.

Types can also be composed with a summation:
```haskell
data HairColorType = HairColor ColorName |
                     Bald
```

In this case, ```ColorName``` is a text string describing the color. The values associated with a ```HairColorType``` could be any of the following (as examples):

* ```HairColor "Brown"```
* ```HairColor "Blonde"```
* ```Bald```

Here, we witness that a type can be composed of multiple other types in a kind of "or" relationship. Indeed, this is precisely our intention when we wish to mark something as unknown! If a record's "age" attribute is unknown, the value of that record's field should model that intention. Furthermore, we can be explicit about why the value is missing:

```haskell
data AgeType = PreciseAge NaturalNumberType |
               ForgotToAsk |
               RefusedToDisclose |
               NotApplicable |
               ApproximateAge NaturalNumberType NaturalNumberType
```
and some examples values:

* ```PreciseAge 25```
* ```NotApplicable```
* ```ApproximateAge 30 40``` approximately between 30-40 years old
 
Already, we can see that this single data type can pack far more useful information than any combination of an integer and NULL.

### In SQL

An algebraic data type may be emulated in SQL, albeit in a convoluted fashion and without type safety. Here is the above example translated using SQL enumerations:

```SQL
postgres=# create type age_type as enum ('precise_age', 'forgot_to_ask', 'refused_to_disclose', 'not_applicable', 'approximate_age');
CREATE TYPE

postgres=# create table hospital_patient(name text not null, 
                                         age age_type not null, 
                                         precise_age int,
                                         approximate_age int[2]);
postgres=# insert into hospital_patient(name,age,precise_age,approximate_age) values ('Bob','approximate_age',NULL,array[30,40]);  
INSERT 0 1
postgres=# select name,
case when age = 'precise_age' then
  'precisely ' || precise_age::text
when age = 'approximate_age' then
  'approximately ' || approximate_age[1] || '-' || approximate_age[2]
else
  age::text
end
from hospital_patient;
 name |         age         
------+---------------------
 Bob  | approximately 30-40
(1 row)
```

But all we have really done is add even more NULLs. Retrieving a sensible value is convoluted and not strongly-typed. In addition, the table constraints (not shown) necessary to ensure that, for example, precise_age is NULL if age is "forgot_to_ask" make this scheme even more complicated to implement.

### In Project:M36

Any Haskell data type can be turned into a value (called an "atom") in Project:M36. The following example illustrates:

```haskell
{-# LANGUAGE OverloadedStrings,DeriveAnyClass,DeriveGeneric #-}
module Hospital where

import ProjectM36.Client
import ProjectM36.Atom
import Data.Typeable
import ProjectM36.Relation
import Data.Binary
import Control.DeepSeq
import Data.Text
import GHC.Generics
import Data.Hashable
import ProjectM36.Tuple

data AgeType = PreciseAge Int |
               ForgotToAsk |
               RefusedToDisclose |
               NotApplicable |
               ApproximateAge Int Int
             deriving (Eq,Show,Read,Hashable,Binary,Typeable,NFData,Generic)
                      
instance Atomable AgeType

failFastMaybe :: (Show a) => Maybe a -> IO ()
failFastMaybe (Just err) = error (show err)
failFastMaybe Nothing = return ()

failFastEither :: (Show a) => Either a b -> IO (b)
failFastEither (Left err) = error (show err)
failFastEither (Right val) = return val

ageAtomType :: AtomType
ageAtomType = atomTypeForProxy (Proxy :: Proxy AgeType)
               
runExample :: IO ()
runExample = do
  let bob_relation_attrs = attributesFromList [Attribute "name" stringAtomType,
                                               Attribute "age" ageAtomType]
      relvar_name = "hospital_patient"
      age_value_in = ApproximateAge 30 40 
      bob_relation_err = mkRelationFromList 
                     bob_relation_attrs 
                     [[Atom ("Bob"::Text), Atom age_value_in]]

  connerr <- connectProjectM36 (InProcessConnectionInfo NoPersistence)
  conn <- failFastEither connerr
  bob_relation <- failFastEither bob_relation_err
  --create the relation variable
  merr <- executeDatabaseContextExpr conn (Assign relvar_name (ExistingRelation bob_relation))
  failFastMaybe merr
  --query the relation variable
  result_err <- executeRelationalExpr conn (RelationVariable relvar_name)
  result <- failFastEither result_err
  case singletonTuple result of
    Nothing -> error "not a singleton relation!"
    Just tuple -> do
      (Atom age_value_out) <- failFastEither $ atomForAttributeName "age" tuple
      putStrLn (show age_value_in)
      putStrLn (show age_value_out)
      case cast age_value_out of
        Nothing -> error "wrong datatype"
        Just age_value_out' -> do
          putStrLn (show (age_value_in == age_value_out'))

```

The example demonstrates that the algebraic datatype ```AgeType``` value which goes into the database is an equal value when it is retrieved from the database.

Since ```AgeType``` is a normal value, but one which can be created using multiple constructors (```PreciseAge x```, ```ForgotToAsk```, etc.), it can be manipulated using standard database functions without any special consideration for which values indicate "missing-ness".

## Summary

This document summarizes the inconsistencies and ambiguities when faced with dealing with SQL's NULL. As Date writes, "You can never trust the answers you get from a database with nulls. In my opinion, this state of affairs is a complete showstopper." ([Database In Depth](http://shop.oreilly.com/product/9780596100124.do) p. 55)

Regardless, modeling the fact that some information may be missing is valuable, as long as the fact is unambiguous and does not violate the relational algebra. This can be accomplished by several means, but the easiest is to make use of algebraic data types. Unfortunately, SQL products do not support algebraic data types.


## Additional Reading

Date, C.J. Darwen, Hugh. [Database Explorations: Essays on the Third Manifesto and Related Topics](http://bookstore.trafford.com/Products/SKU-000177853/Database-Explorations.aspx) chapters 23-29

Date, C.J. [Database in Depth](http://bookstore.trafford.com/Products/SKU-000177853/Database-Explorations.aspx) pages 53-55




