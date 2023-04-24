# TutorialD for SQL Developers

## Introduction
<!--
Hi, I'm Darin from Jumpstartups. If you been using SQL, you know that it has some sharp edges- unpleasant surprises like unexpected NULLs, managing schema migrations, or differences in how SQL and the client language handle types.


As an SQL developer, you may have even become accustomed to such weirdness, but, in this course, you will learn that SQL's quirks are not a result of the relational algebra, rather that they conflict with the mathematics itself. A new language called "TutorialD" eliminates these quirks by sticking closely to the mathematics.

In this video, we'll take a look at what makes TutorialD a better database query language.

<media-tag src="https://redapes.org/wp-content/uploads/2012/05/Gunung3-900w-300x200.jpg"></media-tag>
-->
* 'differences in how SQL and the client language handle types': show SQL logo and python logo clashing
* tutorialD visual at the last sentence
---

## Setup Project:M36

<!--We'll be using the TutorialD interpreter provided by Project:M36, a relational algebra engine/DBMS. We'll be running Project:M36 from a docker container. If you would prefer to build Project:M36 yourself, refer to Project:M36 website.-->

First, make sure you have docker installed:

```
docker --version
```

Next, run:

```
docker run -it projectm36/project-m36 tutd
```
* video of terminal installing project m36
---

<!-- After a quick download, the `tutd` command line interpreter should be ready-to-use. -->

## Why TutorialD?

<!--So why are we bothering with another database query language? Despite its quirks, isn't SQL good enough?
-->
* speaking to camera (engaging, warmth)

<!--
The fundamental problem with SQL is its historical baggage. As SQL was being designed, design decisions were made with the hardware and software limitations of the 1970s in mind.
-->
* show image of computer from 1970s

<!--
For example, you may have heard that a relation as defined in relational algebra is composed of attributes and a set of attribute-matching tuples. SQL, however, supports duplicate "rows" because back then, checking to eliminate duplicate rows was viewed as too costly. As we shall see, not sticking to the underlying math has a price.
-->
* show sql duplicate rows in a table with red arrow pointing out duplicate rows

<!--

TutorialD is an developer-ergonomic reimagining of a database query language and drops the decades-long legacy baggage of SQL. Let's test out TutorialD!-->

* regular Darin

---

## TutorialD with Project:M36
<!--
You should see a TutorialD prompt like this one here.
-->
```
TutorialD (master/main):
```
* show video of tutd terminal interaction
<!--
"master" is the name of the current branch. Like git, Project:M36 supports branching.

"main" is the name of current schema.
-->
* display arrows pointing at master and then schema in terminal

<!--
We're going to focus on the TutorialD language, but you can learn more about these features on the Project:M36 website linked below.
-->

* show Project:M36 github page
---

<!--
Let's create our first relation:
-->
```
TutorialD (master/main): :showexpr relation{tuple{name "Steve", age 30, dept "Sales"}, tuple{name "Bob", age 20, dept "Engineering"}}
┌────────────┬─────────────┬──────────┐
│age::Integer│dept::Text   │name::Text│
├────────────┼─────────────┼──────────┤
│30          │"Sales"      │"Steve"   │
│20          │"Engineering"│"Bob"     │
└────────────┴─────────────┴──────────┘

```
<!--
English: Create a relation with information about two employees: Steve, age 30, in the sales department, and Bob, age 20 in Engineering.
-->

<!--
Here, we have created a relation with employee data regarding Bob and Steve. An equivalent expression in SQL could be:
-->
```SQL
select 30 as age, 'Sales' as dept, 'Steve' as name union select 20, 'Engineering', 'Bob';
 age |    dept     | name  
-----+-------------+-------
  20 | Engineering | Bob
  30 | Sales       | Steve
(2 rows)

```
---
<!--
**Did you notice that the attribute ordering in TutorialD is not the same as we typed it in?**
~~Note that the attribute ordering in TutorialD is not the same as we typed it in.~~ That is because, unlike SQL, the attributes have no specific ordering so one is chosen arbitrarily.
-->
* show arrows pointing at attributes listed in query vs. those in tabular result

<!--
In addition, the body data of a relation is a set of rows or "tuples" as called in (the?) relational algebra. Indeed, if you run the above expression multiple times, you will discover that the tuples change position because, naturally, a set of tuples has no ordering so one is chosen arbitrarily.
-->
* show multiple runs of TutorialD evaluation returning different ordering of tuples
---
1.html
```
TutorialD (master/main): :showexpr relation{tuple{name "Steve", age 30, dept "Sales"}, tuple{name "Bob", age 20, dept "Engineering"}}
┌────────────┬─────────────┬──────────┐
│age::Integer│dept::Text   │name::Text│
├────────────┼─────────────┼──────────┤
│20          │"Engineering"│"Bob"     │
│30          │"Sales"      │"Steve"   │
└────────────┴─────────────┴──────────┘
```


---

<!--
Like SQL, we can store relations as named tables. In a relational algebra engine, we call these "relation variables" or "relvars". Relvars are names mapped to relations which can change over time, making them "variable". In the above expression (show expression), we ask the database to generate a relation, but we don't save it to a relvar. Let's see what we can do with relvars.
-->

<!--
To see which relvars we have defined, use "show relvars" (don't forget the preceding colon):
-->
2.html
```
TutorialD (master/main): :showrelvars
┌─────────────────────────────────────────────────┬──────────┐
│attributes::relation {attribute::Text,type::Text}│name::Text│
├─────────────────────────────────────────────────┼──────────┤
│┌───────────────┬──────────┐                     │"true"    │
││attribute::Text│type::Text│                     │          │
│└───────────────┴──────────┘                     │          │
│┌───────────────┬──────────┐                     │"false"   │
││attribute::Text│type::Text│                     │          │
│└───────────────┴──────────┘                     │          │
└─────────────────────────────────────────────────┴──────────┘

```

<!--
In the default database, we have added two relvars named "true" and "false". We'll get to those later. Let's load up some more common data and explore.
-->

---
3.html

```
TutorialD (master/main): :importexample cjdate
```
<!--
This command loads in a small example set of relvars discussed in books by C.J. Date. 
-->

```
TutorialD (master/main): :showrelvars
┌─────────────────────────────────────────────────┬──────────┐
│attributes::relation {attribute::Text,type::Text}│name::Text│
├─────────────────────────────────────────────────┼──────────┤
│┌───────────────┬──────────┐                     │"true"    │
││attribute::Text│type::Text│                     │          │
│└───────────────┴──────────┘                     │          │
│┌───────────────┬──────────┐                     │"sp"      │
││attribute::Text│type::Text│                     │          │
│├───────────────┼──────────┤                     │          │
││"s#"           │"Text"    │                     │          │
││"p#"           │"Text"    │                     │          │
││"qty"          │"Integer" │                     │          │
│└───────────────┴──────────┘                     │          │
│┌───────────────┬──────────┐                     │"false"   │
││attribute::Text│type::Text│                     │          │
│└───────────────┴──────────┘                     │          │
│┌───────────────┬──────────┐                     │"s"       │
││attribute::Text│type::Text│                     │          │
│├───────────────┼──────────┤                     │          │
││"s#"           │"Text"    │                     │          │
││"city"         │"Text"    │                     │          │
││"status"       │"Integer" │                     │          │
││"sname"        │"Text"    │                     │          │
│└───────────────┴──────────┘                     │          │
│┌───────────────┬──────────┐                     │"p"       │
││attribute::Text│type::Text│                     │          │
│├───────────────┼──────────┤                     │          │
││"color"        │"Text"    │                     │          │
││"city"         │"Text"    │                     │          │
││"p#"           │"Text"    │                     │          │
││"pname"        │"Text"    │                     │          │
││"weight"       │"Integer" │                     │          │
│└───────────────┴──────────┘                     │          │
└─────────────────────────────────────────────────┴──────────┘
```
<!--
(Woah, ok- that's new.) In addition to "true" and "false", we also got "s" for "supplier", "p" for "parts", and "sp" for the many-to-many join condition representing "shipments". We can also see all the attributes for each table. In SQL, we call these "columns". For example, relvar "s" has four attributes, three "Text" types and one "Integer".

Hold on, do you see anything weird here? Take a look at the type of "attributes" for the matching relvar names. Yes, that's a relation within a relation, which, as we will cover later, is a fundamental component of relational algebra. Next, look at what's under the "attributes" section- we see a box within an outer box- so that's the relation within a relation. Here, you can see how it is immediately useful to associate a relation with detail data and the relvar name. In SQL, this representation is simply impossible as SQL does not support tables within tables. In SQL, this might look like:
-->
* highlight relation-within-relation (shade background)

---

```SQL
# select * from (values ('s#','Text','s'),('city','Text','s'),('status','Integer','s'),('sname','Text','s'),('p#','Text','p')) as t(attribute,type,name);
 attribute |  type   | name 
-----------+---------+------
 s#        | Text    | s
 city      | Text    | s
 status    | Integer | s
 sname     | Text    | s
 p#        | Text    | p
...
```
* combine with previous slide to show tutd and sql side-by-side

<!--
But, wait, that's not the same thing at all. With nested relations, we can accurately represent a grouping within a single tuple. SQL cannot. If I were trying to display a hierarchical view of this data from SQL, I would have to loop over the results and manually split up groups in order to come close to what TutorialD already can provide. We'll see this pattern in a future section.

Ok, so backtracking now, we have a bunch of relvars available. Let's take a look at one (s):
-->

---
4/html
```
TutorialD (master/main): :showexpr s
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Paris"   │"S3"    │"Blake"    │30             │
│"London"  │"S4"    │"Clark"    │20             │
│"Paris"   │"S2"    │"Jones"    │10             │
│"Athens"  │"S5"    │"Adams"    │30             │
│"London"  │"S1"    │"Smith"    │20             │
└──────────┴────────┴───────────┴───────────────┘
```

* English: show me all data available on suppliers.

<!--
Like SQL, each attribute has a type- here we see "Text" and "Integer" types. Reiterating here, neither attributes nor tuples have any ordering, so the ordering we see here is arbitrary. We'll see later how to apply ordering.
-->
* highlight text and integer types

---

# Restriction

<!--
Here is some basic filtering with a where clause:
-->
5.html
```
TutorialD (master/main): :showexpr s where city="London"
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"London"  │"S4"    │"Clark"    │20             │
│"London"  │"S1"    │"Smith"    │20             │
└──────────┴────────┴───────────┴───────────────┘
```
English: show me all data available on suppliers in London.
<!--
Ok, that's quite similar to SQL. This is called "restriction" in relational algebra.
-->

---

# Projection
6.html
<!--
How about selecting a subset of attributes:
-->

```
TutorialD (master/main): :showexpr s{s#,sname} where city="London"
┌────────┬───────────┐
│s#::Text│sname::Text│
├────────┼───────────┤
│"S4"    │"Clark"    │
│"S1"    │"Smith"    │
└────────┴───────────┘
```
English: show me IDs and names of suppliers in London.

<!--
That's a bit different syntax from SQL, but makes sense. By the way, constraining attributes in relational algebra is called "projection". We can also filter out some the attributes we don't want.
-->
7.html
```
TutorialD (master/main): :showexpr s{all but city}
┌────────┬───────────┬───────────────┐
│s#::Text│sname::Text│status::Integer│
├────────┼───────────┼───────────────┤
│"S3"    │"Blake"    │30             │
│"S5"    │"Adams"    │30             │
│"S1"    │"Smith"    │20             │
│"S4"    │"Clark"    │20             │
│"S2"    │"Jones"    │10             │
└────────┴───────────┴───────────────┘ 
```

English: show me all data available on suppliers except their city info.
<!--
Some SQL variants include this handy feature as `SELECT * EXCEPT city FROM s`, but it's non-standard SQL.

Next, let's see how we can modify these relvars.
-->

---

## Relvar Updates

<!--
To create a new relvar, we can simply assign it:
-->
8.html

```
TutorialD (master/main): x:=relation{tuple{name "Steve", age 30, dept "Sales"}, tuple{name "Bob", age 20, dept "Engineering"}}
TutorialD (master/main): :showexpr x
┌────────────┬─────────────┬──────────┐
│age::Integer│dept::Text   │name::Text│
├────────────┼─────────────┼──────────┤
│30          │"Sales"      │"Steve"   │
│20          │"Engineering"│"Bob"     │
└────────────┴─────────────┴──────────┘
```
English: create a relation with Bob and Steve's employee information and save it to the relvar "x".

<!--
or we can define it and then insert, like SQL.
-->
9.html
```
TutorialD (master/main): x := relation{name Text, age Integer, dept Text}
TutorialD (master/main): insert x relation{tuple{name "Steve", age 30, dept "Sales"}, tuple{name "Bob", age 20, dept "Engineering"}}
TutorialD (master/main): :showexpr x
┌────────────┬─────────────┬──────────┐
│age::Integer│dept::Text   │name::Text│
├────────────┼─────────────┼──────────┤
│20          │"Engineering"│"Bob"     │
│30          │"Sales"      │"Steve"   │
└────────────┴─────────────┴──────────┘
```
English: create a relvar "x" with no tuples but having name, age, and dept properties. Next, insert employee data about Steve and Bob. Finally show me the contents of the relvar named "x".

<!--
Note that we don't insert a list of tuples, we insert a whole relation (highlight the whole relation for clarity)! That means that we effectively unioned two relations to get a new relation. That's the same as an insert but different than in SQL.
-->

* highlight relation being inserted

```
create table x(age int, name text, dept text);
insert into x(age, name, dept) values (20, 'Bob', 'Engineering');
insert into x(age, name, dept) values (30, 'Steve', 'Sales');
table x;
 age | name  |    dept
-----+-------+-------------
  20 | Bob   | Engineering
  30 | Steve | Sales
(2 rows)
```

---

<!--
SQL sort of supports inserting a table into another table, but it does it in a roundabout way:
-->

```
insert into x (SELECT * FROM x);
table x;
 age | name  |    dept     
-----+-------+-------------
  20 | Bob   | Engineering
  30 | Steve | Sales
  20 | Bob   | Engineering
  30 | Steve | Sales
(4 rows)
```

---

# Dealing with Duplicates

<!--
Note that tuplesets in the relational algebra are true sets- that means *no duplicate tuples*! So what does our table mean now? Do we have two different Steves with the same age in the same department? Is the same Steve represented twice? Saying something twice doesn't make it more true. The meaning in SQL is ambiguous and is likely to cause problems.

Here's the same table be inserted into itself in TutorialD:
-->
10.html
```
TutorialD (master/main): x := x union x
TutorialD (master/main): :showexpr x
┌────────────┬─────────────┬──────────┐
│age::Integer│dept::Text   │name::Text│
├────────────┼─────────────┼──────────┤
│30          │"Sales"      │"Steve"   │
│20          │"Engineering"│"Bob"     │
└────────────┴─────────────┴──────────┘
```

English: insert x into itself, the result is x!

<!--
That's right, `x union x` is effectively a no-op- this means that `x` and `x union x` are completely equivalent expressions. The database can recognize and not do the additional useless work involving `union`, by the way.
-->

---

# Deleting Tuples
<!--
How about deleting tuples?  How would we go about deleting tuples related to 'Bob'?
-->
11.html
```
TutorialD (master/main): delete x where name = "Bob"
TutorialD (master/main): :showexpr x
┌────────────┬──────────┬──────────┐
│age::Integer│dept::Text│name::Text│
├────────────┼──────────┼──────────┤
│30          │"Sales"   │"Steve"   │
└────────────┴──────────┴──────────┘
```

English: remove all data in relvar "x" pertaining to any employee named "Bob".


```
DELETE FROM x WHERE name = 'Bob';
DELETE 2
table x;
 age | name  | dept
-----+-------+-------
  30 | Steve | Sales
```

---

# Updating Tuples

Finally, we can update existing tuples:
12.html
```
TutorialD (master/main): update x where age=30 (age := 31)
TutorialD (master/main): :showexpr x
┌────────────┬──────────┬──────────┐
│age::Integer│dept::Text│name::Text│
├────────────┼──────────┼──────────┤
│31          │"Sales"   │"Steve"   │
└────────────┴──────────┴──────────┘
```

English: change every age which is equal to 30 to 31.

<!--
That's quite similar to SQL:
-->

```SQL
UPDATE x SET age=31 WHERE age=30;
UPDATE 2
table x;
 age | name  | dept  
-----+-------+-------
  31 | Steve | Sales
  31 | Steve | Sales
(2 rows)
```

---

# Duplicates Summary

<!--
In summary, relations cannot contain duplicate tuples because a relation contains a *set* of tuples. Yes, that is a *set* from mathematical set theory. Tuple uniqueness is an important property of relational algebra which will come up many times later.
-->

<!--
We've hinted at some relational operators, but let's look at some more detailed examples.
-->

* regular Darin talking

---

## More Relational Operators

<!--
Let's take a look at the all important join:
-->
13.html
```
TutorialD (master/main): :showexpr s join sp
┌──────────┬────────┬────────────┬────────┬───────────┬───────────────┐
│city::Text│p#::Text│qty::Integer│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼────────────┼────────┼───────────┼───────────────┤
│"London"  │"P2"    │200         │"S1"    │"Smith"    │20             │
│"London"  │"P5"    │400         │"S4"    │"Clark"    │20             │
│"London"  │"P2"    │200         │"S4"    │"Clark"    │20             │
│"London"  │"P1"    │300         │"S1"    │"Smith"    │20             │
│"Paris"   │"P2"    │400         │"S2"    │"Jones"    │10             │
│"Paris"   │"P1"    │300         │"S2"    │"Jones"    │10             │
│"Paris"   │"P2"    │200         │"S3"    │"Blake"    │30             │
│"London"  │"P3"    │400         │"S1"    │"Smith"    │20             │
│"London"  │"P4"    │300         │"S4"    │"Clark"    │20             │
│"London"  │"P4"    │200         │"S1"    │"Smith"    │20             │
└──────────┴────────┴────────────┴────────┴───────────┴───────────────┘
```

English: merge matching tuples in both relvars "s" and "sp" based on attributes of the same name.
<!--
Yep, two relvar names with a join in between is all you need. This is actually the seldom-used `NATURAL JOIN` in SQL which infers the join condition from identical column names. **Show s and p separately on-screen and then show the join result**

In SQL, this looks like:
-->
```
SELECT * FROM s NATURAL JOIN sp;
 s# | sname | status |  city  | p# | qty 
----+-------+--------+--------+----+-----
 S1 | Smith |     20 | London | P4 | 200
 S4 | Clark |     20 | London | P5 | 400
 S1 | Smith |     20 | London | P3 | 400
 S2 | Jones |     10 | Paris  | P2 | 400
 S2 | Jones |     10 | Paris  | P1 | 300
 S4 | Clark |     20 | London | P4 | 300
 S1 | Smith |     20 | London | P1 | 300
 S3 | Blake |     30 | Paris  | P2 | 200
 S4 | Clark |     20 | London | P2 | 200
 S1 | Smith |     20 | London | P2 | 200
(10 rows)

```

<!--
Not so different after all. 
-->

---

# Rename

<!-- But wait, what if I need to specify a join condition? No problem, we just need to rename the attributes until the natural join applies. In other words, we need to ensure that the join condition is represented by attributes having the same name.
-->
14.html
```
TutorialD (master/main): :showexpr (p{weight} rename {weight as status}) join s{status}
┌───────────────┐
│status::Integer│
└───────────────┘

```
English: Rename "weight" in relvar "p" to "status" to merge matching tuples in all status values in "s" with all status values in "p" (formerly "weight"). 
<!--
The equivalent in SQL would probably not use `NATURAL JOIN` because SQL emphasizes novel join conditions:
-->

```SQL
SELECT status FROM p JOIN s ON status = weight;
 status 
--------
(0 rows)

```

---

# Outer Join Equivalent

<!--
Ok, well how about something more complicated... like an OUTER JOIN! In SQL, if we wanted to see a list of parts along with their destinations, we would use an OUTER JOIN so as not to drop parts which have never been shipped. First, let's add a new part "P7":
-->
15.html
```SQL
INSERT INTO p("p#",pname,color,weight,city) VALUES ('P7','Widget','Beige',21,'Reykjavik');
```
<!--
Next, we execute the OUTER JOIN:
-->

```SQL
SELECT p."p#",sp."s#" FROM p AS p LEFT OUTER JOIN sp AS sp ON p."p#" = sp."p#";
 p# | s# 
----+----
 P1 | S2
 P1 | S1
 P2 | S2
 P2 | S4
 P2 | S3
 P2 | S1
 P3 | S1
 P4 | S1
 P4 | S4
 P5 | S4
 P5 | S1
 P6 | S1
 P7 | 
(13 rows)
```
<!--
Note that the last row includes a NULL because P7 is new and has not shipped from any supplier.

We can do the same in TutorialD:
-->
```
TutorialD (master/main): insert p relation{tuple{p# "P7", pname "Widget", color "Beige", weight 21, city "Reykjavik"}}
TutorialD (master/main): :showexpr (p : {suppliers := (sp rename {p# as pid} where p#=@pid) {s#}}) {p#,suppliers}
┌────────┬──────────────────────────────┐
│p#::Text│suppliers::relation {s#::Text}│
├────────┼──────────────────────────────┤
│"P5"    │┌────────┐                    │
│        ││s#::Text│                    │
│        │├────────┤                    │
│        ││"S1"    │                    │
│        ││"S4"    │                    │
│        │└────────┘                    │
│"P6"    │┌────────┐                    │
│        ││s#::Text│                    │
│        │├────────┤                    │
│        ││"S1"    │                    │
│        │└────────┘                    │
│"P3"    │┌────────┐                    │
│        ││s#::Text│                    │
│        │├────────┤                    │
│        ││"S1"    │                    │
│        │└────────┘                    │
│"P1"    │┌────────┐                    │
│        ││s#::Text│                    │
│        │├────────┤                    │
│        ││"S1"    │                    │
│        ││"S2"    │                    │
│        │└────────┘                    │
│"P2"    │┌────────┐                    │
│        ││s#::Text│                    │
│        │├────────┤                    │
│        ││"S1"    │                    │
│        ││"S4"    │                    │
│        ││"S3"    │                    │
│        ││"S2"    │                    │
│        │└────────┘                    │
│"P7"    │┌────────┐                    │
│        ││s#::Text│                    │
│        │└────────┘                    │
│"P4"    │┌────────┐                    │
│        ││s#::Text│                    │
│        │├────────┤                    │
│        ││"S1"    │                    │
│        ││"S4"    │                    │
│        │└────────┘                    │
└────────┴──────────────────────────────┘
```
English: add a product to relvar "p" representing information about a "Widget". Then, show me all products and their related supplier information.

<!--
Let's briefly break down the TutorialD query. First, we add the `suppliers` attribute to the existing attributes of `p`. The new `suppliers` attribute is the relation result of matching `sp`'s part number to the part number from the `p` table. Here, we rename `p#` to `pid` in order to disambiguate the attribute names. Finally, we project on the `p#` and `suppliers` attributes.
-->

* show arrows pointing to attributes mentioned

<!--

Now, how about those results. Note that we have one tuple per part with the part number followed by a value with a relation containing all matching suppliers. So far, it's similar to data you might get from a join, but the supplier results are grouped into relation. Wait a second, what do you see at "P7"? We just added "P7" a minute ago, so it can't have any shipments to suppliers. In that case, an empty relation makes perfect sense. While in SQL, we have a row with a NULL supplier, here, we have a complete list of suppliers for each part- no NULLs required. This representation satisfies our original question much better than the SQL representation where the part-supplier matches are split across multiple rows- ew. If you want to learn more about how TutorialD handles missing data, jump to chapter "Good Riddance to Bad NULLs".
-->

* highlight P7 in tutd results
---

# Union

<!--
So far, we've covered joins and the equivalent to OUTER JOIN in SQL- let's look at some other fundamental relational operators.

UNION glues two tables together, but in SQL, did you know that UNION deduplicates rows? You would have to use UNION ALL to retain all rows from both tables. Witness:
-->
16.html

```SQL
SELECT * FROM s UNION SELECT * FROM s;
 s# | sname | status |  city  
----+-------+--------+--------
 S3 | Blake |     30 | Paris
 S2 | Jones |     10 | Paris
 S4 | Clark |     20 | London
 S1 | Smith |     20 | London
 S5 | Adams |     30 | Athens
(5 rows)

vs.
 
```SQL
SELECT * FROM s UNION ALL SELECT * FROM s;
 s# | sname | status |  city  
----+-------+--------+--------
 S5 | Adams |     30 | Athens
 S3 | Blake |     30 | Paris
 S1 | Smith |     20 | London
 S2 | Jones |     10 | Paris
 S4 | Clark |     20 | London
 S5 | Adams |     30 | Athens
 S3 | Blake |     30 | Paris
 S1 | Smith |     20 | London
 S2 | Jones |     10 | Paris
 S4 | Clark |     20 | London
(10 rows)

```

<!--
However, as we know, relations are sets of tuples, so it doesn't actually make sense to have duplicate tuples. 

In SQL, the notion of equality is muddied by the existence of NULL. For example, what would you expect if we UNION'd two tables with a row which includes a NULL? **Does this expression return one row or two?**
-->
17.html
```SQL
SELECT * FROM (VALUES (1,NULL)) as t(a,b) UNION SELECT * FROM (VALUES (1,NULL)) as t;
```
<!--
In fact, it returns one row because NULL is indeed equal to itself in specific circumstances in SQL. I guess you'll have to memorize which operators think NULL is equal to itself. Good luck!

In TutorialD, the union is clean and unambiguous. We'll see later how we can handle missing data, but NULL and its ternary logic are not needed nor desired. 

By the way, the logic in SQL is called "ternary" in reference to the boolean algebra SQL supports- every boolean expression in SQL can be valued as "true", "false", or "unknown/NULL". Standard boolean logic is binary- either "true" or "false".
-->

* show keyword "ternary logic alongside true+false+NULL" (not boolean logic)
---

# More Unions
18.html
```
TutorialD (master/main): :showexpr s union relation{tuple{city "Beijing", s# "S5", sname "Cui", status 15}}
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"Athens"  │"S5"    │"Adams"    │30             │
│"Paris"   │"S3"    │"Blake"    │30             │
│"Beijing" │"S5"    │"Cui"      │15             │
│"Paris"   │"S2"    │"Jones"    │10             │
│"London"  │"S4"    │"Clark"    │20             │
│"London"  │"S1"    │"Smith"    │20             │
└──────────┴────────┴───────────┴───────────────┘
```

English: show me all supplier info stored in relvar "s" along with an additional supplier "S5".



---

# More Unions 2
19.html
<!--
In fact, in relational algebra, any relation unioned to itself is an identity. Any `x union x` is equivalent to `x`, so the Project:M36 expression optimizer is free to replace any `x union x` with just `x`.
-->

```
TutorialD (master/main): :showexpr s union s  
┌──────────┬────────┬───────────┬───────────────┐
│city::Text│s#::Text│sname::Text│status::Integer│
├──────────┼────────┼───────────┼───────────────┤
│"London"  │"S4"    │"Clark"    │20             │
│"Athens"  │"S5"    │"Adams"    │30             │
│"London"  │"S1"    │"Smith"    │20             │
│"Paris"   │"S2"    │"Jones"    │10             │
│"Paris"   │"S3"    │"Blake"    │30             │
└──────────┴────────┴───────────┴───────────────┘
```
English: show me the result of gluing together tuples in relvar "s" with the tuples in relvar "s".

--- 

# Group
20.html
<!--
Next up are a bunch of operators which SQL can never support. Which operators do you think these are? 
Let's take a look. First up, the `group` operator rolls up tuples using a nested relation. This means that we look for matching tuples on a tuple-by-tuple basis. Let's look at an example.
-->

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
English: show me all supplier information but rolled up on a per-city basis.

<!--
Here, we roll up the relvar `s` to answer the query "Which suppliers are located in each city?" Again, this is different from SQL's GROUP BY operator which can only aggregate rows and collapse them into one row. In SQL, we could get close with an array aggregation:
-->

```SQL
SELECT city,ARRAY_AGG(ROW("s#",sname,status)) FROM s GROUP BY city;
  city  |             array_agg             
--------+-----------------------------------
 Paris  | {"(S3,Blake,30)","(S2,Jones,10)"}
 London | {"(S1,Smith,20)","(S4,Clark,20)"}
 Athens | {"(S5,Adams,30)"}
(3 rows)
```

<!--
but an array is not a relation- arrays have ordering and the relational operators cannot be applied.

The simplest query which gets us close enough uses a simple ORDER BY:
-->
21.html
```SQL
SELECT * FROM s ORDER BY city;
 s# | sname | status |  city  
----+-------+--------+--------
 S5 | Adams |     30 | Athens
 S1 | Smith |     20 | London
 S4 | Clark |     20 | London
 S3 | Blake |     30 | Paris
 S2 | Jones |     10 | Paris
```

---

# Ungroup
22.html
<!--
As you can imagine, the TutorialD `group` operator utilizing nested relations is great for representing hierarchical data- the top-level provides summary context while the nested relation provides detail.

Naturally, you would expect an inverse operator and, indeed, it is called `ungroup`.
-->

```
:showexpr (s group ({sname,status,s#} as subrel)) ungroup subrel
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

English: show me a per-city rollup of relvar "s" but then undo it.
<!--
Note, however, that this going from a nested relation model to an ungrouped version can lose information. Let's look at a previous example and apply `ungroup`:
-->

---

# P7 is Missing
23.html
```
:showexpr ((p : {suppliers := (sp rename {p# as pid} where p#=@pid) {s#}}) {p#,suppliers}) ungroup suppliers
┌────────┬────────┐
│p#::Text│s#::Text│
├────────┼────────┤
│"P2"    │"S4"    │
│"P2"    │"S2"    │
│"P1"    │"S2"    │
│"P5"    │"S4"    │
│"P2"    │"S1"    │
│"P3"    │"S1"    │
│"P6"    │"S1"    │
│"P1"    │"S1"    │
│"P4"    │"S4"    │
│"P2"    │"S3"    │
│"P4"    │"S1"    │
│"P5"    │"S1"    │
└────────┴────────┘
```

English: for all products, give me matching supplier-product (sp) supplier IDs as attribute "suppliers", then give me just the product ID and suppliers info from that. Finally, unroll the nested data into a flat relation.


<!--

"P7" is missing! **Why do you think that is?**

Well, if "P7" has no supplier, what would go into the "s#" attribute for it? Well, there is no concept of NULL, so nothing logical can be placed there. Therefore, the nested relation model actually contains more information than the flattened one. Thus, `group` and `ungroup` are not inverses as they cannot round-trip the data. We must conclude that nested relations are fundamental to the relational algebra since they enable us to answer questions about data associations.
-->
24.html
<--
Note that if flatten a grouped relation, we lose any mention of P7, just like the SQL version above. This demonstrates that group and ungroup are not inverses of each other and that an ungroup can cause data loss.
-->
<--

~~Finally, you may not have noticed, but SQL does not support relational equality.~~
**Did you notice SQL does not support relational equality?**
-->


---

# Equality
25.html
<!--
It seems perfectly natural to want to know if the results of two expressions are equal and it is in TutorialD!

Let's try it:
-->
```
TutorialD (master/main): :showexpr s = s
┌┐
││
├┤
└┘
```
English: tell me if relvar "s" has the same data as relvar "s".

26.html
<!--
Hm. I expected to see some sort of "true" value, but what is this result? Let's compare to a false result:
-->
```
TutorialD (master/main): :showexpr s = sp
┌┐
││
└┘
```
English: tell me if relvar "s" is equal to revlar "s" but without any tuples.

<!--
These two relvars cannot be equal since one some tuples and the other has zero.

So, true is two boxes and false is one box? Why? As it turns out, the relational algebra has two, fundamental relations which we call "true" and "false" in the relational algebra's context. We use these forms of truth and falsity because the relational algebra is a closed system- that means that a relational algebra expression can only return a relation. These true and false relations represent the result of a boolean evaluation within the context of relational algebra.

"True" is the relation with no attributes and one tuple in the body.

"False" is the relation with no attributes and zero tuples.

In order to help you distinguish them, these tables are the result of the query: "does relvar X have any tuples?"
-->
27.html
```
TutorialD (master/main): :showexpr s{}
┌┐
││
├┤
└┘
```
English: show me the result of relvar "s" if it had no attributes at all. Alternatively, tell me if relvar "s" has any tuples in its body.
28.html
```
TutorialD (master/main): :showexpr (s where false){}
┌┐
││
└┘
```
English: tell me if relvar "s" stripped of tuples has any tuples. It doesn't.
<!--
Recall that a relvar name followed by braces refers to a projection. In this case, we are projecting the relvar `s` on no attributes, so, if `s` has any tuples, all tuples are collapsed into a single tuple (because duplicates cannot be in a set). In the second expression, we remove all tuples and then project on the empty attribute set, so we end up with a relation with zero attributes and zero tuples. Thus, projection actually tells us if there are any tuples in the body, "true" for yes (it has tuples), "false" for no (zero tuples).
-->

---

# Equality 2
29.html
<!--
Now that we have defined some boolean primitives within the algebra, we can use them in relational equality.

So, how is relational equality useful? Well, we can check if a relvar meets our expectations; for example, is the set of `s#` values in `sp` equal to the `s#` values in `s`?

Are all the suppliers who appear in sp the same as the suppliers who appear in s?
-->

```
TutorialD (master/main): :showexpr sp{s#} = s{s#}
┌┐
││
└┘
```
English: tell me if all the supplier IDs in relvar "sp" are equivalent to the supplier IDs in relvar "s".

<!--

Ah, it's because supplier "S5" has never shipped a part. As you can guess, relational equality helps us to define database constraints. We'll cover those in a subsequent section. Let's take a look at how aggregations are done using the pure relational algebra.
-->

---

## Aggregate Functions


<!--
If you've been paying attention, you might realize that I haven't shown a means of collapsing multiple tuples into one using an aggregation. Aggregations are relatively easy-to-use in SQL:
30.html
-->
```SQL
SELECT city,COUNT(*) FROM s GROUP BY city;
  city  | count 
--------+-------
 Paris  |     2
 London |     2
 Athens |     1
(3 rows)

```
English: show me the number of suppliers in each city.

<!--

But let's consider what is actually happening. First, the execution engine must identify which cities are the same, then count how many times they appear. This process is not really relational since if we naively got a list of cities, **then they would necessarily be deduplicated: ALEX- ARE YOU SAYING YOU CAN'T DO AN AGGREGATE IN TUTORIALD THE SAME WAY YOU WOULD IN SQL?** 
-->

*Show me the cities in which I have suppliers.*
```
TutorialD (master/main): :showexpr s{city}
┌──────────┐
│city::Text│
├──────────┤
│"London"  │
│"Athens"  │
│"Paris"   │
└──────────┘
```
31.html
<!--
Hm, but we do have a tool to break a table into groups- the `group` operator!
-->

English: show me complete supplier information grouped by city.

```
TutorialD (master/main): :showexpr s group ({all but city} as subrel)
┌──────────┬───────────────────────────────────────────────────────┐
│city::Text│subrel::relation {s#::Text,sname::Text,status::Integer}│
├──────────┼───────────────────────────────────────────────────────┤
│"Paris"   │┌────────┬───────────┬───────────────┐                 │
│          ││s#::Text│sname::Text│status::Integer│                 │
│          │├────────┼───────────┼───────────────┤                 │
│          ││"S2"    │"Jones"    │10             │                 │
│          ││"S3"    │"Blake"    │30             │                 │
│          │└────────┴───────────┴───────────────┘                 │
│"Athens"  │┌────────┬───────────┬───────────────┐                 │
│          ││s#::Text│sname::Text│status::Integer│                 │
│          │├────────┼───────────┼───────────────┤                 │
│          ││"S5"    │"Adams"    │30             │                 │
│          │└────────┴───────────┴───────────────┘                 │
│"London"  │┌────────┬───────────┬───────────────┐                 │
│          ││s#::Text│sname::Text│status::Integer│                 │
│          │├────────┼───────────┼───────────────┤                 │
│          ││"S1"    │"Smith"    │20             │                 │
│          ││"S4"    │"Clark"    │20             │                 │
│          │└────────┴───────────┴───────────────┘                 │
└──────────┴───────────────────────────────────────────────────────┘
```


---

# Aggregate Functions 2
32.html
<!--
So, if we had a function which could count the number of tuples in a nested relation, we could count the number of matches. And that is what we can do!
-->

```
TutorialD (master/main): :showexpr s group ({all but city} as subrel) : {citycount := count(@subrel)}
┌──────────┬──────────────────┬───────────────────────────────────────────────────────┐
│city::Text│citycount::Integer│subrel::relation {s#::Text,sname::Text,status::Integer}│
├──────────┼──────────────────┼───────────────────────────────────────────────────────┤
│"Athens"  │1                 │┌────────┬───────────┬───────────────┐                 │
│          │                  ││s#::Text│sname::Text│status::Integer│                 │
│          │                  │├────────┼───────────┼───────────────┤                 │
│          │                  ││"S5"    │"Adams"    │30             │                 │
│          │                  │└────────┴───────────┴───────────────┘                 │
│"London"  │2                 │┌────────┬───────────┬───────────────┐                 │
│          │                  ││s#::Text│sname::Text│status::Integer│                 │
│          │                  │├────────┼───────────┼───────────────┤                 │
│          │                  ││"S1"    │"Smith"    │20             │                 │
│          │                  ││"S4"    │"Clark"    │20             │                 │
│          │                  │└────────┴───────────┴───────────────┘                 │
│"Paris"   │2                 │┌────────┬───────────┬───────────────┐                 │
│          │                  ││s#::Text│sname::Text│status::Integer│                 │
│          │                  │├────────┼───────────┼───────────────┤                 │
│          │                  ││"S2"    │"Jones"    │10             │                 │
│          │                  ││"S3"    │"Blake"    │30             │                 │
│          │                  │└────────┴───────────┴───────────────┘                 │
└──────────┴──────────────────┴───────────────────────────────────────────────────────┘

```
English (for above): show me complete supplier information grouped by city also including the supplier count per city.

<!--
And, here, we see again something that SQL cannot offer. Not only do we have the per-city aggregate counts, but the detailed supplier data. So we can summarize the data and request the source data in one query. Typically, this same thing would be achieved in two or more SQL queries. Oh, you don't need the original data? No problem, project it away:
-->
33.html
*Show me how many suppliers I have in each city.*
```
TutorialD (master/main): :showexpr (s group ({all but city} as subrel) : {citycount := count(@subrel)}) {all but subrel}
┌──────────┬──────────────────┐
│city::Text│citycount::Integer│
├──────────┼──────────────────┤
│"Paris"   │2                 │
│"London"  │2                 │
│"Athens"  │1                 │
└──────────┴──────────────────┘
```
---

# Aggregate Functions 3
34.html
<!--
What about if we want to see each city along with its maximum supplier status?:
-->
*Show me cities in which I have suppliers along with the maximum status for all suppliers in each city.*
```
TutorialD (master/main): :showexpr ((s{city,status} group ({all but city} as subrel)) : {maxstatus := max(@subrel)}) { all but subrel }
┌──────────┬──────────────────┐
│city::Text│maxstatus::Integer│
├──────────┼──────────────────┤
│"Athens"  │30                │
│"Paris"   │30                │
│"London"  │20                │
└──────────┴──────────────────┘
```

<!--
Again, we group on the attributes we want to summarize, then apply a summarizing function to it. Done! Naturally, TutorialD supports a bunch of other aggregate functions such as `min` and `sum`- what you would expect. How they are applied within the relational algebra is  different when compared to how they are applied in SQL.
-->

---

## Database Constraints

<!--
As I hinted earlier, relational equality helps us to implement database constraints. As it turns out, it has been proven that any database constraint can be implemented as an "inclusion dependency". This means that constraints are represented by two relational expressions: one which is a subset of the other:
-->

```
Constraint: r1 ⊆ r2
```
35.html
<!--
This is true in SQL, too, except that SQL does not allow one to define the inclusion dependency directly:
-->
```SQL
ALTER TABLE sp ADD CONSTRAINT fk_s FOREIGN KEY ("s#") REFERENCES s ("s#");
```
<!--
Here, we define a foreign key constraint. Logically, this means that the set of `s#` values in `sp` must be a subset of the set of `s#` values in `s`.
-->
36.html
*Show me all supplier IDs.*
```SQL
SELECT "s#" FROM s;
 s# 
----
 S5
 S3
 S1
 S2
 S4
(5 rows)
```
37.html
*Show me all unique supplier IDs.*
```SQL
SELECT DISTINCT "s#" FROM sp;
 s# 
----
 S1
 S2
 S4
 S3
(4 rows)
```

```
sp{s#} ⊆ s{s#}
```
<!--
By making this a constraint, we are asking the database to ensure that this property holds true at all times. The database must reject attempts to violate this constraint. For example, inserting unknown supplier "S9" into `sp` must fail:
-->
*Insert a row into suppliers-parts which includes a supplier not mentioned in the supplier table.*
38.html
```SQL
INSERT INTO sp("s#","p#",qty) VALUES ('S9','P4',5000);
ERROR:  insert or update on table "sp" violates foreign key constraint "sp_s#_fkey"
DETAIL:  Key (s#)=(S9) is not present in table "s".
```
39.html
<!-- Here, we use TutorialD to instruct the database to enforce a foreign key constraint between relvars "s" and "sp".
---

# Database Constraints 2

<!--
The SQL standard includes a CREATE ASSERTION expression which could be used to enforce database-wide constraints, but most DBMSes don't support or enforce it. Enforcement of constraints is, however, always database-wide, so it makes sense that TutorialD supports inclusion dependencies directly.
-->

*Create a foreign key on suppliers-parts which ensures that the supplier ID used is mentioned in the supplier relvar.*
40.html
```
TutorialD (master/main): foreign key s_sp_fk2 sp{s#} in s{s#};
```
<!--
Since we already have a foreign key on `s#` in `sp`, here we create a second one. No problem. This is a database wide constraint which happens to apply to one relvar, but we can make constraints across any number of relvars, not limiting us to foreign key constraints.
-->

41.html
*Show all constraints.*
```
TutorialD (master/main): :constraints
┌──────────┬──────────────────────────────────────────────────────────────────────────────────┬───────────┐
│name::Text│sub::Text                                                                         │super::Text│
├──────────┼──────────────────────────────────────────────────────────────────────────────────┼───────────┤
│"s_pkey"  │"(((true:{a:=s, b:=count(@a)}){b}) != ((true:{a:=s{s#}, b:=count(@a)}){b}))"      │"(false)"  │
│"s_sp_fk2"│"(sp{s#})"                                                                        │"(s{s#})"  │
│"sp_pkey" │"(((true:{a:=sp, b:=count(@a)}){b}) != ((true:{a:=sp{p#, s#}, b:=count(@a)}){b}))"│"(false)"  │
│"p_sp_fk" │"(sp{p#})"                                                                        │"(p{p#})"  │
│"p_pkey"  │"(((true:{a:=p, b:=count(@a)}){b}) != ((true:{a:=p{p#}, b:=count(@a)}){b}))"      │"(false)"  │
│"s_sp_fk" │"(sp{s#})"                                                                        │"(s{s#})"  │
└──────────┴──────────────────────────────────────────────────────────────────────────────────┴───────────┘
```

<!--
Take a look at `s_sp_fk2` which we just created. The `sub` attribute indicates the TutorialD expression which represents the subset of the values which must appear in the `super` attribute results. That's the definition of a foreign key relationship, so not surprising. 
-->

---

# Uniqueness Constraints
<!--
But what about those uniqueness (key) constraints? 
-->
*Wrap suppliers in a nested relation to count the number of suppliers.*
42.html
```
TutorialD (master/main): :showexpr true:{a:=s, b:=count(@a)}
┌─────────────────────────────────────────────────────────────┬──────────┐
│a::relation {s#::Text,sname::Text,status::Integer,city::Text}│b::Integer│
├─────────────────────────────────────────────────────────────┼──────────┤
│┌──────────┬────────┬───────────┬───────────────┐            │5         │
││city::Text│s#::Text│sname::Text│status::Integer│            │          │
│├──────────┼────────┼───────────┼───────────────┤            │          │
││"London"  │"S4"    │"Clark"    │20             │            │          │
││"London"  │"S1"    │"Smith"    │20             │            │          │
││"Athens"  │"S5"    │"Adams"    │30             │            │          │
││"Paris"   │"S2"    │"Jones"    │10             │            │          │
││"Paris"   │"S3"    │"Blake"    │30             │            │          │
│└──────────┴────────┴───────────┴───────────────┘            │          │
└─────────────────────────────────────────────────────────────┴──────────┘
```
<!--
What happening there is that the database counts the number of unique rows and makes sure that that count matches the count of the number of unique `s#` attribute values. Ah, so that's what it means to have a key- each tuple must have a unique value inside it. 

Using this scheme, we can create attribute checkers normally used in SQL on a per-column basis:
--->
43.html
*Create a new table y with an attribute a which must be greater than five.*
```SQL
CREATE TABLE y (a INTEGER CHECK (a > 5));
```
<!--
But, in the database constraint model, all constraints operate at the same level: database-wide instead of on a per-table basis. 
-->
44.html
*Create a constraint to ensure that the status of a supplier must be less than 50.*
```
TutorialD (master/main): constraint s_status_less_than_50 (s where gte(@status,50)){} in false
TutorialD (master/main): insert s relation{tuple{city "Calgary", s# "S9", sname "Stephens", status 51}}
ERR: InclusionDependencyCheckError "s_status_less_than_50" Nothing
```
<!--
After this constraint on `status` is added, a new tuple with `status` value 51 cannot be inserted into `s`. Note that this means that all constraints are at "the same level" in that none of them are glued to a table. In addition, this means that there is no such as a "primary key"- all keys are equal and treated the same.

Try it out! I'm sure there are many more interesting kinds of constraints you can discover.
-->

---

## Dataframes
<!--
Thus far, we have been focused on relations and the relational algebra- and for good reason! TutorialD captures the mathematics of relational algebra. Recall that a relation is a set of attributes mapped to a set of tuples with those attributes. But it doesn't make sense for a set to have an ordering- any ordering is completely arbitrary. In SQL, the concept of ordering is baked into SELECT statements:
-->
*Show me the tuple 1,2 followed by the tuple 2,NULL.*
45.html
```SQL
SELECT * FROM (VALUES (1,2),(2,NULL)) as x;
 column1 | column2 
---------+---------
       1 |       2
       2 |        
(2 rows)
46.html
SELECT * FROM (VALUES (1,2),(2,NULL)) as x(a,b) ORDER BY a DESC;
 a | b 
---+---
 2 |  
 1 | 2
(2 rows)
47.html
SELECT * FROM (VALUES (1,2),(2,NULL)) as x(a,b) ORDER BY b NULLS FIRST;
 a | b 
---+---
 2 |  
 1 | 2
(2 rows)
```
<!--
Ah, there's NULL again, this time confusing us in what ordering it should appear.

All this means that SQL's SELECT does not actually return a relation. Well, we sort of already knew that- tables can have duplicate rows, but tables can also be ordered or clustered around a specific ordering. Obviously, that's not compatible with the relational algebra.

Ordered results from a query are valuable, so how can we get such results out of a database when tuples in a relation have no ordering? We can convert the relations into another structure called "dataframes" where the ordering is honored.
-->

---

# Dataframes 2

*Show me suppliers ordered by status.*
48.html
```
TutorialD (master/main): :showdataframe s orderby {status}
┌──┬───────────┬─────────┬────────────┬────────────────┐
│DF│city::Text↕│s#::Text↕│sname::Text↕│status::Integer⬆│
├──┼───────────┼─────────┼────────────┼────────────────┤
│1 │"Paris"    │"S2"     │"Jones"     │10              │
│2 │"London"   │"S4"     │"Clark"     │20              │
│3 │"London"   │"S1"     │"Smith"     │20              │
│4 │"Athens"   │"S5"     │"Adams"     │30              │
│5 │"Paris"    │"S3"     │"Blake"     │30              │
└──┴───────────┴─────────┴────────────┴────────────────┘
```
<!--
Note the differences in representation. First, our tuples are numbered, then each attribute includes an additional arrow indicating what sort ordering is on each- the `status` attribute has an "up" arrow only which means we sorted specifically in that direction for it.
-->

49.html
*Show me supplier status in descending order but skip the first and one and show me only the next three.*
```
TutorialD (master/main): :showdataframe s{status} orderby {status descending} offset 1 limit 3
┌──┬────────────────┐
│DF│status::Integer⬇│
├──┼────────────────┤
│1 │20              │
│2 │10              │
└──┴────────────────┘
```
<!--
Here, we see that the dataframe syntax is very similar to the syntax of ORDER BY clauses in SQL.

Note that the dataframe is a terminal representation- that means that we cannot use a dataframe result in a relational expression because, again, a dataframe is not a relation, though SQL does not make a distinction. Dataframes are useful mostly for human consumption but are generally counter-productive in the context of a database engine.
-->

---

## Transactions

<!--
TutorialD covers the relational algebra, but we can extend it to support other database concepts. Let's compare how Project:M36 and SQL support transactions.
-->
50.html
```SQL
BEGIN;
COMMIT;
ROLLBACK;
```

```
TutorialD (master/main): :commit
TutorialD (master/main): :rollback
```
<!--
So far, the interfaces look the same, but how they operate is quite different. Project:M36 supports a git-like model for every database whereby the database is actually a branchable. This is great for testing out new database changes, A/B testing, or just making sure you're not stepping on anyone's toes.
-->

*Branch the database off the current branch to commit transactions without affecting the original branch.*
51.html
```
TutorialD (master/main): :branch debug_334
TutorialD (debug_334/main):
```
<!--
Aha, notice the changed prompt. We have created a new branch and switched to it. Now, if we add commits to the this branch, they won't be seen on the `master` branch. Once our feature branch is complete, we merge our changes back to `master`. We can switch back to the `master` branch with no issue.
-->
*Change the current session state to the head of the master branch.*
52.html
```
TutorialD (debug_334/main): :jumphead master
TutorialD (master/main): 
```
<!--
SQL's transaction model supports only the notion of a single, progressing database state over which all clients are contending to push their updates. In addition, SQL supports the notion of transaction isolation levels whereby the user can choose what sort of changes he might want to see from other transactions in the course of his own transaction progressing.
-->
53.html
```
begin;
SET TRANSACTION ISOLATION LEVEL READ COMMITTED;
```
<!--
Project:M36 only supports maximum transaction isolation as is required by any mathematically coherent system. Once your transaction is open, the state of the database will not change unless your own expressions change it. There are no concurrency anomalies to endure.
-->
* create visual to emphasize that no concurrency anomalies occur

---

## Good Riddance to Bad NULLs
<!--
I promised that I would address the issue of a database engine lacking NULLs entirely. Many database developers swear by the value of NULLs, but do they truly understand all the corner cases? Here's a quick quiz:
-->
54.html
* show 5 second countdown on each NULL quiz- show answer after x seconds
```
SELECT SUM(a) FROM (VALUES (3),(NULL)) as x(a);
 sum 
-----
   3
(1 row)

SELECT COUNT(a) FROM (VALUES (3),(NULL)) as x(a);
 count 
-------
     1
(1 row)

SELECT * FROM (VALUES (3),(NULL)) as x(a) WHERE a = NULL;
 a 
---
(0 rows)

SELECT * FROM (VALUES (3),(NULL)) as x(a) WHERE a IS NULL;
 a 
---
  
(1 row)

 SELECT * FROM (VALUES (3),(NULL)) as x(a) LEFT OUTER JOIN (VALUES (3),(NULL)) as y(a) ON x.a = y.a;
 a | a 
---+---
 3 | 3
   |  
(2 rows)

SELECT SUM(a) FROM (VALUES (NULL::int)) AS x(a);
 sum 
-----
    
(1 row)

SELECT SUM(a) FROM (SELECT status FROM s WHERE false) AS x(a);
 sum 
-----
    
(1 row)

SELECT AVG(a) FROM (VALUES (3),(NULL)) as x(a);
        avg         
--------------------
 3.0000000000000000
(1 row)

SELECT * FROM (VALUES (3),(NULL)) as x(a) WHERE a IN (NULL,3);
 a 
---
 3
(1 row)

SELECT * FROM (VALUES (3),(NULL)) as x(a) WHERE a NOT IN (1);
 a 
---
 3
(1 row)

SELECT CONCAT('uh oh', NULL);
 concat 
--------
 uh oh
(1 row)

SELECT 'uh oh' || NULL as unfortunate;
 unfortunate 
-------------
 
(1 row)

SELECT DISTINCT(a) FROM (VALUES (3),(NULL),(NULL)) as x(a);
 a 
---
 3
  
(2 rows)

SELECT a FROM (VALUES (3),(NULL),(NULL)) as x(a) GROUP BY a;
 a 
---
 3
  
(2 rows)

SELECT CASE WHEN a IS NULL THEN 5 ELSE a END FROM (VALUES (3),(NULL),(NULL)) as x(a) GROUP BY a;
 a 
---
 3
 5
(2 rows)

SELECT CASE WHEN a = NULL THEN 5 ELSE a END FROM (VALUES (3),(NULL),(NULL)) as x(a) GROUP BY a;
 a 
---
 3
  
(2 rows)

CREATE TABLE y(a INTEGER, UNIQUE(a));
CREATE TABLE
INSERT INTO y(a) VALUES (NULL),(NULL),(3);
INSERT 0 3
table y;
 a 
---
  
  
 3
(3 rows)

```

<!--
How many of these did you get right? If you got any wrong, you might have left a landmine in some old codebase. Good luck finding all the places where NULL's ternary logic might bite you!
-->


---

# NULLs 2

57.html (reordering hiccup)
<!--
So how can we represent missing or otherwise unrepresentable data? With data! Just like in non-SQL programming languages which don't include ternary logic, we can encode optional attributes using simple data types.
-->
```
TutorialD (master/main): :showexpr relation{tuple{name "Steve", age Just 25},tuple{name "Bob", age Nothing}}
┌───────────────────────┬──────────┐
│age::Maybe (a::Integer)│name::Text│
├───────────────────────┼──────────┤
│Nothing                │"Bob"     │
│Just 25                │"Steve"   │
└───────────────────────┴──────────┘
```
<!--
If you are familiar with Haskell, then this should look very familiar. However, the Maybe type doesn't really tell us why the data is missing. Let's remedy that:
-->

---

# NULLs 3
55.html
```
TutorialD (master/main): data Age = Age Integer | DeclinedToAnswer | Dead
TutorialD (master/main): e:=relation{tuple{name "Steve", age Age 25},tuple{name "Bob", age DeclinedToAnswer}, tuple{name "Ghostman", age Dead}}
TutorialD (master/main): :showexpr e
┌────────────────┬──────────┐
│age::Age        │name::Text│
├────────────────┼──────────┤
│DeclinedToAnswer│"Bob"     │
│Age 25          │"Steve"   │
│Dead            │"Ghostman"│
└────────────────┴──────────┘
```
56.html
*Show me all employees except those who are dead.*
```
TutorialD (master/main): :showexpr e minus e where age = Dead
┌────────────────┬──────────┐
│age::Age        │name::Text│
├────────────────┼──────────┤
│DeclinedToAnswer│"Bob"     │
│Age 25          │"Steve"   │
└────────────────┴──────────┘

```
<!--
We can leverage basic algebraic data types (ADTs) to represent missing data in much more interesting ways than NULL could ever dream. In return, we can model data much more accurately and with standard boolean logic instead of SQL's crusty and error-prone ternary logic.
-->

---

## Wrapping it Up
<!--
In this video, we explained some ways how SQL fails to implement the relational algebra while presenting a different query language called "TutorialD". Along the way, we learned some new ways to execute relational expressions in lieu of SQL's deviations from the math.

However, I did not explain why sticking to the math of relational algebra is important. 
-->
* regular Darin

### New Optimizations
<!--
Consider that by taking shortcuts such as allowing duplicate rows in a table, we are leaving interesting optimizations behind. For example, if we can safely assume that a relation has no duplicate tuples, then a b-tree index on its key cannot have conflicts. Furthermore, the tuple in a relation is itself a key since it must be unique! We expect that many more new optimizations are yet to be discovered!
-->
* show b-tree index visual

---

### Developer Understanding
<!--
SQL is complicated enough even without NULL's ternary logic. By sticking to the math, we improve consistency in the engine and results. This results in fewer surprises and sharp edges in the language.
-->
* show NULL x'd out logo

---

### Idempotence
<!--
We have not covered this in this video, but Project:M36 specifically requires all operations to be pure and without side-effects. "Pure" means that any operation on the database can be run any time, anywhere without a change in the result. This enables new means of database replication. In addition, while not required by TutorialD, Project:M36 implements a diff-tree-based transaction graph which would not be achievable without pure database update expressions.
-->
* show priest during ceremony with data blobs moving about

### Data Type Impedance Fix
<!--
The often lossy conversion between SQL types and python types has burned me more than once. As a quick example, I have seen strings in python become unreadable in the database because the string encoding was different. Oops! The problem is that types in the client language are not the same as the types in the database. Combine this with ternary logic and you have an impending disaster with every query. Project:M36 supports client-side and server-side programming in Haskell with full support for Haskell data types being stored and queried natively within the database. Goodbye type mismatch problems!
-->

* show one side with SQL types NUMERIC, INTEGER, VARCHAR and python types Decimal, int, str on the other

### Conclusion

<!--
TutorialD is a viable replacement for SQL, but it will take a shift in developer philosophy for developers to seek out and value math-oriented software rather than settling due to "practical considerations". As Project:M36's motto states: "Software can always be made faster, but rarely can it be made more correct." Logically, but cutting corners on software, we all paint ourselves into corners.
-->

* show "software can..." motto

<!--
Project:M36 hopes to remedy the stagnant state of relational database engines by providing a new meeting point of mathematics and practical implementation considerations, this time skewing towards the math.

We hope you enjoyed learning about the relational algebra and TutorialD. If you would like to learn more, check out these links to documentation and essays below. If you want to ask me something, catch me at [Jumpstartups](https://jumpstartups.io). Thanks for watching!
-->

* regular Darin

* [Project:M36 Main Page](https://github.com/agentm/project-m36)
* [Out Of the Tarpit](http://curtclifton.net/papers/MoseleyMarks06a.pdf) - proposes a unified architecture around the relational algebra
* [Reaching Out of the Tarpit](https://github.com/agentm/project-m36/blob/master/docs/reaching_out_of_the_tarpit.markdown) - an essay on the importance of the original paper and how Project:M36 implements the recommendations
* [On Null](https://github.com/agentm/project-m36) - a diatribe on SQL's NULL disaster
* [TutorialD Cheatsheet](https://github.com/agentm/project-m36/blob/master/docs/tutd_cheatsheet.markdown)
* [TutorialD Tutorial](https://github.com/agentm/project-m36/blob/master/docs/15_minute_tutorial.markdown) - covers much of the same ground as this video
