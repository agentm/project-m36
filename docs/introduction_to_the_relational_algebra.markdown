# Introduction to the Relational Algebra
## What is the relational algebra?
### History
The relational algebra was originally developed by [E. F. Codd](https://en.wikipedia.org/wiki/Edgar_F._Codd) working at IBM in the late 1960s as a proposed method by which database management system could present a unified interface to organization and querying of data. This relational model has been refined over the decades since and represents the state-of-the-art for modeling data used in computerized information processing.

### Definition
As implied by the name "algebra", the relational algebra is a mathematical model for modeling data and the links between data; it is not a specific software design or software product. A specific database management system may choose to label itself "relational", but this need not imply that it adheres to the relational algebra, rather, many existing "relational" software products find a medium ground with legacy features. SQL-based database management systems in particular do not adhere to the relational algebra.

The purpose of a database is to model a small sliver of reality. Data present in the database make boolean logic statements about the nature of our reality, while data not present in the database makes statements to the contrary. This is called the "closed world assumption". For example, if we have a database of employees, the database can unequivocally state whether or not "Steve" is an employee. If that answer does not align with reality, that is because the database's notion of the current state has not been updated. After all, a database can only model reality- it cannot be directly tied into reality.

A relational algebra system must support the notion of relations and relational operators. What makes the relational model "algebraic" is the ability to compose the relational operators to make arbitraily complex operator expressions.

Let us begin by starting with the smallest units in the relational model and work upwards.

#### Value

A value is a typed, singular unit of data.

Common examples could be a number indicating a person's age, the number of dollars in a bank account, or a street address used for delivery. In all cases, a value must be of a specific type and must support the notion of equality.

As an example, it doesn't make sense to ask if a person's age is equal to a street address; the relational model can enforce this- this is called "type safety" and is the motivation for using types.

Note that a value can also be a relation. This creates a recursively-defined data structure. More on this later.

Examples:
* A number:

3

* A name:

|Adam|
|----|

### Attribute

An attribute is a pair containing an attribute name along with an attribute type. An attribute name allows the relational model to uniquely identify values in a tuple or relation, therefore the attribute name must be unique within a relation and tuple.

Examples:
* An "Address" attribute name is of type "Text":

(Address, Text)

* An "Current Balance" attribute name is of type "Dollar Amount":

(Current Balance, Dollar Amount)

#### Tuple

A tuple is a set of attribute names mapped to values. The tuple represents a statement of "truth" in the database. For example, a tuple containing ((Name, Bob),(Address, 123 Main St.),(Age, 45)) could be used to state the logical proposition "A person named Bob lives at 123 Main St. and is 45 years old." However, that is not the only interpretation; the same tuple could be used to represent the proposition "a pet tarantula named Bob is treated by a veterinarian who was aged 45". Current database management systems make no attempt to interpret or store the propositions, so the interpretation is at the sole discretion of the human modeler.

Examples:
* "Cindy deposited $45.30 into account C at 4:15 PM":

((Name, Cindy), (Amount, +$43.30), (Time, 4:15 PM))

* "A car with license plate XYZ-123 failed the inspection due to rust":

((License Number, XYZ-123), (Reason, Rust))

In the last example, the fact that the car failed the inspection is implied by the existence of the tuple in a relation.

#### Relation

A relation is a container of values organized by tuples of the same type. Specifically, a relation is a set of attributes (a relation header) mapped over a set of tuples (a relation body). Note that this implies that tuples cannot appear more than once since a set can only hold unique items.

When a tuple appears in a relation, a relation's attributes and respective types must match that of all of its tuples. Thus, a relation's header summarizes the attribute names and types of all of its constituent tuples.

If a tuple represents a logical proposition, then a relation represents a logical predicate. Thus, a relation contains all the propositions born from the predicate which are true. The absence of a tuple in a relation implies a false proposition.

Since both header and body are sets of attributes and tuples, respectively, neither the header nor body have any ordering. However, when we approximate a relation, we must choose an arbitrary ordering.

A relation can also be thought of as an n-dimensional graph (each dimension defined by an attribute), where each value represents one dimension of a point in the graph.

Examples:
* Predicate: "A supplier SNO supplies part PNO in quantity QTY"
* Propositions: "Supplier S1 supplies part P1 in quantity 300", "Supplier S2 supplies part P1 in quantity 250"

|(SNO, Supplier Number Type)|(PNO,Parts Number Type)|(QTY, Quantity Type)|
|---|---|---|
|S1 |P1 |300|
|S2 |P1 |250|

* Predicate: "A person named Name is an employee and works in department Department."
* Propositions: "A person named Steve is an employee and works in the Accounting department."

|(Name, Name Type)|(Department, Department Type)|
|-----------------|-----------------------------|
|Steve            |Accounting                   |

Note that the absence of a tuple representing "Bob" indicates that Bob is not an employee and not that, for example, Bob could be an employee in an unspecified department.

### Relation Variables

In order to reference relations with a specific, albeit arbitrary name, relation variables map such names to relations, much like an integer variable references a specific, immutable integer. Relation variables vary over state changes, thus a single relation variable may reference an arbitrary number of immutable relations over its lifetime. However, each relation variable can only reference relations of the same type. The type of a relation can be represented by a relation's header (its set of attributes).

When a relation variable is changed, it references a new, immutable relation; relations themselves cannot be modified/mutated.

Examples:
* At state T1, Bob is an employee. At state T2, Bob and Steve are employees.

T1: (Employees, (Name, Name Type)) ->

|(Name, Name Type)|
|-----------------|
|Bob              |

T2: (Employees, (Name, Name Type)) ->

|(Name, Name Type)|
|-----------------|
|Bob              |
|Steve            |

To reiterate, the transition from T1 to T2 replaced the "Employees" relation variable reference found at T1 with the reference to the new relation found at T2.

#### Relational Operators

Since we refer to the relational algebra as a closed algebra, all relational operators necessarily operate on and generate relations. The purpose of the operators is allow data modelers to answer questions (queries) based on the current state of the database. For example, a modeler may wish to know the ages of the employees working in the accounting department. The modeler can then compose the relational operators together to answer this and virtually any other conceivable query.

##### Project

The projection operator's input is a set of attribute names and a relation and outputs the relation with the input set of attribute names. Logically, projection removes attributes from a relation (header and body).

Example:
* What are the names of all my employees?

Project ("Name") in

|(Name, NameType)|(Department, Department Type)|(Room Number, Room Number Type|
|----------------|-----------------------------|------------------------------|
|Steve|Marketing|445|
|Bob|Printing|112|

===>

|(Name, NameType)|
|----------------|
|Steve|
|Bob|

##### Restrict

The restriction operator's input is a relation along with a predicate whose purpose is to filter the relation body's tuple set. This operator answers queries such as "which employees work in the marketing department?" or "which employees' names start with the letter 'S'?"

Examples:
* Find all fruit products.

Restrict (tuples where attribute "Fruit" is true) in

|(Product Name, Product Name Type)|(Is Fruit, Fruit Boolean Flag)|(Price, Price Type)|
|---------------------------------|------------------------------|-------------------|
|Orange|True|$2.50|
|Pineapple|True|$5.00|
|Cucumber|False|$3.00|

===>

|(Product Name, Product Name Type)|(Is Fruit, Fruit Boolean Flag)|(Price, Price Type)|
|---------------------------------|------------------------------|-------------------|
|Orange|True|$2.50|
|Pineapple|True|$5.00|

##### Rename

The rename operator takes a relation argument and a pair of attribute names (A1,A2) and returns a new relation with all appearances of attribute name A1 now appearing as attribute name A2.

This operator is mostly useful in composing other relational operators together.

Examples:
* Rename the "Age" attribute to be "Years Old".

|(Name, Name Type)|(Age, Age Type)|
|-----------------|---------------|
|Bob|42|
|Jen|55|

===>

|(Name, Name Type)|(Years Old, Age Type)|
|-----------------|---------------------|
|Jen|55|
|Bob|42|

Note that rename does not alter the type of the attribute in question.

##### Join

One of the most essential-to-understand operators is the join operator. Unlike the previous two unary (single relation argument) operators, join operates on two (or potentially more) relations. The purpose of a join is to merge multiple relations into one relation in order to determine the relationships between data across relations. For example, there are good reasons (which have not yet been explained) why a relation representing a company's products and records of when those products were shipped should be two disparate relations. The join operators merges relation A and relation B by returning a new relation with the attribute sets of both relation headers merged and with a body of the tuples merged if-and-only-if the in-common attributes' values are equal.

In the case where no attributes are in common between the relations, the join operator becomes a cross-product operator; this is not a special case, rather a logical consequence of the definition.

Examples:
* Find all course information and their respective instructors' information. An instructor may teach more than one course, but each course has one instructor.

Join (Instructor relation)

|(Instructor Name, Instructor Name Type)|(Office Number, Office Number Type)|
|----------------------------------------|----------------------------------|
|Steve|Office 45|
|Bob|Office 30|

(Course Relation)

|(Course Name, Course Name Type)|(Instructor Name, Instructor Name Type)|(Course Room Number, Room Number Type)|
|-------------------------------|----------------------------------|--------------------------------------|
|English 1|Steve|Room 104|
|French 2|Bob|Room 103|
|English 2|Steve|Room 104|

["Instructor Name" is the in-common attribute]
===>

|(Instructor Name, Instructor Name Type)|(Office Number, Office Number Type)|(Course Name, Course Name Type)|(Course Room Number, Room Number Type)|
|---------------------------------------|-----------------------------------|-------------------------------|--------------------------------------|
|Steve|Office 45|English 1|Room 104|
|Steve|Office 45|English 2|Room 104|
|Bob|Office 30|French 2|Room 103|

Note that "Steve" and his "Office 45" appear twice in the resultant relation. This is called "data duplication".

##### Union

The union operator takes two (or more) relations of the same type (relations with identical headers) and returns a new relation whose body is a sum of the tuples from both relations. Note that tuples cannot appear twice, so (A union A) always equals A.

Examples:
* Find all parents and children.

union (Parents relation)

|(Name, Name Type)|(Age, Age Type)|
|-----------------|---------------|
|Bob|45|
|Cindy|32|

(Children relation)

|(Name, Name Type)|(Age, Age Type)|
|-----------------|---------------|
|Steve|12|
|Tom|11|

===>

|(Name, Name Type)|(Age, Age Type)|
|-----------------|---------------|
|Steve|12|
|Cindy|32|
|Bob|45|
|Tom|11|

##### Group

The group operator (not to be confused with SQL's "GROUP BY") creates a sub-relation for the named attributes and collapses the unnamed attributes where the projected tuples are equal. It can be used for aggregate queries whereby the original and aggregate values can be returned. The group operator does not imply any aggregation, but can be used to create one.

Examples:

* Retrieve and group people who are of the same age.

group ("Name") as "SameAgePeople"

| (Name, Name Type)|(Age, Age Type) |
|-----------------|---------------|
|Steve|12|
|Cindy|12|
|Bob|45|
|Tom|12|

===>

|(SameAgePeople, relation (Name, Name Type)) | (Age, Age Type) |
|------------------------------------|-----------------|
|<table><tr><th>(Name, Name Type)</th></tr><tr><td>Steve</td></tr><tr><td>Cindy</td></tr><tr><td>Tom</td></tr></table>| 12|
|<table><tr><th>(Name, Name Type)</th></tr><tr><td>Bob</td></tr></table>|45|

Note that the new type of "SameAgePeople" is relation-based type. This can be thought of as a nested relation within a relation.

##### Ungroup

Ungroup unwraps a relation-valued attribute into the top-level relation.

Examples:

* Given the relation containing people's name mapped to their pets' names, create a relation whereby each person's name is mapped directly to each pet name.

ungroup ("Pets")

|(Name, Name Type)| (Pets, relation (PetName, Name Type))|
|-----------------|--------------------------------------|
|Cindy|<table><tr><th>(PetName, Name Type)</th></tr><tr><td>Smurfy</td></tr><tr><td>Stretchy</td></tr></table>|
|Bob|<table><tr><th>(PetName, Name Type)</th></tr></table>|
|Sam|<table><tr><th>(PetName, Name Type)</th></tr><tr><td>Socks</td></tr></table>|

===>

|(Name, Name Type)|(PetName, Name Type)|
|-----------------|--------------------|
|Cindy|Smurfy|
|Cindy|Stretchy|
|Sam|Socks|

Note that Bob does not appear in the result relation because he has no pets.

#### Difference

The difference operator ("minus" in TutorialD) receives two relations of the same type (same attributes) as arguments rvA and rvB and returns a relation which contains tuples in rvA which are not found in rvB. This operator is similar to set difference.

Example:

* Show a relation of non-managers by starting with the relation of employees and removing the employees found in the manager relation.

Employees

|(Name, Name Type)|
|-----------------|
| Bob |
| Jim |
| Steve |
| Bart |

Managers

|(Name, Name Type)|
|-----------------|
| Bob |
| Steve |

====>

|(Name, Name Type)|
|-----------------|
| Jim |
| Bart |

### Displaying Relations

Note that in the above operator examples, the order of the tuples appears to shift around. Since a relation body is a set of tuples, any ordering, such as required to represent a relation in two-dimenstional space as a table, is completely arbitrary. Note that the following two relations are two representations of *same* relation:

|(Name, Name Type)|(Age, Age Type)|
|-----------------|---------------|
|Bob|40|
|Sam|41|
|Cindy|42|

and

|(Name, Name Type)|(Age, Age Type)|
|-----------------|---------------|
|Sam|41|
|Cindy|42|
|Bob|40|

The relation header is a set of attributes, so the attributes also have no ordering. Here is a third representation of the above relation:

|(Age, Age Type)|(Name, Name Type)|
|---------------|-----------------|
|42|Cindy|
|40|Bob|
|41|Sam|

In general, an n-dimensional relation can be represented by an n-dimensional graph whereby each point in n-dimensional space is represented by each tuple.

## Why is the relational algebra important?

Unlike hierarchical or "document" databases, the relational algebra offers the best option for managing data sets while allowing queries of arbitrary complexity. The relational algebra is built on solid mathematics instead of design-by-commitee or legacy mistakes. Still, there are many exciting opportunities to expand study of the relational algebra; for example, in query optimization, data modeling principles, and distributed databases.

## Why do existing databases not adhere to the relational algebra?

Database management systems existed before the relational algebra existed or was fleshed out. Many database management products built "relational features" into pre-existing database software with legacy requirements where, inevitably, impedance mismatches led to divergence from the mathematics of the relational algebra. Today, the most prominent "relational" databases are based on SQL which is maintained as a standard by a commitee of legacy database product companies, not mathematicians. While SQL continues to evolve, it is quite clear it will never adhere to the mathematical principles of the relational algebra due to legacy concerns.

## What is the advantage of adhering to the relational algebra?

For most companies, its data, such as customer, employee, sales, and product information is its lifeblood. To entrust that data to unprincipled database management systems or worse, proprietary database software, is self-defeating. By providing a firm mathematical foundation on which to build their internal information technology infrastructure, the relational algebra reduces cross-product integration headaches, missing feature workarounds, and reliance on proprietary technology.

## What is the advantage of having relation-valued attributes?

Since a value can store any (immutable) type, it can certainly hold a relation. At the very least, the mathematics does not exclude the possibility. This does create a recursively-defined data structure (Relation -> Tuple -> Value -> Relation), but it turns out that relations as values can be quite useful.

For example, in SQL, if one wants to collect some aggregate data over some tuples while still returning the individual tuples, in the best case, the aggregate information must be repeated for every individual tuple. In the worst case, two separate queries are required since nested tables are not supported.

In the relational algebra, The aggregate value can be placed alongside the relation value from which the aggregate was derived.

Example:

* Find the average salary per department along with the employees in each department.

(Employees relation)

|(Name, Name Type)|(Salary, Salary Type)|(Department, Department Type)|
|-----------------|---------------------|-----------------------------|
|Steve|$32000|Marketing|
|Bob|$40000|Marketing|
|Sam|$41000|Sales|

(relational algebra magic)

====>

|(Department, Department Type)|(Average Salary, Salary Type)|(Employees, (Name, Name Type))|
|-----------------------------|-----------------------------|------------------------------|
|Marketing|$36000|<table><tr><th>(Name, Name Type)</th></tr><tr><td>Steve</td></tr><tr><td>Bob</td></tr></table>|
|Sales|$41000|<table><tr><th>(Name, Name Type)</th></tr><tr><td>Sam</td></tr></table>|

## Suggested Reading

Chris Date has been researching the relational model and its consequences for over forty years. Date has published his findings in various papers but also summarizes his work in a series of introductory and advanced books. Date freely admits that his work on the relational algebra has been corrected over time, so it is best to read his latest works.

Despite that fact that modern SQL databases do not adhere to the principles of relational algebra doesn't mean that the principles cannot be applied. Date often discusses and compares SQL constructions with his own "TutorialD" language. Project:M36 also supports TutorialD.

Introductory texts:
* [SQL and Relational Theory:
How to Write Accurate SQL Code](http://shop.oreilly.com/product/0636920022879.do)


Advanced texts:
* [Database Design & Relational Theory](http://shop.oreilly.com/product/0636920025276.do)
* [Database Explorations](http://bookstore.trafford.com/Products/SKU-000177853/Database-Explorations.aspx)
* [View Updating and Relational Theory](http://shop.oreilly.com/product/0636920028437.do)
* [Time and Relational Theory](http://shop.oreilly.com/product/9780128006313.do)
