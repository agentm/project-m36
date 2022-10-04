# Project:M36 Isomorphic Schemas

## Introduction

Project:M36 implements a unique feature called "isomorphic schemas" which allow database users to create any number of additional schemas which are completely isomorphic to the original schema.

Two schemas which are isomorphic can have different structure, such as different relation variables, but actually represent identical information. This means that both schemas contain the same data, but the database structures such as relations are defined distinctly and in such a way as to make the schemas serve potentially different purposes. Tautologically, two schemas which are identical are isomorphic to each other.

## Towards Solving Action-at-a-Distance

Isomorphic schemas were designed to handle the use-case commonly covered by SQL views. SQL views allow a database user to create named SQL macros in a database schema. With many caveats, these macros can also be made "updateable", i.e. views can be the table targets of UPDATE, INSERT, or DELETE expressions.

Chris Date elucidates on the numerous caveats involved in updateable SQL views in his "View Updating & Relational Theory" book. Of specific importance is the violation of the "Principle of Orthogonal Design" which states that two relation variables in a database should not represent the same data. Using SQL views as tables clearly violates this principle since a view can only create a duplicate view of the existing data. This same principle is violated even with materialized views.

Furthermore, SQL views violate the "Principle of Interchangeability" because of the special nature of views. Whether views are updateable or not is at the discretion of the database user. Some SQL views are proveably not updateable such as with summary or report views which intentionally collate data. Therefore, views do not behave identically to tables/relation variables.

SQL views violate "The Information Principle" because they represent variable storage which may behave outside the realm of what a table/relation variable could accomplish. A simple example is an SQL view which does *not* change the database state on an UPDATE (ON UPDATE DO NOTHING). A more complex example could involve a one-way hash function against database values which would be impossible to invert and thus prevent a logical update.

Updateable SQL views necessarily allow for "action at-a-distance", i.e. when a view is updated, an unknown number of actual tables may be altered in unanticipatable ways.

Isomorphic schemas were designed to address these violations with a mathematically-coherent design.

## Definition

Given a schema A containing relation variables, a given schema B is isomorphic to schema A if-and-only-if all relational expressions which can be written against schema B can be mechanically transformed to perform an operation on schema A which returns the same result relation.

## The Use Case

Despite their flaws, SQL views are useful for several reasons:

 * as an SQL macro language to store commonly-used queries
 * as a means of hiding schema complexity; a set of views can be used to aggregate data across many tables, for example, to denormalize a set of table data
 * as a means to contain different access methods to the database as the database evoles, i.e. the database schema needs to be altered, but the old access strategies can be emulated through views
 * as a means of enforcing security access by implementing views which offer a restricted subset of the original data

Of these use cases, isomorphic schemas only cover the schema complexity and database evolution use cases. Further research (see Conclusion) may be able to extend this design to the missing use cases.

## Isomorphic Schema Building Blocks

Project:M36 does not purport to be able to represent all possible isomorphic schemas, rather Project:M36 includes a set of "building blocks" from which one can create isomorphic schemas. These building blocks represent transformations needed to shift contexts from one schema to another. Currently, the building blocks are:

* rename: a relation variable in schema A may have a different name in schema B
* restrict: a relation variable in schema A can be split into two relation variables in schema B given a restriction predicate
* union: two relation variables in schema A can be union'd and represent one

Note that the restrict and union building blocks are inverse operations to each other.

Clearly, there are additional limitations placed on the relation variables that can be used in the building blocks; for example, for two relations to be union-able, they must have the same header and no tuple overlap.

## Isomorphic Schemas in Practice

Project:M36 is the first DBMS to implement isomorphic schemas. By default, each transaction is given a special "main" schema which cannot be deleted. To create a new isomorphic schema, use the "addschema" command:

```
:addschema <schema name> (<isomorphic building block>,...)
```

where the building blocks are one of:

* ```isorestrict "<new schema relvar name>" "<existing schema rel var where predicate is true>" "<existing schema rel var where predicate is false>" <predicate>```
* ```isounion "<new schema relvar name where predicate is true>" "<new schema relvar name where predicate is false>" "<existing schema relvar name>" <predicate>```
* ```isorename "<new schema relvar name>" "<existing schema relvar name>"```
* ```isopassthrough "<relvar name to retain in both existing and new schemas>"```

This list of building blocks is merely what Project:M36 supports and is not meant to represent an exhaustive list of potential, mathematically-sound isomorphisms.

To switch to an alternative schema, use the ```:setschema``` command:

```:setschema <schema name>```

To delete a schema, use the ```:removeschema``` command:

```:removeschema <schema name>```

The special "main" schema cannot be deleted.

Full example:
```
TutorialD (master/main): x1:=relation{tuple{y 3}, tuple{y 4}}
TutorialD (master/main): x2:=relation{tuple{y 5}, tuple{y 6}}
TutorialD (master/main): :addschema spam (isorestrict "x" "x1" "x2" lt(@y,5), isopassthrough "true", isopassthrough "false")
TutorialD (master/main): :setschema spam
TutorialD (master/spam): :showexpr x
┌──────────────┐
│y::IntAtomType│
├──────────────┤
│4             │
│5             │
│3             │
│6             │
└──────────────┘
TutorialD (master/spam): :showrelvars
┌─────────────────────────────────────────────────────────────────┬──────────────────┐
│attributes::relation {attribute::TextAtomType,type::TextAtomType}│name::TextAtomType│
├─────────────────────────────────────────────────────────────────┼──────────────────┤
│┌───────────────────────┬──────────────────┐                     │"true"            │
││attribute::TextAtomType│type::TextAtomType│                     │                  │
│└───────────────────────┴──────────────────┘                     │                  │
│┌───────────────────────┬──────────────────┐                     │"false"           │
││attribute::TextAtomType│type::TextAtomType│                     │                  │
│└───────────────────────┴──────────────────┘                     │                  │
│┌───────────────────────┬──────────────────┐                     │"x"               │
││attribute::TextAtomType│type::TextAtomType│                     │                  │
│├───────────────────────┼──────────────────┤                     │                  │
││"y"                    │"IntAtomType"     │                     │                  │
│└───────────────────────┴──────────────────┘                     │                  │
└─────────────────────────────────────────────────────────────────┴──────────────────┘
TutorialD (master/spam): :setschema main
TutorialD (master/main): :showrelvars
┌─────────────────────────────────────────────────────────────────┬──────────────────┐
│attributes::relation {attribute::TextAtomType,type::TextAtomType}│name::TextAtomType│
├─────────────────────────────────────────────────────────────────┼──────────────────┤
│┌───────────────────────┬──────────────────┐                     │"true"            │
││attribute::TextAtomType│type::TextAtomType│                     │                  │
│└───────────────────────┴──────────────────┘                     │                  │
│┌───────────────────────┬──────────────────┐                     │"x1"              │
││attribute::TextAtomType│type::TextAtomType│                     │                  │
│├───────────────────────┼──────────────────┤                     │                  │
││"y"                    │"IntAtomType"     │                     │                  │
│└───────────────────────┴──────────────────┘                     │                  │
│┌───────────────────────┬──────────────────┐                     │"false"           │
││attribute::TextAtomType│type::TextAtomType│                     │                  │
│└───────────────────────┴──────────────────┘                     │                  │
│┌───────────────────────┬──────────────────┐                     │"x2"              │
││attribute::TextAtomType│type::TextAtomType│                     │                  │
│├───────────────────────┼──────────────────┤                     │                  │
││"y"                    │"IntAtomType"     │                     │                  │
│└───────────────────────┴──────────────────┘                     │                  │
└─────────────────────────────────────────────────────────────────┴──────────────────┘
```

Note that for schema to be isomorphic, each relation variable in the existing schema must be referenced in the building blocks, otherwise an error is thrown which prevents the creation of a non-isomorphic schema.

If not all relation variables were referenced in the proposed isomorphism, then the schema cannot be isomorphic. Consider a schema with two relation variables, ```A``` and ```B``` and a foreign key constraint ```X``` between them. If a new schema were to be created with only ```A``` appearing, given an insert to ```A``` which violates the constraint ```X```, it would be possible to issue an insert to ```A``` which would violate a constraint on a relation variable which does not appear to exist. Therefore, such a schema would be not be isomorphic and the schema would violate the both the Closed World Assumption and the Principle of Interchangeability.

While it would be possible to create sub-isomorphic schemas mentioning a subset of all relation variables which do not have constraints between them, this has not been implemented because most schemas typically have constraints linking all relation variables together. This could be an area of further research.

## More on Use Cases

Instead of SQL views which model individual relation variables, by taking the notion of a "view" to a higher level in the database structure hierarchy, we find a "view" into a given database context can be represented as an isomorphic schema.

Isomorphic schemas can be used in lieu of views to offer different APIs to an existing database, potentially supporting legacy interfaces while allowing the database schema to evolve further.

Because it is not possible to query across schemas in one query (only one schema is under consideration at-a-time), the Principle of Orthogonal Design is not violated.

## Conclusion

The existence of isomorphic schemas underscores the need for DBMSes to embrace data independence which can allow for relation variables to be sliced to suit the needs of a application without violating the relational algebra's basic principles.

Because the schemas are isomorphic, any derived, isomorphic schema could be used to assist in an on-disk, optimal representation instead of the schema which a user happened to type in first.

Project:M36 is the first DBMS to propose and implement isomorphic schemas.

Further research into isomorphic schemas could include:

* methods to determine which persistent data representation represented as a schema would be optimal for a given set of input queries against the data.
* restricted access sub-schemas which emulate the functionality of SQL views used for security
* additional isomorphic building blocks such as
 * type constructor isomorphisms
 * atom function isomorphisms
 * project/extend isomorphisms
 * join isomorphisms

Many of the additional isomorphisms are mentioned in Date's "View Updating & Relational Theory" book but not named as such.

Project:M36 will continue to be developed in order to implement additional isomorphisms. We offer no proof that *every* isomorphic schema can be represented by these building blocks, but, hopefully, the blocks can be used to build the most interesting/useful schemas.

### Bibliography

Date, C.J. "View Updating & Relational Theory: Solving the View Update Problem". O'Reilly Media, Inc. 2013.

Date, C.J. "Database Design & Relational Theory: Normal Forms and All That Jazz". O'Reilly Media, Inc. 2012.

### Sponsors and Contributors

The funding covering the research and implementation of isomorphic schemas was generously provided by E.E.P.R grant #28557448. This work would not have been possible without discussions with Dr. G. Garbés, Ph.D.
