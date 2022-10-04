# Handling Data Description Language (DDL) Changes

One challenge SQL developers often face is knowing whether the queries embedded in their client application are still valid. For example:

* Do the tables mentioned in the query exist?
* Do those tables have the expected columns?
* Has the database schema been changed while the app has been running?
* Are the database schema changes compatible with the queries which the app will issue?
* More generally: is the client app compatible with the database schema?

Project:M36 solves this using:

## DDL Hashing

The DDL hash is a SHA256 hash of the current schema/DDL of the current transaction. This implies that the hash is not altered by DML (data manipulation language) changes. The data in the relation variables does not affect the hash.

The client developer can compare the hash of the DDL (schema) against an expected hash or set of hashes indicating app compatibility. If the hash is unknown, it indicates that the client app is not compatible with the schema and the user can be notified immediately at connection time instead of failing when a query is issued at some future time.

To retrieve the current DDL hash:

* in TutorialD:
```
TutorialD (master/main): :ddlhash
┌────────────────────────────────────────────┐
│ddlHash::ByteString                         │
├────────────────────────────────────────────┤
│8IOTpL8gNwsaQyFatCHpQVQGBx5kvlR9ddkol2/+nsw=│
└────────────────────────────────────────────┘
```
* in Haskell: `ProjectM36.Client.getDDLHash`

## Registered Queries

While inclusion dependencies are used to constraing the data in the database such as with foreign keys and uniqueness checks, registered queries are a means of constraining the DDL.

A registered query is a contract between the database and client: it is a promise by the database that the registered queries will be executable as long as they remain registered. If a schema change attempt is made which would render a registered query unable to run, such as due to a missing relation variable, then the schema change is rejected. Registered queries are type-checked but not run by the database whenever a schema change is made, so this check is effectively cost-free. 

Schema changes which do not affect the set of registered queries can proceed as normal. A database developer could use this feature to lock down certain parts of the schema which are used heavily and would be costly to change.

The user can still run any unregistered queries without issue or penalty. The purpose of registered queries is create an obligation by the database to enforce a specific, expected schema based on queries the client will normally issue.

Here, we add a registered query with the database:
```
TutorialD (master/main): employees := relation{tuple{name "Bob", age 32}}

TutorialD (master/main): registerquery "protect_employees" employees{name}
```
A common query may be to get the list of employee names.

If we try to delete `employees`, then the registered query will prevent it.

```
TutorialD (master/main): undefine employees
ERR: RegisteredQueryValidationError "protect_employees" (RelVarNotDefinedError "employees")
```

To remove this constraint on the schema, remove the registered query.

```
TutorialD (master/main): unregisterquery "protect_employees"
TutorialD (master/main): undefine employees
TutorialD (master/main): employees := relation{tuple{name "Bob", age 32, dept "Sales"}}
```

Whereas inclusion dependencies (foreign keys, uniqueness keys) operate to ensure that data remains coherent, registered queries ensure that the schema remains suitable for the intended queries.

