# Handling Data Description Language (DDL) Changes

One challenge SQL developers often face is knowing whether or not a query a client application may issue is valid. For example:

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
