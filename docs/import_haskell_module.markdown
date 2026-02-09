# Importing a Haskell Module

## Introduction

Typical DBMS products have their own programming language built-in; for example PL/SQL, PL/pgSQL, PL/python, or even JavaScript. But these development environments have to be declared through SQL using `CREATE FUNCTION`, use database-specific means to extract and return arguments, and have non-obvious semantics with regards to SQL optimization. We can do better.

Imagine you are a programmer with some business logic. Here's an example:

```
apply_discount :: Integer -> Integer -> Integer
apply_discount age price = if age <= 10 then
    price `div` 2
    else
    price
```

Presumably, even if you have not used Haskell before, you can understand that this function does not rely on any database-specific features. It's just a regular Haskell function. Now, you must decide where to place this function- is it more appropriate to have it run in the client code, the application layer, or the database layer?

If you install the function on the client side, the client can grant himself an arbitrary discount. Goodbye, security.

If you install the function in the application layer, then no database query can run the function. Goodbye to reporting on discounts.

If you install the function in the database layer, it may be more difficult to debug and adds the complexity of database schema migrations. Goodbye, developer experience.

The reason there is no obvious place to put this basic business logic is because the bifurcation of application and database layers is arbitrary. Project:M36 purports to unify these layers by allowing standard Haskell code to be loaded into a DBMS which provides a clearly delineated API, unifying API definition, access control, and code versioning. The Haskell module loader described here provides the infrastructure to achieve this unification.

## Writing Your Haskell Module

Project:M36 aims to make the writing Haskell code to be run in the database as natural as running it locally. To that end, a Project:M36-compatible Haskell module is just a normal Haskell module with some metadata attached.

```haskell
module TestFuncs (projectM36Functions, apply_discount) where
import ProjectM36.Module

apply_discount :: Integer -> Integer -> Integer
apply_discount age price =
  if age <= 10 then
    price `div` 2
    else
    price

projectM36Functions :: EntryPoints ()
projectM36Functions = do
  declareAtomFunction "apply_discount"
```

Note how the Project:M36-specific parts of the code are minor:

* we import `ProjectM36.Module`
* we declare which functions we wish to expose to Project:M36 using `declareAtomFunction` within the `projectM36Functions` function


## Loading Your Haskell Module

The Haskell module can be used as-is within an existing Haskell codebase. The Project:M36-specific code could be compiled out using CPP (C preprocessor) macros, if needed.

To load the module within a Project:M36 database, use the `LoadModuleWithFunctions` value with the `executeDatabaseContextIOExpr` function or from the `tutd` console:

```
TutorialD (master/main): loadmodulefromfile "TestFuncs.hs"
```

After the module has been loaded, the function- an atom function in this example- can be used right away:

```
TutorialD (master/main): ticket_sales := relation{tuple{ticketId 123, base_price 20, actual_price apply_discount(8,20)}}
TutorialD (master/main): :showexpr ticket_sales
┌─────────────────────┬───────────────────┬─────────────────┐
│actual_price::Integer│base_price::Integer│ticketId::Integer│
├─────────────────────┼───────────────────┼─────────────────┤
│10                   │20                 │123              │
└─────────────────────┴───────────────────┴─────────────────┘

```

## Types of Functions

Project:M36 currently supports two types of functions in modules: atom functions and database context functions.

The *atom function* (seen in the example above) operates on values within a tuple to create a new value.

The *database context function* operates on any state within a single database transaction. Let's look at an example:

```haskell
addSale :: Integer -> Integer -> Integer -> Day -> DatabaseContextFunctionMonad ()
addSale ticketId age price purchaseDay = do
  let tuples = [TupleExpr (M.fromList [("ticketId", i ticketId),
                                       ("visitorAge", i age),
                                       ("basePrice", FunctionAtomExpr "applyDiscount" [i age, i price] ()),
                                       ("visitDate", NakedAtomExpr (DayAtom purchaseDay))])]
      i = NakedAtomExpr . IntegerAtom
  executeDatabaseContextExpr (Insert "ticket_sales" (MakeRelationFromExprs Nothing (TupleExprs () tuples)))
```

This function is noticeably more bound to the Project:M36 API, but that is to be expected when manipulating key database concepts such as "relation variable" and "tuple". This could be further improved in the future. Declaring this type of function is as simple as declaring the atom functions seen earlier:

```haskell
projectM36Functions :: EntryPoints ()
projectM36Functions = do
  declareAtomFunction "applyDiscount" -- copied from above
  declareDatabaseContextFunction "addSale" (allPermissionsForRoleId adminRoleId)
```

In addition to the function's name, we must also include which role-based access control permissions we wish to grant to which roles. We can always change the permissions later, so we grant blanket permissions to the administrator role.

## Benefits of Keeping Functions in the Database

Project:M36 deliberately supports defining business logic in the database and not as a second-rate citizen to SQL such as with PL/plpgsql or PL/python in PostgreSQL.

Here are some good reasons to keep your business logic within the database:

### Immutable Functions

Once the function is committed wihin a transaction to the database, it cannot be altered. A new function with the same name could replace the function in a subsequent transaction, but the original function is still accessible through the time-travel feature.

### Running Past Versions of Functions

Since historical functions are still accessible, they can be run, for example, for historical reporting or hypothetical reporting (whereby one could ask what the revenues might look like if a previous version of the discount function were used).

### Audit Trail

Since functions are immutable, they can be recalled for auditing along with past database states, including running past versions of functions.

### Intentional Security

Access control is mandatory for all database context functions. Such protected functions can be used to define a user-accessible, secure API. No additional security in the application is necessary to provide secure access to the database.

## Future Direction

Project:M36 aims to be at the forefront of the unified application and database architecture. More research needs to be done on how to effectively apply execution planner optmizations to the different types of functions such as to reduce unnecessary round-trips or to identify trends in modified data to feed to the next planner phases.