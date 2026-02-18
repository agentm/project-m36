# The Unified Backend: A New Architecture for Application Development

## Introduction to the Forced Choice Problem

Have you, dear programmer, ever had to decide whether some business logic should be implemented in the application layer or the database layer? If so, this article is for you!

That unease you felt when implementing something in Python and wondering if it should be done in SQL or vice versa is understandable- the choice is rarely driven by actual architectural design. Instead, the decision can come down to:

* pre-optimization: the developer assumes that the code in the database or application layer will be faster
* function access: some logic is implemented in the application layer but not in the database layer, so the developer is forced to add the code to the application layer
* a coin flip: the developer cannot make a determination but must choose one- or should he implement the same logic in both layers?
* team dynamics: the developer is more familiar with programming in one layer over another
* misunderstanding: the developer doesn't know that said business logic could be implemented in either layer
* access: the logic requires access to a resource only available from a specific layer; for example, access to a web service or database data
* limitations: the logic cannot be implemented in the database layer due to how the database was implemented
* data locality: the logic must be implemented in the database in order to exercise optimizations specific to the query (but see also *pre-optimization*)
* ORMs: common ORMs promise to be able to bridge the gap between application and database, but they have sharp edges or limitations. These ORMs don't help to make the layer decision, but rather obscure it.

You are not delusional: this forced choice is not only *arbitrary*, but *unnecessary*- the app layer/database layer split is not an inherent component of software architecture but, instead, a historical quirk.

### Problems with Placing Business Logic

Let's consider a sample application which sells zoo tickets. Here's a Haskell data structure to represent a ticket:

```haskell
data Ticket = Ticket
  { ticketId   :: Integer
  , visitorAge :: Integer     -- years
  , ticketPrice  :: Integer 
  , visitDate  :: Day
}
```

and a corresponding PostgreSQL table:

```sql
CREATE TABLE ticket_sales(
 id SERIAL PRIMARY KEY, 
 visitor_age INTEGER NOT NULL, 
 ticket_price INTEGER NOT NULL, 
 visit_date DATE NOT NULL);
```

Now that we have the setup out of the way, let's add some business logic operating on the ticket sales. Here'a a Haskell function to implement a discount:

```haskell
-- | Apply a 50% discount for kids under 10 years old. Arguments: age, base price
applyDiscount :: Integer -> Integer -> Integer
applyDiscount age base_price =
  if age < 10 then base_price `div` 2 else base_price
```

Note how this function can operate on any age and price information. The code is *not* dependent on any ORM on the application layer or any database-specific features on the database side. It is merely a standard Haskell function which could be executed anywhere.

So how do we figure out where to place this function in the codebase? Consider if we now run this function in the application layer, before we issue the "ticket_sales" INSERT expression to the database, then:

* information on which tickets received the children's discount is not recorded in the database.
* if this discount code changes- for example, to make it seasonal- then the older ticket data in the database is not modified. This is correct since the ticket was already sold, so the existing sales data in the database should *not* be affected.
* if we wish to calculate all past discounts- for example, to make a chart of discounts over time- the database cannot provide it because the discount amounts were not recorded (should they be?). 

The client cannot provide the discount used at a previous point in time unless the client retains all previous discount functions. This implies that we would need to somehow version all the discount functions and know when to apply each function based on the customer's age and also when the ticket was purchased.

If we wish to achieve this in the application layer, then we end up with something messy like:

```haskell
applyDiscount_v1 :: Integer -> Integer -> Integer
applyDiscount_v1 age price = ...

applyDiscount_v2 :: Integer -> Integer -> Integer
applyDiscount_v2 age price = ...

applyDiscount :: Day -> Integer -> Integer -> Integer
applyDiscount day age price = if day <= fromGregorian 2025 10 30 then applyDiscount_v1 age price else applyDiscount_v2 age price
```

If the discount code function has a bug, it cannot be ever fixed, because that could change historically-calculated discounts which have been attached to sold tickets!

Ok, no problem, you think- you can add a "discount_price" column to the "ticket_sales" table to record the discount. That's a possibility, but then a manager asks you, the programmer, "Hey, how much money would we have made if we had applied last year's discount to this year's ticket sales?" Suddenly, you realize you have a programming task which neither the application layer nor the database can answer without some bespoke implementation.

Luckily you thought about these complications in advance and you decided to put the discount function into the database as an SQL function.

```SQL
CREATE FUNCTION apply_discount(age INTEGER, price INTEGER) RETURNS INTEGER IMMUTABLE AS $$
 SELECT CASE WHEN age <= 10 THEN price / 2 ELSE price END; 
$$ LANGUAGE SQL;

INSERT INTO ticket_sales(visitor_age, price, visit_day) 
VALUES (20, apply_discount(20, 25), now());
```

But that doesn't really solve the multiple discount function problem- perhaps you can record which version or function name of the discount function was applied for each row, but that requires a lot of error-prone, but essential, bookkeeping. The advantage of having the implementation in the database is that we can write queries to answer critical business questions such as "how much discount was applied?" The problem becomes that we now have to integrate the function into the database- it's not the same function as our Haskell function (or any other application layer language), so can we be certain it behaves identically?

With this simple zoo ticket example, we hit immediate limitations on what current application and database products provide. Let's examine an architecture without these limitations.

## The Solution

[Out of the Tarpit](https://curtclifton.net/papers/MoseleyMarks06a.pdf), a paper published in 2006, lays out the solution: the database and application layer should be (and should have always been) the same software. [Project:M36](https://github.com/agentm/project-m36) is an implementation of the paper's recommendations which includes:

* a unified relational algebra implementation with business logic support
* support for queries against past transaction states
* a rejection of SQL's legacy baggage and poor security features
* a novel security model which prevents SQL's security-related anomalies
* retention of past database transactions to enable time-travel queries

Let's take a closer look.

### Unifying the Application and Database Layers

With the relational model component unified with other application components, the developer benefits from a single programming language and environment. All data types are unified across business logic and queries. Here's an example using Project:M36 which implements an API to manage zoo ticket sales.

First, we start the Project:M36 database server because we'll be connecting to the database with multiple users. We'll be disabling TLS connection encryption for this example just to keep authentication simple, but rest assured that Project:M36 does support encrypted authentication. 

The following command does not return because it is running the database. You can kill it, as usual, with Control-C.

```
project-m36-server --disable-tls --database zoo
```

Then, as a database administrator in another console, we setup the necessary schema and role through the `tutd` database console:

```
tutd --disable-tls --database zoo #assumes admin role
TutorialD (master/main): :addloginrole ticket_seller maylogin
TutorialD (master/main): grant ticket_seller executefunctions nogrant
TutorialD (master/main): grant ticket_seller committransaction nogrant
TutorialD (master/main): ticket_sales := relation{ticketId Integer, visitorAge Integer, price Integer, visitDate Day}
```

We create a `ticket_seller` login role and, in the second and third expressions, grant that role the permission to run functions and commit transactions while, with `nogrant` prevent the role from granting the role to others. We allow `ticket_seller` access to execute functions because these functions will become our user-facing API.

The fourth expression defines a `ticket_sales` relation variable (similar to a table in SQL) with `ticketId`, `visitorAge`, `price`, and `visitDate` attributes. `ticket_seller` will *not* be granted access to this relation variable since `ticket_seller`'s job is to sell tickets in this example.

This completes the role, relation variable, and permissions setup.

Next, we create a Haskell module to define the API and permissions and save it to `zoo.hs` locally.

```haskell
module Zoo where
import ProjectM36.Module
import ProjectM36.AccessControlList
import Data.Time.Calendar
import ProjectM36.Base
import qualified Data.Map as M

-- type alias Age and Price as Integers
type Age = Integer
type Price = Integer

applyDiscount :: Age -> Price -> Price
applyDiscount age price =
  if age <= 10 then
    price `div` 2
    else
    price

addSale :: Integer -> Age -> Price -> Day -> DatabaseContextFunctionMonad ()
addSale ticketId age price purchaseDay = do
  let tuples = [TupleExpr (M.fromList [("ticketId", i ticketId),
                                       ("visitorAge", i age),
                                       ("price", FunctionAtomExpr "applyDiscount" [i age, i price] ()),
                                       ("visitDate", NakedAtomExpr (DayAtom purchaseDay))])]
      i = NakedAtomExpr . IntegerAtom
  executeDatabaseContextExpr (Insert "ticket_sales" (MakeRelationFromExprs Nothing (TupleExprs () tuples)))


projectM36Functions :: EntryPoints ()
projectM36Functions = do
  declareAtomFunction "applyDiscount"
  declareDatabaseContextFunction "addSale" (permissionForRole ExecuteDBCFunctionPermission "ticket_seller" <> allPermissionsForRole "admin")
```

We implement a standard Haskell function `applyDiscount` and then, in the `projectM36Functions`, we instruct Project:M36 to use it as an "atom function"- a function which works on database values. 

Next, we implement an `addSale` function which is a `DatabaseContextFunction`. Obviously, this function includes some database-specific function calls to manipulate the underlying database transaction, specifically to insert a row into the `ticket_sales` relation variable.

Finally, we load the Haskell module into our database:

```
TutorialD (master/main): loadmodulefromfile "zoo.hs"
TutorialD (master/main): :commit
```

The `loadmodulefromfile` command copies the `zoo.hs` Haskell module to the database, parses it, compiles it to Haskell bytecode, and installs the functions we declared in the `projectM36Functions` function. Once the transaction is committed, the module and its functions are permanent and immutable.

We can use these functions immediately:

```
TutorialD (master/main): execute addSale(123, 5, 20, fromGregorian(2024,10,5))
TutorialD (master/main): :showexpr ticket_sales
┌──────────────┬─────────────────┬──────────────┬───────────────────┐
│price::Integer│ticketId::Integer│visitDate::Day│visitorAge::Integer│
├──────────────┼─────────────────┼──────────────┼───────────────────┤
│10            │123              │2024-10-05    │5                  │
└──────────────┴─────────────────┴──────────────┴───────────────────┘
TutorialD (master/main): :commit
```

However, our goal is to use `addSale` as a security-conscious API. So let's exercise that. We'll restart our `tutd` client to connect as the `ticket_seller` role.

```
tutd --database zoo --disable-tls --login-role ticket_seller
TutorialD (master/main): :showexpr ticket_sales
ERR: AccessDeniedError (SomeRelVarPermission AccessRelVarsPermission)
```
Note that the `ticket_seller` cannot access the `ticket_sales` relation variable.

Can `ticket_seller` insert some malicious ticket data?

```
TutorialD (master/main): insert ticket_sales relation{tuple{ticketId "456", visitorAge "-1", price "-20", visitDate fromGregorian(2000,10,10)}}
ERR: AccessDeniedError (SomeRelVarPermission AccessRelVarsPermission)
```

No. The ticket seller has neither read nor write access to the relation variable behind the `addSale` function. This is intentional so that a seller cannot modify data after a sale is made. But, we can run the `addSale` function since the `admin` role granted permission to `ticket_seller` to execute the function and commit the change.

```
TutorialD (master/main): execute addSale(124, 15, 20, fromGregorian(2024,10,5))
TutorialD (master/main): :commit
```

Thus, `addSale` defines the singular API function available to `ticket_seller`, ensuring the security of the data. Of course, `addSale` could also perform some data validation, but, for the purposes of brevity, validation is elided here.

Thus, we have defined a complete, albeit intentionally simple, API, including role-based access control and a strongly-typed API definition, all centralized within the database/application server. In production, we would add TLS encryption and certificate authentication, and then we could connect any user interface to the database and feel confident about application security.

### Looking at Past State

Don't forget that functions are immutable in Project:M36. This is different from run-of-the-mill SQL databases which overwrite their function definitions and only allow access to the "current" state/transaction. 

Let's update our discount function to grant the 50% discount to children 10 years or younger *except* on New Year's Day. We'll update our Zoo module to change the applyDiscount function.

```haskell
module Zoo where
import ProjectM36.Module
import ProjectM36.AccessControlList
import Data.Time.Calendar
import ProjectM36.Base
import qualified Data.Map as M

type Age = Integer
type Price = Integer

applyDiscount :: Age -> Price -> Day -> Price
applyDiscount age price day =
  if age <= 10 && not isNewYearsDay then
    price `div` 2
    else
    price
 where
  isNewYearsDay =
    case toGregorian day of
      (_, m, d) -> m == 1 && d == 1

addSale :: Integer -> Age -> Price -> Day -> DatabaseContextFunctionMonad ()
addSale ticketId age price purchaseDay = do
  let tuples = [TupleExpr (M.fromList [("ticketId", i ticketId),
                                       ("visitorAge", i age),
                                       ("price", FunctionAtomExpr "applyDiscount" [i age, i price] ()),
                                       ("visitDate", NakedAtomExpr (DayAtom purchaseDay))])]
      i = NakedAtomExpr . IntegerAtom
  executeDatabaseContextExpr (Insert "ticket_sales" (MakeRelationFromExprs Nothing (TupleExprs () tuples)))


projectM36Functions :: EntryPoints ()
projectM36Functions = do
  declareAtomFunction "applyDiscount"
  declareDatabaseContextFunction "addSale" (permissionForRole ExecuteDBCFunctionPermission "ticket_seller" <> allPermissionsForRole "admin")
```

We need to reload the module to load the new functions:

```
TutorialD (master/main): loadmodulefromfile "examples/zoo.hs"
TutorialD (master/main): :commit
```

Despite having loaded two functions with the same names, the previous functions are still available. We can access them using trans-graph relational expressions which tag our commands with past-state markers similar to traversing past patches in git source control.

```
TutorialD (master/main): :showtransgraphexpr relation{tuple{day "New Year's Day", price applyDiscount(8,20,fromGregorian(2025,1,1)@master)@master}}@master union relation{tuple{day "normal day", price applyDiscount(8,20)@master^}}@master
┌────────────────┬──────────────┐
│day::Text       │price::Integer│
├────────────────┼──────────────┤
│"New Year's Day"│20            │
│"normal day"    │10            │
└────────────────┴──────────────┘
```

Note that `applyDiscount(8,20,fromGregorian(2025,1,1)@master)@master` is applied to the version of `applyDiscount` we most recently committed to the `master` branch while the original version is executed using `applyDiscount(8,20)@master^` (note the caret) which elided the `Day` argument.

With this time-travel-like feature, we can answer business hypotheticals such as: 

* What if we applied last year's discount function to this year's ticket sales?
* Why did we calculate such a discount on a specific date?

This is only possible because we are able to recreate past states for query use.

### A Secure API By Design

By defining functions in the database and exposing them via mandatory role-based access control, we eliminate the need for an "application" layer to bolt on security checks. Application layer security checks are often poorly-implemented, difficult to verify, and not centrally managed. With Project:M36, all permissions are managed right alongside the functions defining the API.

### Can't We Do This With Procedural Languages Already?

PostgreSQL implements some of the features proposed in this unified architecture, including:

* server-side functions in a selection of languages
* fine-grained role-based access control over functions
* authenticated, remote access

However, the integration with server-side functions is not smooth. Consider, for example, the zoo example above: if we build a basic Python application layer over the PostgreSQL database to serve a web application and want to call `add_sale` as a pl/python function, then we:

1. install the function using `CREATE FUNCTION add_sale(age INTEGER, price INTEGER) RETURNS INTEGER AS $$ return price//2 if age <= 10 else price $$ LEAKPROOF IMMUTABLE STRICT LANGUAGE plpython3u;`
1. issue a SQL query via python `db.execute("SELECT add_sale(%s,%s)",(8,20))` to prevent SQL injections

Installing the function definitely does not look like python, so it's largely inaccessible to python developers. What do "leakproof", "immutable", and "strict" mean and why are they necessary here? (The answers are left as an exercise to the reader.) Project:M36 loads Haskell code directly with minimal database-specific knowledge required.

Next, consider how this function is executed with psycopg.

1. python must convert your python integers (int) to strings to construct the SQL `SELECT` via bound parameters
2. postgresql parses the query and converts the postgresql INTEGERs (which are not the same as python ints) back to python integers to pass as arguments to the python `add_sale` function
3. postgresql loads its own python interpreter and converts the SQL argument INTEGERS to python ints
4. the postgresql python interpreter (which is probably not the same python version as the application layer is running) runs the `add_sale` function and returns a python integer which has to be converted back to an SQL INTEGER
5. the result set is returned via the postgresql binary protocol
6. the result set is converted from the binary protocol to a postgresql INTEGER
7. the postgresql INTEGER is converted to a python int

So, amongst all these conversions, are you sure that all the conversions are sound? Are None (python)/NULL (SQL) values handled properly at every step? What are the maximum and minimum bounds of python ints compared to SQL INTEGER? Are you sure? What about if the types get more complicated, such as with SQL NUMERIC?

In contrast, Project:M36 implements directly equivalent type semantics. A Haskell Integer is the same thing in the database with identical semantics and functions which can operate on the types. In addition, Project:M36 supports algebraic data types which are quite common in Haskell.

```
TutorialD (master/main): data TicketCategory = Adult | Child | Free Text
TutorialD (master/main): :showexpr relation{tuple{price 20, category Free "promotion"}}
┌────────────────────────┬──────────────┐
│category::TicketCategory│price::Integer│
├────────────────────────┼──────────────┤
│Free "promotion"        │20            │
└────────────────────────┴──────────────┘
```

Trying to recreate algebraic data types in SQL is painful, if at all possible.

PostgreSQL is not architecturely equipped to be an application server because of its historical implementation baggage. By being reliant on a fork-on-connection architecture, PostgreSQL cannot service more than a few thousand connections at-at-time. That's where various PostgreSQL proxies with their own quirks try to fill-the-gap.

Finally, SQL offers zero facilities for running the states of past functions. SQL functions are simply replaced and past states are garbage collected. SQL functions operate on the "current" state of the database regardless of when it was added to the database. Audit tracking has to be bolted on. But, as we saw with the simple zoo example, comparing current state to past states is a common request which any database should be able to service.

### Goodbye SQL

With the unified programming environment, we no longer have to deal with SQL's historical baggage and quirks such as:

* language boundary mismatch: SQL doesn't mix well with any application layer language, including data types which don't mesh, differing capitalization and naming schemes, and lack of debuggability- Project:M36 data types are Haskell data types.
* SQL injection: a programmer footgun and persistent threat to any application composing SQL strings- Project:M36 uses a Haskell algebraic data type as RPC, not strings.
* SQL limitations: Project:M36 is a mathematically-consistent implementation of the relational algebra, eschewing all the historical baggage of SQL such as poor-man's custom data types.
* lack of transformation capability: given a string of SQL, how can one reliably replace a table in the "FROM" clause? (Answer: without an SQL parser, it's impossible to accomplish safely.) Project:M36 uses algebraic data types to represent the relational algebra operations which the database client can reliably transform using standard Haskell.
* NULL: which other programming language uses [ternary logic](https://github.com/agentm/project-m36/blob/master/docs/on_null.markdown)? Project:M36 uses Haskell data types to reliably model real-world data.
* security anomalies in row-level security: rows appearing or disappearing based on your role cause join anomalies and business confusion. Project:M36 forces developers to define an API for user-level access without access to the underlying relation variables (tables).

## Conclusion

Project:M36 is an implementation of a Haskell-oriented application server including role-based access control, database-side server functions to define user-facing APIs, and querying of past states. By re-evaluating what an application server can be, we've integrated native Haskell code with permissions and a relational algebra engine for retaining and querying state even as the application evolves, thereby solving the forced choice problem!

Along the way, we've tossed SQL and its quirks in order to provide a consistent and surprise-free programmable interface.

Project:M36 includes many features not mentioned in this post. Learn more or join [the project](https://github.com/agentm/project-m36)!
