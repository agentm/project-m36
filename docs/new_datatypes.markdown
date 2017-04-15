# Creating New Data Types

## Introduction

The most common data types are built into Project:M36 directly. Such types include integers, text, byte strings, booleans and floats. It can often be handy to create one's own data types in a database and manipulate them as first-class values through the relational algebra.

In addition, the Project:M36 runtime supports algebraic data types in the same vein as Haskell itself.

Project:M36 makes it easy to add any Haskell data type to the database. Any type which includes instances for a set of typeclasses can be supported.

## Support for Runtime Types

Use the ```tutd``` interpreter to create new type and data constructors with essentially the same syntax as in Haskell:

```
TutorialD (master/main): data Hair = Bald | Brown | Blond | OtherColor Text
TutorialD (master/main): :showexpr relation{tuple{name "Steve", hair Blond}, tuple{name "Sam", hair OtherColor "Grey"}}
┌─────────────────┬──────────┐
│hair::Hair       │name::Text│
├─────────────────┼──────────┤
│Blond            │"Steve"   │
│OtherColor "Grey"│"Sam"     │
└─────────────────┴──────────┘
```

SQL's enumerations or even joins do not compare favorably to a genuine algebraic data type. Exercise for the reader: why is this? Do you agree?

Types can be forgotten as well:

```
TutorialD (master/main): undata Hair
```

These runtime types can therefore change between transactions. Note that the underlying data values do not change when the types change. Once changed, only new values will reflect the new data constructors.

The downside to this approach is that functions which operate on these runtime types cannot be written in the TutorialD interpreter (yet).

Note that the Haskell-standard "Maybe a" and "Either a b" data types are included by default.

## Support for Haskell Types

New algebraic data types can be added to the database context at any time via TutorialD or Haskell. Typeclasses for database types are not supported.

### Implementation for Haskell Types

Any Haskell ADT which implements the ```Atomable``` typeclass can be used directly as a database value or "atom" in Project:M36 parlance. The ```Atomable``` typeclass requires a set of prerequisite typeclasses be included, but, luckily, all of these typeclasses can be derived.

Let's implement the ```Hair``` algebraic data type from the TutorialD example, but in Haskell. The full source code is available under the [examples directory](/examples/hair.hs).

First, we define our data type and derive all the required typeclasses:

```haskell
data Hair = Bald | Brown | Blond | OtherColor Text
   deriving (Generic, Show, Eq, Binary, NFData, Atomable)
```

Note that the ```Atomable``` instance can also be derived.

After setting up the connection (which we elide here), the database context is loaded with the expression to define the data type.

```haskell
executeDatabaseContextExpr sessionId conn (toDatabaseContextExpr (undefined :: Hair))
```

Using a ```Proxy``` would also be acceptable, but the result would be the same.

Next, we can create a relation variable named "people" containing our new data type alongside a person's name.

```haskell
  let blond = NakedAtomExpr (toAtom Blond)
  executeDatabaseContextExpr sessionId conn (Assign "people" (MakeRelationFromExprs Nothing [
            TupleExpr (M.fromList [("hair", blond), ("name", NakedAtomExpr (TextAtom "Colin"))])]))
```

To demonstrate that the new value is fully integrated with the database, we can create a restriction which matches against the value in the database.

```haskell
let restrictionPredicate = AttributeEqualityPredicate "hair" blond
peopleRelOrErr <- executeRelationalExpr sessionId conn (Restrict restrictionPredicate (RelationVariable "people" ()))
```

Finally, we can print the resultant relation to show that it can be properly displayed (via the type's ```Show``` instance).

```
TIO.putStrLn (showRelation peopleRel)
```

```
┌──────────┬──────────┐
│hair::Hair│name::Text│
├──────────┼──────────┤
│Blond     │"Colin"   │
└──────────┴──────────┘
```

Note here that ```Blond``` is not quoted because it is not a text value, but rather a real algebraic data type constructor honored by the database. The value is not a black box nor is an enumeration. Try, for example, replacing the ```Blond``` value with an ```OtherColor "Black"``` value to see how it appears and how it can be restricted against as well.

The essential ```Atomable``` typeclass functions are:

| Function | Purpose |
| -------- | ------- |
| ```toAtom :: a -> Atom``` | convert a Haskell data type to a database atom |
| ```fromAtom :: Atom -> a``` | convert a database atom to a Haskell data type |
| ```toAtomType :: a -> AtomType``` | generate a database AtomType for a Haskell datatype |
| ```toDatabaseContextExpr :: a -> DatabaseContextExpr``` | generate a ```DatabaseContextExpr``` which can be executed against a database context in order to add the new type |

In addition, basic Haskell data types like ```Int```, ```Double```, ```Text```, ```Day```, ```Bool```, ```UTCTime```, and ```ByteString``` already have Atomable instances ready-to-go. In addition, an ```Atomable``` instance is available for lists: ```Atomable a => [a]```, though one should be very careful not to use this in a common anti-pattern. Typically, it makes more sense to store one-to-many relationships as discrete relation variables; when storing them as lists, one gives up the various query capabilities and optimizations of the relational algebra.

To be clear, these data types are not black boxes once stored in the database. These new types can be scanned and manipulated by Haskell scripts which can be created at runtime. For more information, read about [```AtomFunction```s](/docs/atomfunctions.markdown).
