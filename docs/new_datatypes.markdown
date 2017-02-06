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

Given a ```ProjectM36.Client``` ```Connection```, create an ```AddTypeConstructor``` with arguments a) type constructor name and b) data constructors.

Example:
```haskell
AddTypeConstructor (ADTypeConstructorDef "Hair" []) [DataConstructorDef "Brown" [], DataConstructorDef "Blond" [], DataConstructorDef "Bald" [],...]
```

Pass the ```AddTypeConstructor``` value to ```executeDatabaseContextExpr``` to add it to the database.

To create a value for the new algebraic data type, create the type:

```haskell
let hairType = ConstructedAtomType "Hair" M.empty
```

then create the value:

```haskell
let blondAtom = ConstructedAtom "Blond" hairType []
```

The ```blondAtom``` can now be used in any place where an Atom is otherwise used such as in ```AtomExpr``` checking for equality in restriction or as an argument to ```AtomFunction```s or simply in building a new relation.

In the previous example, the additional empty arguments are for polymorphic variables and arguments. In this simple ADT, there is no polymorphism necessary, so the arguments are empty.
