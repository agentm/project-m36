# Creating New Data Types

## Introduction

The most common data types are built into Project:M36 directly. Such types include integers, text, byte strings, booleans and floats. It can often be handy to create one's own data types in a database and manipulate them as first-class values through the relational algebra.

In addition, the Project:M36 runtime supports algebraic data types in the same vein as Haskell itself.

Project:M36 makes it easy to add any Haskell data type to the database. Any type which includes instances for a set of typeclasses can be supported.

## Support for Runtime Types

Use the ```tutd``` interpreter to create new type and data constructors with essentially the same syntax as in Haskell:

```
TutorialD (master): data Hair = Bald | Brown | Blond | OtherColor Text
TutorialD (master): :showexpr relation{tuple{name "Steve", hair Blond}, tuple{name "Sam", hair OtherColor "Grey"}}
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
TutorialD (master): undata Hair
```

These runtime types can therefore change between transactions. Note that the underlying data values do not change when the types change. Once changed, only new values will reflect the new data constructors.

The downside to this approach is that functions which operate on these runtime types cannot be written in the TutorialD interpreter (yet).

Note that the Haskell-standard "Maybe a" and "Either a b" data types are included by default.

## Support for Haskell Types

Any Haskell data type which is an instance of ```Atomable``` is a candidate for a Project:M36 value called an ```Atom```. Database tuples are composed of Atoms.

Project:M36 does not yet support dynamically-loaded components, so the data type must be compiled into the Project:M36 backend.

### Implementation for Haskell Types

1. Create an instance of your data type for the ```Atomable``` typeclass. The ```Atomable``` typeclass implies instances for a variety of standard typeclasses.
1. Update the ```Binary Atom``` instance to include deserialization for your type.
1. Update the ```makeAtomFromText``` in ```ProjectM36/Atom.hs``` to support CSV import.
1. Optionally adjust ```TutorialD/Interpreter/RelationalExpression.hs``` to add support for parsing your data type in the TutorialD interpreter. Look for ```atomP```.
