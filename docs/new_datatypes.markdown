# Creating New Data Types

## Introduction

The most common data types are built into Project:M36 directly. Such types include integers, text, byte strings, booleans and floats. It can often be handy to create one's own data types in a database and manipulate them as first-class values through the relational algebra. 

Project:M36 makes it easy to add any Haskell data type to the database. Any type which includes instances for a set of typeclasses can be supported.

## Support

Any Haskell data type which is an instance of ```Atomable``` is a candidate for a Project:M36 value called an ```Atom```. Database tuples are composed of Atoms.

Project:M36 does not yet support dynamically-loaded components, so the data type must be compiled into the Project:M36 backend.

### Implementation

1. Create an instance of your data type for the ```Atomable``` typeclass. The ```Atomable``` typeclass implies instances for a variety of standard typeclasses.
1. Update the ```Binary Atom``` instance to include deserialization for your type.
1. Update the ```makeAtomFromText``` in ```ProjectM36/Atom.hs``` to support CSV import.
1. Optionally adjust ```TutorialD/Interpreter/RelationalExpression.hs``` to add support for parsing your data type in the TutorialD interpreter. Look for ```atomP```.
