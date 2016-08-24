# Project:M36 Extendable Atom Functions

## Introduction

While Project:M36 includes a set of basic function which operate on relational values (atoms in Project:M36 parlance), users may find it useful to write functions which operate on these atoms in the database context. For example, if a value represents a temperature in Celsius units, an atom function could be implemented to convert the value to Fahrenheit units.

Other DBMSes also include this feature by way of stored functions such as with PL/pgsql, T-SQL, or PL/SQL.

Project:M36 allows users to implement strongly-typed and pure atom functions in the Haskell language at runtime. This facility makes use of GHC, the Haskell compiler, as a runtime service.

## Atom Function Example

In this example, we install an atom function to convert between Fahrenheit and Celsius temperatures where the temperatures are represented as integers of type ```Int```.

We write the function giving it the name "celsiusToFahrenheit" with type ```Int -> Int``` which indicates to the compiler that the function take one ```Int``` argument value and returns one ```Int``` value. Finally, the body of the function is implemented in quotes or triple-quotes. Note that the function makes use of the "[ProjectM36.Base](/src/lib/ProjectM36/Base.hs)" module to handle the Atom type. The function must take a list of Atoms as arguments and return an Atom. The standard GHC Prelude is available in this AtomFunctionBody context.

```
TutorialD (master): addatomfunction "celsiusToFahrenheit" Int -> Int """\((IntAtom c):_) -> IntAtom ( (c * 9 `div` 5) + 32)"""
```

Using triple-quotes allows quotes to be used inside the string without backslash escaping, which can become tedious.

## Advanced Atom Function Example

Project:M36 also supports runtime-created new data types represented as algebraic data types. Internally, these are represented as ```ConstructedAtom```s, so the atom function script must use appropriate constructors.

In the following example, splitting the temperature values into two types make sense because it prevents accidental use of operators between the two domains. For example, a ```CelsiusTemperature``` could never be accidentially added to a ```FahrenheitTemperature``` or otherwise compared without a conversion function in between. Thus, the separation of types increases our confidence in the database results when compared to the raw ```Int``` representation above.

```
data CelsiusTemperature = CelsiusTemperature Int
data FahrenheitTemperature = FahrenheitTemperature Int

addatomfunction "celsiusToFahrenheit" CelsiusTemperature -> FahrenheitTemperature """\(ConstructedAtom _ _ (IntAtom celsius:_):_) -> ConstructedAtom "Fahrenheit" (ConstructedAtomType "Fahrenheit" empty) [IntAtom (celsius * (9 `div` 5) + 32)] """
```

First, we define our new types and use these types in the function's type definition. Next, we deconstruct the incoming ```ConstructedAtom``` argument to extract its ```Int``` value. Finally, we perform the calculation and wrap the value in a new Fahrenheit-based ```ConstructedAtom```.

## Differences when compared to other DBMSes

Project:M36 enforces strongly-typed atom functions. Specifically, atom functions must be pure (free of any side effects) and cannot operate on any variables other than the atoms passed in. This certainly sets stricter limits on what can be accomplished in an atom function when compared to stored functions in other DBMS products. For example, an atom function in Project:M36 would never be able to retrieve stock information from an external website because such a function cannot be pure and thus would be rejected by the atom function compiler. This restriction maintains the mathematical cohesiveness of the database and allows the DBMS to make performance-enhancing inferences it would not otherwise be able to make.
