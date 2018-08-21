# Project:M36 Extendable Atom Functions

## Introduction

While Project:M36 includes a set of basic function which operate on relational values (atoms in Project:M36 parlance), users may find it useful to write functions which operate on these atoms in the database context. For example, if a value represents a temperature in Celsius units, an atom function could be implemented to convert the value to Fahrenheit units.

Other DBMSes also include this feature by way of stored functions such as with PL/pgsql, T-SQL, or PL/SQL.

Project:M36 allows users to implement strongly-typed and pure atom functions in the Haskell language at runtime. This facility makes use of GHC, the Haskell compiler, as a runtime service. Alternatively, Haskell-based atom functions can be pre-compiled and loaded at runtime.

## Atom Function Example

In this example, we install an atom function to convert between Fahrenheit and Celsius temperatures where the temperatures are represented as integers of type ```Int```.

We write the function giving it the name "celsiusToFahrenheit" with type ```Int -> Int``` which indicates to the compiler that the function take one ```Int``` argument value and returns one ```Int``` value. Finally, the body of the function is implemented in quotes or triple-quotes. Note that the function makes use of the "[ProjectM36.Base](/src/lib/ProjectM36/Base.hs)" module to handle the Atom type. The function must take a list of Atoms as arguments and return an Atom. The standard GHC Prelude is available in this AtomFunctionBody context.

```
TutorialD (master/main): addatomfunction "celsiusToFahrenheit" Int -> Either AtomFunctionError Int """(\((IntAtom c):_) -> pure $ IntAtom ( (c * 9 `div` 5) + 32)) :: [Atom] -> Either AtomFunctionError Atom"""
```

Using triple-quotes allows quotes to be used inside the string without backslash escaping, which can become tedious. The sum type using ```Either``` allows the function to return an error, if necessary.

## Advanced Atom Function Example

Project:M36 also supports runtime-created new data types represented as algebraic data types. Internally, these are represented as ```ConstructedAtom```s, so the atom function script must use appropriate constructors.

In the following example, splitting the temperature values into two types make sense because it prevents accidental use of operators between the two domains. For example, a ```CelsiusTemperature``` could never be accidentially added to a ```FahrenheitTemperature``` or otherwise compared without a conversion function in between. Thus, the separation of types increases our confidence in the database results when compared to the raw ```Int``` representation above.

```
data CelsiusTemperature = CelsiusTemperature Int
data FahrenheitTemperature = FahrenheitTemperature Int

addatomfunction "celsiusToFahrenheit" CelsiusTemperature -> Either AtomFunctionError FahrenheitTemperature """(\(ConstructedAtom _ _ (IntAtom celsius:_):_) -> pure $ ConstructedAtom "Fahrenheit" (ConstructedAtomType "Fahrenheit" empty) [IntAtom (celsius * (9 `div` 5) + 32)]) :: [Atom] -> Either AtomFunctionError Atom """
```

First, we define our new types and use these types in the function's type definition. Next, we deconstruct the incoming ```ConstructedAtom``` argument to extract its ```Int``` value. Finally, we perform the calculation and wrap the value in a new Fahrenheit-based ```ConstructedAtom```.

Project:M36 also supports polymorphic types when used with atom functions. Here is an implementation of the veritable ```id``` function which merely returns its argument:

```
addatomfunction "idTest" a -> Either AtomFunctionError a "(\(x:_) -> pure x) :: [Atom] -> Either AtomFunctionError Atom"
```

## Pre-compiled Atom Functions

Compiling atom functions at runtime can incur a performance cost. To mitigate this, atom functions can be compiled by GHC to object files and then loaded into the Project:M36 server. Let's look at an [example](https://github.com/agentm/project-m36/):

```haskell
module DynamicAtomFunctions where
import ProjectM36.Base

someAtomFunctions :: [AtomFunction]
someAtomFunctions = [AtomFunction{
                    atomFuncName = "constTrue",
                    atomFuncType = [TypeVariableType "a", BoolAtomType],
                    atomFuncBody = AtomFunctionBody Nothing (\(x:_) -> pure (BoolAtom True))}]
```

Any function inside any module which returns a list of `AtomFunction`s can be used to load more atom functions. First, create the Haskell object file:

`cabal exec ghc -- examples/DynamicAtomFunctions.hs -package project-m36`

Use cabal or stack to invoke ghc so that the project-m36 installed package will be found.

Finally, connect to your Project:M36 database using the `tutd` client and run:

`TutorialD (master/main): loadatomfunctions "DynamicAtomFunctions" "someFunctions" "examples/DynamicAtomFunctions.o"`

If you see an error such as "unknown symbol", the version of the "project-m36" library installed in your sandbox is different from that with which you linked the object file. Make sure that the same versions are used in linking the server and object file.

The atom function is now loaded and ready-to-use. Note, however, that the function must be added every time the project-m36-server starts. This will probably be improved in a future release.

## Differences when compared to other DBMSes

Project:M36 enforces strongly-typed atom functions. Specifically, atom functions must be pure (free of any side effects) and cannot operate on any variables other than the atoms passed in. This certainly sets stricter limits on what can be accomplished in an atom function when compared to stored functions in other DBMS products. For example, an atom function in Project:M36 would never be able to retrieve stock information from an external website because such a function cannot be pure and thus would be rejected by the atom function compiler. This restriction maintains the mathematical cohesiveness of the database and allows the DBMS to make performance-enhancing inferences it would not otherwise be able to make.
