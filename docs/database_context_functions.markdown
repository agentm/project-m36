# Project:M36 Database Context Functions

## Introduction

While "atom functions" operate on individual values and return new values (atoms), database context functions operate on pre-committed database contexts in the same context as the ```update```, ```insert```, and ```delete``` operators. The difference is that database context functions are written in Haskell and support any number of context changes.

Other DBMS software may include a similar feature called "stored procedures". However, "stored procedures" are stored outside the transaction stream and are not versioned by the database as in Project:M36.

In order to use this feature in Project:M36, be sure to build your own Project:M36 library so that the optional scripting is enabled.

## Database Context Function Types

The Haskell type of all database context functions is:

```haskell
[Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext
```

That is, the function takes an array of ```Atom```s which represent the function's arguments and a context and returns an updated context or an error. Notice that this operation is idempotent ("pure" in Haskell parlance).

The database context references all data which can be committed; this includes relation variables, atom functions, notifications, database context functions, and other versionable data. Database contexts become immutable once committed.

## Scope of Database Context Functions

Database context functions are committed alongside the transaction. Thus, previous versions of the functions can be explored or restored. Like all database context elements, database context functions are immutable once committed. However, the transaction graph can be branched in order to examine how database context functions changes might apply to future contexts.

Remember that database context functions don't need to worry about locks or other synchronization. The database context is completely isolated from other ongoing transactions.

Returning a ```DatabaseContextFunctionError``` from the function ensures that any changes made by the function are discarded.

## TutorialD Demonstration

Use ```adddatabasecontextfunction``` to compile and name a new database context function:

```
adddatabasecontextfunction "addperson" Int -> Text -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext """(\(age:name:_) ctx -> let newrel = MakeRelationFromExprs Nothing [TupleExpr (fromList [("name", NakedAtomExpr name),("age", NakedAtomExpr age)])] in if isRight (evalState (evalRelationalExpr (RelationVariable "person" ())) (mkRelationalExprState ctx)) then pure (execState (evalDatabaseContextExpr (Insert "person" newrel)) ctx) else pure (execState (evalDatabaseContextExpr (Assign "person" newrel)) ctx)) :: [Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext"""   
```

This function is quite dense, so let's examine its components.

The first components are:

```
adddatabasecontextfunction "addperson" Int -> Text -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext
```

which defines a function "addperson" which takes three arguments (an Int value, a Text value, and a DatabaseContext) and returns a DatabaseContext. All database context functions take as a final argument a database context and must return a database context.

The meat of the function is obviously the Haskell, so let's lay it out:

```haskell
(\(age:name:_) ctx -> let newrel = MakeRelationFromExprs Nothing [TupleExpr (fromList [("name", NakedAtomExpr name),("age", NakedAtomExpr age)])] in
  if isRight (evalState (evalRelationalExpr (RelationVariable "person" ())) (mkRelationalExprState ctx)) then
    pure (execState (evalDatabaseContextExpr (Insert "person" newrel)) ctx)
  else
    pure (execState (evalDatabaseContextExpr (Assign "person" newrel)) ctx)) :: [Atom] -> DatabaseContext -> Either DatabaseContextFunctionError DatabaseContext
```

Line 1 sets up a new relation created from the function's arguments. In this case, we create a relation to represent a new person in our database.

Line 2 determines if the "person" relation variable is already available in the context. If so, the function inserts the new "person" relation into the existing relation variable in line 3.

In line 5, since the relation variable was determined not yet to be defined, we assign the new person relation directly.

Note that there is nothing special about the Haskell above- it is indeed a full-blown Haskell environment; however, this environment operates in "Safe" mode which prohibits any non-pure function access (such as with ```unsafePerformIO```). Thus, it is safe to use as a trusted programming environment with users creating arbitrary convenience functions within the implicit sandbox.

Users can add arbitrary (but safe and pure) calculations to these functions to capture business logic. The list of modules available within the function context can be modified at compile-time.

Let's try the new function!

```
TutorialD (master/main): execute addperson(30, "Steve")
TutorialD (master/main): :showexpr person
┌────────┬──────────┐
│age::Int│name::Text│
├────────┼──────────┤
│30      │"Steve"   │
└────────┴──────────┘
TutorialD (master/main): execute addperson(32, "Bob")
TutorialD (master/main): :showexpr person
┌────────┬──────────┐
│age::Int│name::Text│
├────────┼──────────┤
│32      │"Bob"     │
│30      │"Steve"   │
└────────┴──────────┘
TutorialD (master/main):
```

Here we see that the first call to ```addperson``` created the relation variable while a subsequent call added another tuple to the existing relation variable.

## Haskell access

Database context functions come in two varieties:

* scripted (as above)
* pre-compiled

If you are dealing with the Haskell interface directly and you don't anticipate changing the functions, the easiest option is to compile your Haskell database context function into the database. It can always be removed or supplanted by a scripted version later.

To add a new, compiled database context function to the database, use:

```haskell
executeDatabaseContextIOExpr sessionId dbconn (AddDatabaseContextFunction funcName Nothing (\(arg1:arg2:_) ctx -> ...))
```
from the ```ProjectM36.Client``` module.

The function can be executed using:

```haskell
executeDatabaseContextExpr sessionId dbconn (ExecuteDatabaseContextFunction funcName atomArgList)
```
from the same module.

## Future Improvements

There are likely a variety of convenience functions which would make sense in the context of the scripted function. If you have ideas for such functions, please open a GitHub issue.
