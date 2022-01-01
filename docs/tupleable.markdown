# Marshaling Haskell Data Types to Database Tuples

## Introduction

Typical ORMs map certain data types to tuples within a relation variable. Because Project:M36 supports Haskell data types natively, any record data type which derives the ```Generic``` typeclass can be marshaled to a database tuple using the ```Tupleable``` typeclass. These marshaled database values can be operated on within the database using Haskell as well.

## Setup

Let's imagine we have a Haskell record type "Person" which we wish to map to a tuple in a relation variable called "person". Using ```GHC.Generics```, creating this mapping is trivial. Any record fields which are ```Atomable```, including algebraic data types, are supported.

```haskell
import ProjectM36.Tupleable

data Person = Person {
  name :: Text,
  employeeNumber :: Int,
  likesHorses :: Bool
}
deriving Generic

instance Tupleable Person
```

Now, ```Person``` values can be marshaled between the database and the client. The attribute names appearing in the Haskell data type will appear identically in the server with the same types.

### Changing Options

If our field names don't exactly match attribute names, but follow a common pattern, we can derive Tupleable with custom options. Let's say each field on person has a prefix "person", but we still want to use the attribute names without the prefix. We can make use of the ProjectM36.Tupleable module to accomplish our goal like so:

```haskell
import ProjectM36.Tupleable.Deriving

data Person = Person {
  personName :: Text,
  personEmployeeNumber :: Int,
  personLikesHorses :: Bool
  }
  deriving stock Generic
  deriving Tupleable
    via Codec (Field (DropPrefix "person" >>> CamelCase)) Person
```

The extensions DerivingGeneric, DerivingVia, TypeOperators, and DataKinds must be enabled for this code to compile. See the documentation on Hackage for all available options.

## Usage

The ```Tupleable``` module includes the following functions:

 * ```toInsertExpr :: forall a t. (Tupleable a, Traversable t) => t a -> RelVarName -> Either RelationalError DatabaseContextExpr```
     * creates an ```Insert``` expression which can be used to insert a set of ```Tupleable``` values into the relation variable named by the ```RelVarName```
 * ```toUpdateExpr :: forall a. Tupleable a => RelVarName -> [AttributeName] -> a -> Either RelationalError DatabaseContextExpr```
     * create an `Update` expression which updates the tuples with the attribute values of the argument `Tupleable` using the list of attribute names as the key to the underlying relation variable
 * ```toDeleteExpr :: forall a. Tupleable a => RelVarName -> [AttributeName] -> a -> Either RelationalError DatabaseContextExpr```
    * create a `Delete` expression which will remove the tuples from the relation variable matching the values of the argument `Tupleable` using the attribute name list as the matching key
 * ```toAttributes :: proxy a -> Attributes```
     * generates ```Attributes``` from a ```Tupleable``` value- can be used with ```undefined```, so an actual value is not necessary
 * ```toTuple :: a -> RelationTuple```
     * converts a ```Tupleable``` value to a tuple for use in queries
 * ```fromTuple :: RelationTuple -> Either RelationalError a```
     * converts a tuple to ```Tupleable``` value, unless there is an error such as a missing attribute

For example usage, see the [Out of the Tarpit example](/examples/out_of_the_tarpit.hs).

## Comparison to Other DBMSes

The most annoying misfeature of the so-called object-relational model (ORM) is the data type impedance mismatch. This is especially egregious for Haskell clients which must contend with down-sampling the quality of Haskell data structures. For example, most DBMSes don't support algebraic data types, so the ORM can't represent them or it must represent them as binary or JSON blobs in the database which precludes such types from database-level type safety, constraints, and value queries.

Project:M36 solves this mismatch by supporting algebraic data types as database-level values, Haskell scripting of database- and value-level server-side functions, and automatic and lossless marshaling of Haskell algebraic data types to values and tuples. Because the mapping between client data types and database-level data types is one-to-one, the mismatch is eliminated- any expression which could be evaluated against the data type on the client side can also be evaluated server-side.
