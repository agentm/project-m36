# Tutorial D Cheatsheet

The following tables list examples of expressions for `tutd`, Project:M36's "Tutorial D" implementation.

## Relational Expressions

Relational expressions query database state without being able to change it.

|Relational Expression                                |Description                            |
|------------------------------------------------------|---------------------------------------|
|`:showexpr p`|Display the relation variable `p`|
|`:showexpr s rename {city as town}`|Display the result after renaming a relational attribute|
|`:showexpr p{color,city}`|Display the result of the projection on the `color` and `city` attributes|
|`:showexpr p{all but city}`|Display the result of the projection of all attribute of `p` except `city`|
|`:showexpr p join sp`|Display the result of the relational expression `p` joined against `sp` (equivalent to `NATURAL JOIN` in SQL)|
|`:showexpr (s join sp){all from s}`|Display the result of a join but only include attributes from `s`|
|`:showexpr s semijoin sp`|Equivalent to `(s join sp){all from s}`|
|`:showexpr s antijoin sp`|Display the result of all tuples in `s` which do not match appear in the result of `s semijoin sp`|
|`:showexpr s union s`| Display the result of `s` unioned with itself (which is equivalent to `s`)|
|`:showexpr s:{status2:=add(10,@status)}`| Display the result of extending the `s` relation variable with a new attribute which adds 10 to each `status` attribute|
|`:showexpr s group ({sname,status,s#} as subrel)`| Display the result of grouping the `sname`, `status`, and `s#` into a subrel for each tuple in the `s` relation where the `city` attribute (not mentioned) is the grouping criteria|
|`:showexpr (s group ({sname,status,s#} as subrel)) ungroup subrel`| Display the result of unwrapping a subrelation to create one new tuple for each subrelation tuple in the result|
|`:showexpr s minus s`|Display the result after removing all tuples that match the second argument. `x minus x` is equivalent to `x where false`|
|`:showexpr relation{tuple{name "Mike",age 6},tuple{name "Sam",age 10}}`|Display an unnamed relation using manually constructed tuples|
|`:showexpr relation{a Integer, b Text}`|Display an empty-tupled relation of a specified type|
|`:showexpr relation{a Integer, b Text}{tuple{a 4, b "four"}}`|Display a relation bound by the specified types and containing one or more tuples|
|`:showexpr s group ({s#,sname,status} as subrel):{citycount:=count(@subrel)}`|Display a result which groups `s` by `city` and extends the relation by a count for each city.|
|`:showexpr s : { xstatus := if gte(@status, 20) then "good stock" else "low stock" }`|Extend a relational result with a conditional boolean expression|
|`:showexpr s : { xstatus := case @sname of { "Adams" -> Just "expedited order" ; _ -> Nothing }}`|Extend a relation with a case pattern-matching expression.|

|`:showrelvars`|Display a list of relation variables in the current context along with their types (attributes)|

## Built-In Types

|Example Value Expression                              |Type                                   |Explanation|
|------------------------------------------------------|---------------------------------------|-----------|
|`"abc"`|Text|UTF-8 string|
|`123`|Integer|an integer of limitless size (using gmp)|
|`int(456)`|Int|machine-word-based integer|
|`12.56`|Double|64-bit IEEE 754 floating point number|
|`dateTimeFromEpochSeconds(1502304846)`|DateTime||
|`fromGregorian(2017,05,30)`|Date||
|`t`|Bool|`t` or `f`|
|`bytestring("dGVzdGRhdGE=")`|Bytestring|base-64-encoded string of bytes|
|`interval(3,5,False,False)`|Interval(Integer)|the constructor function includes two bounds and two boolean flags for inclusiveness|

## Database Context Expressions

Database context expressions take the current database context as input and alter it to create a new context.

|Database Context Expression                           |Description                            |
|------------------------------------------------------|---------------------------------------|
|`:importexample cjdate`|Imports a precooked schema from Chris Date's books resulting in relvars `s`, `sp`, and `p`|
|`:importtutd "file:///home/agentm/project-m36/scripts/emp.tutd"`|Imports TutorialD from file URI|
|`:importtutd "https://raw.githubusercontent.com/agentm/project-m36/master/scripts/emp.tutd" "effe32b247586dc3ac0079fc241b9618d41d189afcaeb7907edbe5a8b45992a4"`| Imports TutorialD from HTTP or HTTPS URI|
|`newrelvar:=relation{tuple{age 3}}`|Assign a new relation variable named `newrelvar`|
|`undefine s`|Remove a relation variable|
|`insert s relation{tuple{city "Boston",s# "S10",sname "Gonzalez",status 10}}`| Append to an existing relation variable with another relational expression|
|`update s where status=20 (sname:="Mr. Twenty")`|Replace one or more values in matching tuples with a new value|
|`delete sp where s#="S4"`|Remove tuples matching the criteria|
|`key s_key_constraint {s#} s`|Add a constraint named `s_key_constraint` on relation variable `s` ensuring that `s#` is unique|
|`foreign key s#_in_sp sp{s#} in s{s#}`|Add a foreign key constraint that ensures that `sp`'s `s#` attribute always references a `s#` value in `s`|
|`constraint s_status_less_than_50 (s where ^lt(50,@status)){} in false`|Add a constraint that stipulates that the `status` values in `s` are less than 50|
|`createarbitraryrelation employee {name Text, empid Int, hired DateTime} 3-100`|Create a relation variable with random values with between 3-100 tuples- useful for testing|
|`notify steve_change person where name="Steve" true (person where name="Steve"){address}`|Notify the current connection with an asynchronous event if any committed tuples matching `person where name="Steve"` with `true` for the pre-change state and `person where name="Steve"){address}` for the post-change state- these states will be sent along with the notification|
|`data Hair = Bald | Brown | Blond | OtherColor Text`|Create a new algebraic data type for use with values inside tuples|
|`undata Hair`|Delete the `Hair` custom type|

Relation variable and attribute names must begin with a lowercase letter unless quoted with backticks.


## Transaction Graph Expressions

Transaction graph expressions alter the state of the transaction graph by append new states.

|Transaction Graph Expression                          |Description                            |
|------------------------------------------------------|---------------------------------------|
|`:commit`|Save the current database context to a new transaction and add it to the transaction graph|
|`:rollback`|Return the database context to the state before any alterations were made|
|`:branch experimental`|Create a new branch from the existing branch and switch the current context to be able to commit to the new branch|
|`:jumphead master`|Discard any changes in the current context and jump to the head of a different branch|
|`:jump 11b601c9-f46b-42b0-a2ce-cf18ced31b7f`|Jump to the specific transaction by UUID- useful for introspecting past states|
|`:showgraph`|Display a representation of the transaction graph|
|`:walkbacktotime "2017-08-10 16:44:00"`|Walk back in time from the current branch until a transaction less than or equal to the time is found and switch the current context to that past transaction- useful for auditing|

## Data Frame Expressions

In contrast to relational expressions, data frame expressions generate ordered tuples.

|Data Frame Expression                                 |Description                            |
|------------------------------------------------------|---------------------------------------|
|`:showdataframe s orderby {status}`|Display a data frame result for the relational expression `s` and sort the tuples by `status`|
|`:showdataframe s{status} orderby {status descending} offset 1 limit 3`|Display a data frame result for `s{status}` but ordered by `status` values descending, skip the first tuple, and return no more than 3 tuples|

## Trans-Graph Relational Expressions

Trans-graph relational expressions allow queries spanning multiple transactions.

|Trans-Graph Relational Expressions                    |Description                            |
|------------------------------------------------------|---------------------------------------|
|`:showtransgraphexpr s@master join sp@master`|Display a result for joining `s` and `sp` on the branch `master`|
|`:showtransgraphexpr s@master~ join sp@master`|Display a result for joining `s` on the parent transaction of `master`'s current head and `sp` on master|
|`:showtransgraphexpr s@master~^2`|Display a result for `s` two parents previous to the parent of the current `master`'s head|
 
