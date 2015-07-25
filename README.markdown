# Project:M36 Relational Algebra Engine

##Introduction
Project:M36 implements a relational algebra engine as inspired by the writings of Chris Date.

## Installation

1. Install [Haskell Platform](https://www.haskell.org/platform/)
1. ```git clone https://github.com/agentm/project-m36```
2. ```cd project-m36```
3. ```cabal sandbox init```
4. ```cabal configure --enable-tests```
5. ```cabal install --enable-tests```
6. ```cabal run tutd``` to run the Tutorial D interpreter
7. ```cabal test``` to run test suite

## Usage

Currently, the best-supported frontend is the Tutorial D interpreter. Run `cabal run tutd` to start the command line interpreter linked to an in-memory database. Alternatively, to create a database backed by the filesystem (to be able to save committed transactions in the filesystem), use `cabal run tutd -- -d <db directory>`.

You will be greeted with:

`TutorialD (master):`

To load Chris Date's supplier/product examples, run `:importtutd "scripts/DateExamples.tutd"` from within the tutd interpreter. After this, the `S`, `P`, and `SP` relation variables are available within the current context.

This TutorialD interpreter is case-sensitive. 

At this point, there are three types of command which can be executed:

1. relational algebra expressions such as: 

  ```
TutorialD (master): :showexpr S join P
┌──────┬─────┬──┬─────┬──┬─────┬──────┬──────┐
│CITY  │COLOR│P#│PNAME│S#│SNAME│STATUS│WEIGHT│
├──────┼─────┼──┼─────┼──┼─────┼──────┼──────┤
│Paris │Blue │P5│Cam  │S3│Blake│30    │12    │
│London│Red  │P1│Nut  │S4│Clark│20    │12    │
│London│Red  │P1│Nut  │S1│Smith│20    │12    │
│Paris │Blue │P5│Cam  │S2│Jones│10    │12    │
│London│Red  │P6│Cog  │S1│Smith│20    │19    │
│London│Red  │P6│Cog  │S4│Clark│20    │19    │
│Paris │Green│P2│Bolt │S3│Blake│30    │17    │
│London│Red  │P4│Screw│S1│Smith│20    │14    │
│London│Red  │P4│Screw│S4│Clark│20    │14    │
│Paris │Green│P2│Bolt │S2│Jones│10    │17    │
└──────┴─────┴──┴─────┴──┴─────┴──────┴──────┘
```

  where ":showexpr" instructs the interpreter to show the resultant relation, "S" and "P" are relation names, and "join" is the join operator.
2. interpreter-level queries such as:

  ```
TutorialD (master): :type S
{CITY "char", S# "char", SNAME "char", STATUS "char"}
```

  which shows the Tutorial D type for a relational expression and 

  ```
TutorialD (master): :constraints
fromList []
```
which displays database constraints.

  ```
TutorialD (master): :showplan x:=S{CITY,S#,SNAME,STATUS}
Right (Assign "x" (RelationVariable "S"))
```
which displays the optimized execution plan which would have been executed to evaluate the expression.

  ```
TutorialD (master): :commit
Done.
```
which commits the current "disconnected" transaction which is similar to the git staging area/index.

  ```
TutorialD (master): :jumphead master
Done.
```
which changes the current disconnected transaction to reference another head.

  ```
TutorialD (master): :jump 4a4ee715-297d-49db-bfd1-afaefadc902b
Done.
```
which resets the current "disconnected" transaction to refer to the committed transaction specified by the UUID.

  ```
TutorialD (master): :branch devel
Done.
```

which creates a new head named "devel" in the transaction graph and resets the current "disconnected" transaction to refer to the new branch.

  ```
TutorialD (devel): :rollback
Done.
```

which resets the current "disconnected" transaction to discard any collected changes.

  ```
TutorialD (devel): :showGraph
┌───────┬────────────────────────────────────┬──────────────────────────────────────┐
│current│id                                  │parents                               │
├───────┼────────────────────────────────────┼──────────────────────────────────────┤
│True   │1d064d54-cd2a-4a43-822a-cbd18cf7c4e8│┌────────────────────────────────────┐│
│       │                                    ││id                                  ││
│       │                                    │├────────────────────────────────────┤│
│       │                                    ││4a4ee715-297d-49db-bfd1-afaefadc902b││
│       │                                    │└────────────────────────────────────┘│
│False  │4a4ee715-297d-49db-bfd1-afaefadc902b│┌──┐                                  │
│       │                                    ││id│                                  │
│       │                                    │                                      │
│       │                                    │└──┘                                  │
└───────┴────────────────────────────────────┴──────────────────────────────────────┘
```

which displays all committed transactions and their parents. The "current" attribute indicates to which transactions the current "disconnected" transaction refers.

3. expressions which modify the database context such as:

  ```
TutorialD (master): x := S JOIN P
```
  which sets the relational variable named "x" to the results of "S join P" if the database constraint checking succeeds.

  To create a new constraint which are represented as inclusion dependencies:

  ```
constraint check_x false in true
```

  checks that the expression "false" is a subrelation of "true", which, in this case, is always true.
  
## Development

Project:M36 is developed in Haskell and compiled with GHC. There are three primary, independent modules:

1. Relation - represents a relation in the relational algebra and implements the relational operators
2. Relational Expressions - represents a composable abstract syntax tree for the relational algebra
3. Tutorial D Interpreter - one available frontend to evaluate relational expressions

Other modules include:

* displaying a relation as HTML
* displaying a relation in the terminal
* relational expression static optimizer
* relational expression cost-based optimizer
* relational error enumeration
* import of CSV as a relation and export of a relation to CSV
* import of TutorialD from files