# Project:M36 Relational Algebra Engine

##Introduction
Project:M36 implements a relational algebra engine as inspired by the writings of Chris Date.

## Installation

1. Install [Haskell Platform](https://www.haskell.org/platform/)
1. ```git clone https://github.com/agentm/project-m36```
2. ```cd project-m36```
3. ```cabal configure --enable-tests```
4. ```cabal build```
5. ```./dist/build/tutd/tutd``` to run the Tutorial D interpreter
6. ```cabal test``` to run test suite

## Usage

Currently, the best-supported frontend is the Tutorial D interpreter. Run `cabal run tutd` in newer versions of cabal or `./dist/build/tutd/tutd` in older version of cabal to start the command line interpreter. You will be greeted with:

`TutorialD:`

The Tutorial D interpreter comes pre-loaded with the Chris Date supplier "S"/product "P"/supplier-product "SP" relations for experimentation. This TutorialD interpreter is case-sensitive. 

At this point, there are three types of command which can be executed:

1. relational algebra expressions such as: 

  ```
TutorialD: :s S join P
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

  where ":s" instructs the interpreter to show the resultant relation, "S" and "P" are relation names, and "join" is the join operator.
2. interpreter-level queries such as:

  ```
TutorialD: :t S
{CITY "char", S# "char", SNAME "char", STATUS "char"}
```

  which shows the Tutorial D type for a relational expression and 

  ```
TutorialD: :c
fromList []
```
which displays database constraints.

3. expressions which modify the database context such as:

  ```
TutorialD: x := S JOIN P
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