# Project:M36 Persistent Library Driver

Project:M36 includes a driver for the [persistent](https://hackage.haskell.org/package/persistent) library which offers lowest-common-denominator DBMS support for storing and retrieving Haskell-based datatypes. While the persistent library does not support every Project:M36 feature, it can nonetheless be handy for simple use-cases and database-agnostic mapping.

For the most part, using the Project:M36 driver should be identical to using other drivers.

## Setup

Use ```withProjectM36Conn``` with a configuration argument to add a ReaderT to your monad stack. The, follow that with ```runProjectM36Conn``` to run the monad.

## Usage

At this point, all persistent functions are available. The driver always commits to the "master" branch.

## Caveats

* Not every persistent data type is supported yet.
* Tuple ordering in selects is not yet supported.
