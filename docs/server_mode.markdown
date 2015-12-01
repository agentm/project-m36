# Project:M36 Server Mode

## Introduction

While Project:M36 is easy-to-use as an interactive, in-process database management system, it can also be run as a server-based daemon to serve requests from multiple client applications.

## Starting the Server

To start the Project:M36 server to serve the local host with a database named "mydbname", run ```cabal run project-m36-server -- --database mydbname --hostname 127.0.0.1``` in the project-m36 source directory. The double-dash indicates that the arguments should be passed to ```project-m36-server``` instead of ```cabal```. Under normal conditions, this command will print nothing and block indefinitely to serve incoming requests on the default port 6543.

Note that the above invocation will serve and in-memory, transient "mydbname" database. The database is deleted when the server exits. To serve a database with filesystem persistence, invoke ```cabal run project-m36-server -- --database mydbname --hostname 127.0.0.1 --database-directory /path/to/dbdirectory --fsync```. 

## Connecting an Interactive Client

To start an interactive session with the server, run ```cabal run tutd -- --database mydbname --hostname 127.0.0.1```. This will open a TutorialD shell to the remote database and should operate exactly like a local session. Note that the import and export functions import and export from/to the client host's filesystem. There is currently no function to import or export from/to the server host's filesystem. 

## Connecting a Haskell Client

Connecting to a local or remote database uses the same API found in ```ProjectM36.Client```. See [this annotated example](examples/SimpleClient.hs) for more information.


