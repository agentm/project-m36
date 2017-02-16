# Project:M36 JavaScript Driver

## Introduction

 The JavaScript database driver for Project:M36 is a fully-featured database driver for the Project:M36 Relational Algebra Engine DBMS.  The driver's only dependency is a working websockets implementation which is provided by virtually any modern web browser and node.js. The API is callback-based.

 ## Usage

1. [Download the driver](/src/bin/ProjectM36/Websocket/project-m36.js).
1. Connect to the websocket server with a ```new ProjectM36Connection(..)```  including the asynchronous callback functions.
1. When the ```openCallback``` is called, the database is ready to execute TutorialD sing the ```executeTutorialD()``` function.
1. Implement ```statusCallback``` to update your user interface for expected and asynchronous event handling.

## Example

A fully-functioning interactive websocket client is available:

* [websocket-client.html](/src/bin/ProjectM36/Server/WebSocket/websocket-client.html)
* [websocket-client.js](/src/bin/ProjectM36/Server/WebSocket/websocket-client.js)

This example also powers [try.project-m36.io](https://try.project-m36.io).
