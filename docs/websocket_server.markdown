# Project:M36 WebSocket Server

## Purpose

The Project:M36 WebSocket server makes it easy to connect non-Haskell programs to the Project:M36 database. This server operates as a proxy to an actual Project:M36 database.

## Setup

The websocket server is started with the same arguments as the normal [Project:M36 server](docs/server_mode.markdown):

```
cabal run project-m36-server -- --database mydbname --hostname 127.0.0.1
```

except that the websocket server is now listening for websocket connections on port 8888.

## Client Connection

A [simple JavaScript library](ProjectM36/Server/WebSocket/project-m36.js) to manage the WebSocket connection is available.

## Sample Application

The [WebSocket example application](ProjectM36/Server/WebSocket/websocket-client.html) can be used to learn TutorialD and for remote database access from the web browser.
