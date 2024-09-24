# Examples for purescript-express

A couple of examples of purescript-express usage

## Installation
In the top-level directory:
```
$ npm install -g spago purescript
$ npm install
$ spago -x examples/spago.dhall build
```

## Running
In the top-level directory:
```
$ spago -x examples/spago.dhall run -m <example-name>
```

## Available examples

#### `HelloWorldServer`

Listens on `8080` and displays `Hello, World!`, nothing else.

#### `ToDoServer`

Simple Todo application. Nothing fancy, just plain text.

#### `EmbedApp`

Shows how to embed purescript-express application into existing express.js
application.

#### `JSMiddleware`

Shows how to use external middleware in purescript-express application.
