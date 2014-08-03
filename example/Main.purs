module Main where

import Debug.Trace
import Data.Maybe
import Data.Array (map, range, zipWith, length)
import Data.Foreign.EasyFFI
import Control.Monad.Eff.Class
import Control.Monad.Eff
import Control.Monad.ST
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


-- Pretend for now that this is our cool database (:

foreign import todos "var todos = [];" :: [Todo]

type Todo = { desc :: String, isDone :: Boolean }
type IndexedTodo = { id :: Number, desc :: String, isDone :: Boolean }

addTodo :: forall e. [Todo] -> Todo -> Eff e Number
addTodo =
    unsafeForeignFunction ["todos", "todo", ""]
    "todos.push(todo) - 1"

updateTodo :: forall e. [Todo] -> Number -> String -> Eff e Unit
updateTodo =
    unsafeForeignProcedure ["todos", "id", "newDesc", ""]
    "(todos[id] || {}).desc = newDesc;"

deleteTodo :: forall e. [Todo] -> Number -> Eff e Unit
deleteTodo =
    unsafeForeignProcedure ["todos", "id", ""]
    "todos.splice(id, 1);"

setDone :: forall e. [Todo] -> Number -> Eff e Unit
setDone =
    unsafeForeignProcedure ["todos", "id", ""]
    "(todos[id] || {}).isDone = true;"

getTodosWithIndexes :: forall e. [Todo] -> Eff e [IndexedTodo]
getTodosWithIndexes =
    unsafeForeignFunction ["todos", ""]
    "todos.map(function(t,i) { return { id: i, desc: t.desc, isDone: t.isDone }; });"

parseNumber :: String -> Number
parseNumber = unsafeForeignFunction ["str"] "parseInt(str);"


logger :: Handler
logger = do
    url <- getOriginalUrl
    liftEff $ trace (">>> " ++ url)
    next

help = { name: "Todo example"
       , purpose: "To present a subset of purescript-express package capabilities"
       , howToUse:
            { listTodos: "/list"
            , createTodo: "/create?desc=Do+something"
            , doTodo: "/done/:id"
            , updateTodo: "/update/:id?desc=Do+something+else"
            , deleteTodo: "/delete/:id"
            }
       , forkMe: "https://github.com/dancingrobot84/purescript-express"
       }

errorHandler :: Number -> String -> Handler
errorHandler status msg = do
    setStatus status
    sendJson {error: msg}

indexHandler :: Handler
indexHandler = sendJson help

listTodosHandler :: Handler
listTodosHandler = do
    indexedTodos <- liftEff $ getTodosWithIndexes todos
    sendJson indexedTodos

createTodoHandler :: Handler
createTodoHandler = do
    descParam <- getQueryParam "desc"
    maybe (errorHandler 400 "Description is required") handleNewTodo descParam
  where
    handleNewTodo desc = do
        newId <- liftEff $ addTodo todos { desc: desc, isDone: false }
        sendJson {status: "Created", id: newId}

updateTodoHandler :: Handler
updateTodoHandler = do
    idParam <- getRouteParam "id"
    descParam <- getQueryParam "desc"
    maybe (errorHandler 400 "Id is required") (\id ->
        maybe (errorHandler 400 "Description is required") (handleUpdateTodo id) descParam) idParam
  where
    handleUpdateTodo id desc = do
        liftEff $ updateTodo todos (parseNumber id) desc
        sendJson {status: "Updated"}

deleteTodoHandler :: Handler
deleteTodoHandler = do
    idParam <- getRouteParam "id"
    maybe (errorHandler 400 "Id is required") handleDeleteTodo idParam
  where
    handleDeleteTodo id = do
        liftEff $ deleteTodo todos (parseNumber id)
        sendJson {status: "Deleted"}

doTodoHandler :: Handler
doTodoHandler = do
    idParam <- getRouteParam "id"
    maybe (errorHandler 400 "Id is required") handleDoTodo idParam
  where
    handleDoTodo id = do
        liftEff $ setDone todos (parseNumber id)
        sendJson {status: "Done"}

appSetup :: App
appSetup = do
    liftEff $ trace "Setting up"
    setProp "json spaces" 4
    use logger
    get "/" indexHandler
    get "/list" listTodosHandler
    get "/create" createTodoHandler
    get "/update/:id" updateTodoHandler
    get "/delete/:id" deleteTodoHandler
    get "/done/:id" doTodoHandler

main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listen appSetup port \_ ->
        trace $ "Listening on " ++ show port
