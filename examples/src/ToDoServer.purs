module ToDoServer where

import Prelude hiding (apply)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Array    as A
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error, message)
import Effect.Ref (Ref, modify', read, new)
import Node.Express.App (App, listenHttp, useOnError, get, use, setProp)
import Node.Express.Handler (Handler, nextThrow, next)
import Node.Express.Request (getRouteParam, getQueryParam, getOriginalUrl,
                             setUserData, getUserData)
import Node.Express.Response (sendJson, setStatus)
import Node.HTTP (Server)
import Node.Process (lookupEnv)

--- Model type definitions
type Todo        = { desc :: String, isDone :: Boolean }
type IndexedTodo = { id :: Int, desc :: String, isDone :: Boolean }

-- Global state data
type AppStateData = Array Todo
type AppState     = Ref AppStateData
type AppError     = String

initState :: Effect AppState
initState = new ([] :: AppStateData)

{-
  Model manipulation functions
  Each function receives current state data and returns a record with
  the updated state and Either error or value. If no value is assumed
  then Unit is returned as a value. If Left error is returned then
  the state is returned unchanged.

  The returned record structure is compatible with modifyRef' function
  and thus error could be examined as a return value from modifyRef'
-}
type ChangeResult a = { state :: AppStateData, value :: Either AppError a }

addTodo :: Todo -> AppStateData -> ChangeResult Int
addTodo todo statedata =
  { state: A.snoc statedata todo, value: Right (A.length statedata + 1) }

updateTodo :: Int -> String -> AppStateData -> ChangeResult Unit
updateTodo id newDesc statedata =
  case A.modifyAt id (\el -> el { desc = newDesc }) statedata of
    Nothing  -> { state: statedata, value: Left "No such ID" }
    Just arr -> { state: arr,       value: Right unit }

deleteTodo :: Int -> AppStateData -> ChangeResult Unit
deleteTodo id statedata =
  case A.deleteAt id statedata of
    Nothing  -> { state: statedata, value: Left "No such ID" }
    Just arr -> { state: arr,       value: Right unit }

setDone :: Int -> AppStateData -> ChangeResult Unit
setDone id statedata =
  case A.modifyAt id (\el -> el { isDone = true }) statedata of
    Nothing  -> { state: statedata, value: Left "No such ID" }
    Just arr -> { state: arr,       value: Right unit }

getTodosWithIndexes :: Array Todo -> Array IndexedTodo
getTodosWithIndexes items =
  A.zipWith (\item idx -> {id: idx, desc: item.desc, isDone: item.isDone })
            items
            (A.range 0 $ A.length items)

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str


-- Monadic handlers
logger :: AppState -> Handler
logger state = do
  todos <- liftEffect $ read state
  url   <- getOriginalUrl
  liftEffect $ log (">>> " <> url <> " count =" <> (show $ A.length todos))
  setUserData "logged" url
  next

errorHandler :: AppState -> Error -> Handler
errorHandler state err = do
  setStatus 400
  sendJson {error: message err}

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

indexHandler :: AppState -> Handler
indexHandler _ = do
  sendJson help

-- demonstrates middleware-to-handler communication through
-- setUserData/getUserData functions
getLoggerStatus :: AppState -> Handler
getLoggerStatus _ = do
  userdata <- getUserData "logged"
  sendJson $ fromMaybe "missing" userdata

listTodosHandler :: AppState -> Handler
listTodosHandler state = do
  todos <- liftEffect $ read state
  sendJson $ getTodosWithIndexes todos

createTodoHandler :: AppState -> Handler
createTodoHandler state = do
  descParam <- getQueryParam "desc"
  case descParam of
    Nothing -> nextThrow $ error "Description is required"
    Just desc -> do
      newId <- liftEffect $ modify' (addTodo { desc: desc, isDone: false }) state
      sendJson {status: "Created", id: newId}

updateTodoHandler :: AppState -> Handler
updateTodoHandler state = do
  idParam   <- getRouteParam "id"
  descParam <- getQueryParam "desc"
  case [idParam, descParam] of
    [Just id, Just desc] -> do
      res <- liftEffect $ modify' (updateTodo (parseInt id) desc) state
      case res of
        Left msg -> nextThrow $ error msg
        _        -> sendJson {status: "Updated"}
    _ -> nextThrow $ error "Id and Description are required"

deleteTodoHandler :: AppState -> Handler
deleteTodoHandler state = do
  idParam <- getRouteParam "id"
  case idParam of
    Nothing -> nextThrow $ error "Id is required"
    Just id -> do
      res <- liftEffect $ modify' (deleteTodo (parseInt id)) state
      case res of
        Left msg -> nextThrow $ error msg
        _        -> sendJson {status: "Deleted"}

doTodoHandler :: AppState -> Handler
doTodoHandler state = do
  idParam <- getRouteParam "id"
  case idParam of
    Nothing -> nextThrow $ error "Id is required"
    Just id -> do
      res <- liftEffect $ modify' (setDone (parseInt id)) state
      case res of
        Left msg -> nextThrow $ error msg
        _        -> sendJson {status: "Done"}

appSetup :: AppState -> App
appSetup state = do
  liftEffect $ log "Setting up"
  setProp "json spaces" 4.0
  use               (logger            state)
  get "/"           (indexHandler      state)
  get "/list"       (listTodosHandler  state)
  get "/create"     (createTodoHandler state)
  get "/update/:id" (updateTodoHandler state)
  get "/delete/:id" (deleteTodoHandler state)
  get "/done/:id"   (doTodoHandler     state)
  get "/logger"     (getLoggerStatus   state)
  useOnError        (errorHandler      state)

main :: Effect Server
main = do
  state <- initState
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  listenHttp (appSetup state) port \_ ->
    log $ "Listening on " <> show port
