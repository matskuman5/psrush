module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (errorShow, log, logShow)
import Foreign (fail, readString)
import Node.Process (lookupEnv)
import Simple.JSON as JSON
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage, onOpen)
import Web.Socket.Event.MessageEvent (fromEvent, data_)
import Web.Socket.WebSocket as WS
import Fetch

data GameInstance = GameInstance {
  gameState :: String,
  owner :: String,
  status :: String,
  createdAt :: String,
  gameType :: String,
  entityId :: String
}

instance showGameInstance :: Show GameInstance where
  show (GameInstance { gameState, owner, status, createdAt, gameType, entityId }) = "GameInstance { gameState: " <> gameState <> ", owner: " <> owner <> ", status: " <> status <> ", createdAt: " <> createdAt <> ", gameType: " <> gameType <> ", entityId: " <> entityId <> " }"

main :: Effect Unit
main = do
  log "Hello, World!"
  runAff_ (\either -> case either of
    Left error -> log $ show error
    Right _ -> do
      envs <- loadEnvs
      case envs of
        Tuple (Just playerToken) (Just levelID) -> do
          runStuff playerToken levelID
        _ -> logShow "error getting envs"
  ) do
    Dotenv.loadFile

runStuff :: String -> String -> Effect Unit
runStuff playerToken levelID = do
  log "running stuff"
  log playerToken
  log levelID

  runAff_ (\either -> case either of
    Left error -> log $ show error
    Right game -> logShow game
  ) do
    createGame playerToken levelID

  connection <- WS.create ("wss://goldrush.monad.fi/backend/" <> playerToken <>  "/") []
  logShow =<< WS.readyState connection

  let socket = WS.toEventTarget connection

  openListener <- eventListener(\event -> do
    log "Connection opened"
    WS.sendString connection ("[\"sub-game\",{\"id\":}]"))
  messageListener <- eventListener(messageReceiver)

  addEventListener onOpen openListener true socket
  addEventListener onMessage messageListener true socket

createGame ∷ String → String → Aff (Either Error GameInstance)
createGame playerToken levelID = do
  _response <- attempt $ fetch ("https://goldrush.monad.fi/backend/api/levels/" <> levelID) { headers: { "Authorization": playerToken }}
  case _response of
    Left e -> do
      pure (Left e)
    Right response -> do
      pure (Right (GameInstance {
        gameState: "waiting",
        owner: "test",
        status: "waiting",
        createdAt: "2021-03-21T12:00:00.000Z",
        gameType: "goldrush",
        entityId: "test"
      }))

messageReceiver :: Event -> Effect Unit
messageReceiver event = do
  log "got a message"
  let message = fromEvent event
  case message of
    Just msg -> do
      logShow $ runExcept $ readString $ data_ msg
    Nothing -> log "no message"

loadEnvs :: Effect (Tuple (Maybe String) (Maybe String))
loadEnvs = do
  playerToken <- lookupEnv "PLAYER_TOKEN"
  levelID <- lookupEnv "LEVEL_ID"
  pure (Tuple playerToken levelID)