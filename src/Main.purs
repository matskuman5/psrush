module Main where

import Prelude

import Affjax (Error, Response)
import Affjax.Node as AN
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (defaultRequest)
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, jsonParser, toArray)
import Data.Array (last, length, (!!))
import Data.Either (Either(..))
import Data.HTTP.Method as Method
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Foreign (readString)
import Node.Process (lookupEnv)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage, onOpen)
import Web.Socket.Event.MessageEvent (fromEvent, data_)
import Web.Socket.WebSocket as WS

type GameInstance = {
  gameState :: String,
  ownerId :: String,
  status :: String,
  createdAt :: String,
  gameType :: String,
  entityId :: String
}

-- TODO: make rotation its own type
type GameState = {
  player :: Player,
  moves :: Int,
  timer :: Int,
  start :: Position,
  startRotation :: Int,
  target :: Position,
  rows :: Int,
  columns :: Int,
  square :: Int
}

type Position = {
  x :: Int,
  y :: Int
}

type Player = {
  position :: Position,
  rotation :: Int
}

data Action = Move | Reset | Int

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
    Left str -> log $ show str
    Right game -> do
      log $ "game created"
      logShow game
      case game of
        Left str -> log "error"
        Right realgame -> do
          connection <- WS.create ("wss://goldrush.monad.fi/backend/" <> playerToken <>  "/") []
          logShow =<< WS.readyState connection

          let socket = WS.toEventTarget connection

          openListener <- eventListener(\event -> do
            log "Connection opened"
            WS.sendString connection ("[\"sub-game\",{\"id\":\""<> realgame.entityId <> "\"}]"))
          messageListener <- eventListener(messageReceiver)

          addEventListener onOpen openListener true socket
          addEventListener onMessage messageListener true socket
      
  ) do
    createGame playerToken levelID

gameInstanceFromJson :: Json -> Either JsonDecodeError GameInstance
gameInstanceFromJson = decodeJson

gameStateFromJson :: Json -> Either JsonDecodeError GameState
gameStateFromJson = decodeJson

createGame :: String -> String -> Aff (Either String GameInstance)
createGame playerToken levelID = do
  result <- AN.request (defaultRequest { url = ("https://goldrush.monad.fi/backend/api/levels/" <> levelID), method = Left Method.POST, headers = [(RequestHeader "Authorization" playerToken)], responseFormat = RF.string })
  case result of
    Left err -> pure (Left "GET /api response failed to decode")
    Right response -> case jsonParser response.body of
      Left err -> pure (Left "failed to parse json")
      Right game -> case gameInstanceFromJson game of
        Left e -> liftEffect do
          logShow response.body
          pure (Left (show e))
        Right gameInstance -> pure (Right gameInstance)

messageReceiver :: Event -> Effect Unit
messageReceiver event = do
  log "got a message"
  let message = fromEvent event
  case message of
    Just msg -> case runExcept $ readString $ data_ msg of
      Left e -> logShow e
      Right str -> do
        case jsonParser str of
          Left e -> logShow e
          Right gameState -> case toArray gameState of
            Nothing -> log "error decoding array"
            Just arr -> do
              case arr !! 1 of
                Nothing -> log "error getting last element"
                Just realGameState -> do
                  case gameInstanceFromJson realGameState of
                    Left e -> logShow e
                    Right gameInstance -> case jsonParser gameInstance.gameState of
                      Left e -> logShow e
                      Right bullshit -> case gameStateFromJson bullshit of
                        Left e -> logShow e
                        Right crap -> logShow crap
    Nothing -> log "no message"

loadEnvs :: Effect (Tuple (Maybe String) (Maybe String))
loadEnvs = do
  playerToken <- lookupEnv "PLAYER_TOKEN"
  levelID <- lookupEnv "LEVEL_ID"
  pure (Tuple playerToken levelID)