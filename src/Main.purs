module Main where

import Data.Maybe (Maybe(..))
import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log, logShow)
import Foreign (readString)
import Node.Process (lookupEnv)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage, onOpen)
import Web.Socket.Event.MessageEvent (fromEvent, data_)
import Web.Socket.WebSocket as WS

main :: Effect Unit
main = do
  log "Hello, World!"
  runAff_ (\either -> case either of
    Left error -> log $ show error
    Right _ -> do
      envs <- loadEnvs
      logShow envs
  ) do
    Dotenv.loadFile

  connection <- WS.create "ws://echo.websocket.events/.ws" []
  logShow =<< WS.readyState connection

  let socket = WS.toEventTarget connection

  openListener <- eventListener(\event -> logShow "Connection opened")
  messageListener <- eventListener(messageReceiver)

  addEventListener onOpen openListener true socket
  addEventListener onMessage messageListener true socket

messageReceiver :: Event -> Effect Unit
messageReceiver event = do
  logShow "got a message"
  let message = fromEvent event
  case message of
    Just msg -> do
      logShow $ runExcept $ readString $ data_ msg
    Nothing -> logShow "no message"

loadEnvs :: Effect (Tuple (Maybe String) (Maybe String))
loadEnvs = do
  playerToken <- lookupEnv "PLAYER_TOKEN"
  levelID <- lookupEnv "LEVEL_ID"
  pure (Tuple playerToken levelID)