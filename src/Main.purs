module Main where

import Data.Maybe
import Prelude

import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Foreign (readString)
import Node.Process (lookupEnv)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage, onOpen)
import Web.Socket.Event.MessageEvent (fromEvent, data_)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS

import Control.Monad.Except (runExcept)

main :: Effect Unit
main = do
  log "Hello, World!"
  loadEnvs
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

loadEnvs :: Effect Unit
loadEnvs = launchAff_ do
  Dotenv.loadFile
  liftEffect do
    playerToken <- lookupEnv "PLAYER_TOKEN"
    levelID <- lookupEnv "LEVEL_ID"
    logShow playerToken
    logShow levelID