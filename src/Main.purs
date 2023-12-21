module Main where

import Prelude

import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Process (lookupEnv)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage, onOpen)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WS

main :: Effect Unit
main = do
  log "Hello, World!"
  loadEnvs
  connection <- WS.create "ws://echo.websocket.events/.ws" []
  logShow =<< WS.readyState connection

  let socket = WS.toEventTarget connection
  messageListener <- eventListener(\event -> logShow "got a message")
  addEventListener onMessage messageListener true socket


loadEnvs :: Effect Unit
loadEnvs = launchAff_ do
  Dotenv.loadFile
  liftEffect do
    playerToken <- lookupEnv "PLAYER_TOKEN"
    levelID <- lookupEnv "LEVEL_ID"
    logShow playerToken
    logShow levelID