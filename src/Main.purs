module Main where

import Node.Process (lookupEnv)
import Prelude

import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)

main :: Effect Unit
main = launchAff_ do
  Dotenv.loadFile
  liftEffect do
    log "Hello, World!"
    playerToken <- lookupEnv "PLAYER_TOKEN"
    levelID <- lookupEnv "LEVEL_ID"
    logShow playerToken
    logShow levelID