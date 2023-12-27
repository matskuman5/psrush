module Main where

import Prelude

import Affjax (Error, Response)
import Affjax.Node as AN
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (defaultRequest)
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, jsonParser, toArray)
import Data.Array (elem, filter, head, last, length, (!!))
import Data.Either (Either(..))
import Data.HTTP.Method as Method
import Data.Int (binary, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.String as S
import Data.Tuple (Tuple(..))
import Dotenv as Dotenv
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, runAff_)
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

data Action = Move | Reset | Rotation Direction

data Direction = N | NE | E | SE | S | SW | W | NW | NullDir

derive instance eqDirection :: Eq Direction

instance showAction :: Show Action where
  show Move = "Move"
  show Reset = "Reset"
  show (Rotation d) = show (directionToNumber d)

directionToNumber :: Direction -> Int
directionToNumber direction = case direction of
  N -> 0
  NE -> 45
  E -> 90
  SE -> 135
  S -> 180
  SW -> 225
  W -> 270
  NW -> 315
  NullDir -> 999

numberToDirection :: Int -> Direction
numberToDirection number = case number of
  0 -> N
  45 -> NE
  90 -> E
  135 -> SE
  180 -> S
  225 -> SW
  270 -> W
  315 -> NW
  _ -> N -- should never happen

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
          
          messageListener <- eventListener(\event -> do
            gameState <- messageToGameState event
            case gameState of
              Nothing -> log "failed to parse game state"
              Just state -> do
                runAff_ (\either2 -> case either2 of
                  Left err -> logShow err
                  Right _ -> do
                    log $ "current walls: " <> show (toBinaryString (state.square))
                    WS.sendString connection (actionToJson (getNextAction state) realgame.entityId)
                    log $ "sent action " <> show (getNextAction state)) (delay $ Milliseconds 1000.0))

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

-- TODO: implement a smarter algorithm
-- moves forward if possible, otherwise rotates in the first open direction it finds
getNextAction :: GameState -> Action
getNextAction gameState = if playerDir `elem` openDirs
  then Move
  else case randomDir of
    Nothing -> Move
    Just dir -> Rotation dir
    where playerDir = numberToDirection (gameState.player.rotation)
          randomDir = head openDirs
          openDirs = openDirections (toBinaryString (gameState.square))

toBinaryString :: Int -> String
toBinaryString n = case S.length bitString of
  4 -> bitString
  3 -> "0" <> bitString
  2 -> "00" <> bitString
  1 -> "000" <> bitString
  _ -> "0000"
  where bitString = toStringAs binary n

openDirections :: String -> Array Direction
openDirections bitString = filter (\d -> d /= NullDir) [nN, nE, nS, nW]
  where nN = if bits !! 0 == Just '0' then N else NullDir
        nE = if bits !! 1 == Just '0' then E else NullDir
        nS = if bits !! 2 == Just '0' then S else NullDir
        nW = if bits !! 3 == Just '0' then W else NullDir
        bits = toCharArray bitString

actionToJson :: Action -> String -> String
actionToJson action gameId = case action of
  Move -> "[\"run-command\", {\"gameId\": \"" <> gameId <> "\", \"payload\": {\"action\": \"move\"}}]"
  Reset -> "[\"run-command\", {\"gameId\": \"" <> gameId <> "\", \"payload\": {\"action\": \"reset\"}}]"
  Rotation d -> "[\"run-command\", {\"gameId\": \"" <> gameId <> "\", \"payload\": {\"action\": \"rotate\", \"rotation\": " <> show (directionToNumber d) <> "}}]"

messageToGameState :: Event -> Effect (Maybe GameState)
messageToGameState event = do
  log "got a message"
  let message = fromEvent event
  case message of
    Just msg -> case runExcept $ readString $ data_ msg of
      Left e -> do
        logShow e
        pure Nothing
      Right str -> do
        case jsonParser str of
          Left e -> do
            logShow e
            pure Nothing
          Right gameState -> case toArray gameState of
            Nothing -> do
              pure Nothing
            Just arr -> do
              case arr !! 1 of
                Nothing -> do
                  pure Nothing
                Just realGameState -> do
                  case gameInstanceFromJson realGameState of
                    Left e -> do
                      logShow e
                      pure Nothing
                    Right gameInstance -> case jsonParser gameInstance.gameState of
                      Left e -> do
                        logShow e
                        pure Nothing
                      Right bullshit -> case gameStateFromJson bullshit of
                        Left e -> do
                          logShow e
                          pure Nothing
                        Right crap -> pure (Just crap)
    Nothing -> do
      pure Nothing

loadEnvs :: Effect (Tuple (Maybe String) (Maybe String))
loadEnvs = do
  playerToken <- lookupEnv "PLAYER_TOKEN"
  levelID <- lookupEnv "LEVEL_ID"
  pure (Tuple playerToken levelID)