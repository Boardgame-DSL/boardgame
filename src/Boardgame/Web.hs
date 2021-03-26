{-# LANGUAGE LambdaCase #-}

module Boardgame.Web (
    playWeb
  , defaultWebGame
  , addWebGame
  , webReady
) where

import Asterius.Aeson (jsonFromJSVal, jsonToJSVal)
import Asterius.Types (JSVal)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Boardgame (
    Player(..)
  , PositionalGame(..)
  , playerToInt
  , play
  )

foreign import javascript "wrapper" jsMakeCallback :: IO () -> IO JSVal

foreign import javascript "boardgame._putState($1)" jsPutState :: JSVal -> IO ()
foreign import javascript "boardgame._putTurn($1)" jsPutTurn :: Int -> IO ()
foreign import javascript safe "boardgame._getMove()" jsGetMove :: IO JSVal
foreign import javascript "boardgame._putInvalidInput()" jsPutInvalidInput :: IO ()
foreign import javascript "boardgame._putInvalidMove()" jsPutInvalidMove :: IO ()
foreign import javascript "boardgame._putGameOver($1)" jsPutGameOver :: JSVal -> IO ()
foreign import javascript "boardgame.games[$1] = $2" jsSetGame :: JSVal -> JSVal -> IO ()
foreign import javascript "boardgame._ready()" jsReady :: IO ()

-- | A main function for running games as a web app. Initializes the provided
--   game as "default" and "tells" JavaScript it's ready. Then JavaScript can
--   start games whenever.
defaultWebGame :: (ToJSON a, FromJSON c, PositionalGame a c) => a -> IO ()
defaultWebGame startState = do
  callback <- jsMakeCallback $ playWeb startState
  jsSetGame (jsonToJSVal "default") callback
  jsReady

-- | Adds a named game to the list of games accessible from JavaScript.
addWebGame :: (ToJSON a, FromJSON c, PositionalGame a c) => String -> a -> IO ()
addWebGame name startState = do
  callback <- jsMakeCallback $ playWeb startState
  jsSetGame (jsonToJSVal name) callback

-- | Lets JavaScript know that the Haskell backend is up and running by firing
--   the ready event.
webReady :: IO ()
webReady = jsReady

-- | Plays a 'PositionalGame' with the help of JavaScript FFI. The state of the
--   game ('a') needs to implement 'Data.Aeson.ToJSON' and the coordinates
--   ('c') needs to implement 'Data.Aeson.FromJSON'. This is because they need
--   to be passed to and from (respectively) the JavaScript runtime.
playWeb :: (ToJSON a, FromJSON c, PositionalGame a c) => a -> IO ()
playWeb = play putState putTurn getMove putInvalidMove putGameOver
  where
    putState = jsPutState . jsonToJSVal
    putTurn = jsPutTurn . playerToInt
    getMove = jsGetMove <&> jsonFromJSVal >>= \case
      Left _  -> jsPutInvalidInput >> getMove
      Right c -> return c
    putInvalidMove = jsPutInvalidMove
    putGameOver = jsPutGameOver . jsonToJSVal . fmap playerToInt
