{-# LANGUAGE LambdaCase #-}

module MyLib.Web (
    playWeb
  , webDefaultMain
) where

import Asterius.Aeson (jsonFromJSVal, jsonToJSVal)
import Asterius.Types (JSVal)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import MyLib (
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
foreign import javascript "boardgame.startGame = $1" jsSetGame :: JSVal -> IO ()
foreign import javascript "boardgame._ready()" jsReady :: IO ()

-- | A main function for running games as a we app. Initializes the game and
--   "tells" JavaScript it's ready. Then JavaScript can start games whenever.
webDefaultMain :: (ToJSON a, FromJSON c, PositionalGame a c) => a -> IO ()
webDefaultMain startState = do
  callback <- jsMakeCallback $ playWeb startState
  jsSetGame callback
  jsReady

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
