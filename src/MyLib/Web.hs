{-# LANGUAGE LambdaCase #-}

module MyLib.Web (
    playWeb
) where

import Asterius.Aeson (jsonFromJSVal, jsonToJSVal)
import Asterius.Types (JSVal)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import MyLib (
    Player(..)
  , PositionalGame(..)
  , play
  )

foreign import javascript "boardgame.putState($1)" jsPutState :: JSVal -> IO ()
foreign import javascript "boardgame.putTurn($1)" jsPutTurn :: Int -> IO ()
foreign import javascript safe "boardgame.getMove()" jsGetMove :: IO JSVal
foreign import javascript "boardgame.putInvalidInput()" jsPutInvalidInput :: IO ()
foreign import javascript "boardgame.putInvalidMove()" jsPutInvalidMove :: IO ()
foreign import javascript "boardgame.putGameOver($1)" jsPutGameOver :: Int -> IO ()

-- | Plays a 'PositionalGame' with the help of JavaScript FFI. The state of the
--   game ('a') needs to implement 'Data.Aeson.ToJSON' and the coordinates
--   ('c') needs to implement 'Data.Aeson.FromJSON'. This is because they need
--   to be passed to and from (respectively) the JavaScript runtime.
playWeb :: (ToJSON a, FromJSON c, PositionalGame a c) => a -> IO ()
playWeb = play putState putTurn getMove putInvalidMove putGameOver
  where
    putState = jsPutState . jsonToJSVal
    putTurn p = jsPutTurn $ case p of
      Player1 -> 1
      Player2 -> 2
    getMove = jsGetMove <&> jsonFromJSVal >>= \case
      Left _  -> jsPutInvalidInput >> getMove
      Right c -> return c
    putInvalidMove = jsPutInvalidMove
    putGameOver = \case
      Just Player1 -> jsPutGameOver 1
      Just Player2 -> jsPutGameOver 2
      Nothing      -> jsPutGameOver 0
