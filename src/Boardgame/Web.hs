{-# LANGUAGE LambdaCase #-}

module Boardgame.Web (
    playWeb
  , defaultWebGame
  , addWebGame
  , webReady
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Boardgame (
    Player(..)
  , PositionalGame(..)
  , playerToInt
  , play
  )

-- | A main function for running games as a web app. Initializes the provided
--   game as "default" and "tells" JavaScript it's ready. Then JavaScript can
--   start games whenever.
defaultWebGame :: (ToJSON a, ToJSON c, FromJSON c, PositionalGame a c) => a -> IO ()
defaultWebGame startState = undefined

-- | Adds a named game to the list of games accessible from JavaScript.
addWebGame :: (ToJSON a, ToJSON c, FromJSON c, PositionalGame a c) => String -> a -> IO ()
addWebGame name startState = undefined

-- | Lets JavaScript know that the Haskell backend is up and running by firing
--   the ready event.
webReady :: IO ()
webReady = undefined

-- | Plays a 'PositionalGame' with the help of JavaScript FFI. The state of the
--   game ('a') needs to implement 'Data.Aeson.ToJSON' and the coordinates
--   ('c') needs to implement 'Data.Aeson.FromJSON'. This is because they need
--   to be passed to and from (respectively) the JavaScript runtime.
playWeb :: (ToJSON a, ToJSON c, FromJSON c, PositionalGame a c) => a -> IO ()
playWeb = undefined
