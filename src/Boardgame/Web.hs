{-# LANGUAGE LambdaCase #-}

{-|
Module:      Boardgame.Web
Description: Functions that interact with a JavaScript runtime through WASM.

This module is useful if you wish to create web UI through the use of
<https://webassembly.org/ WebAssembly> and regular web technologies.

Our complementary <https://github.com/Boardgame-DSL/boardgame.js#boardgamejs boardgame.js>
JavaScript library can be useful as it contains the necessary JavaScript
functions and some extra helper functions.

= Usage example

== Simple
Imagine a 'Boardgame.PositionalGame' called @TicTacToe@, with a function
@newTicTacToe@ that instantiates it. If you wish to build a web UI for the
game, you can use the default way of exposing the Haskell model to the
JavaScript runtime:

> main = defaultWebGame newTicTacToe

After that, the game can be started form the JavaScript runtime with the
function @window.boardgame.games.default()@.

== Multiple games
If you also have another game, say @Hex@ with a function @newHex@. And you want
to play both from the same UI you can instead use the following method.

> main = do
>   addWebGame "TicTacToe" newTicTacToe
>   addWebGame "Hex" newHex
>   webReady

With this, the JavaScript runtime can access both games through the
@window.boardgame.games@ object. @window.boardgame.games.TicTacToe()@ and
@window.boardgame.games.Hex()@ respectively.

The 'webReady' call is used to invoke the @window.boardgame.initialized@ event.

= Remember
Compile with <https://github.com/tweag/asterius/ Asterius> and the @wasm@ flag
active.

> ahc-cabal new-build --flags="wasm"
-}
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
foreign import javascript "boardgame._putGameOver($1, $2)" jsPutGameOver :: JSVal -> JSVal -> IO ()
foreign import javascript "boardgame.games[$1] = $2" jsSetGame :: JSVal -> JSVal -> IO ()
foreign import javascript "boardgame._ready()" jsReady :: IO ()

-- | A main function for running games as a web app. Initializes the provided
--   game as "default" and "tells" JavaScript it's ready. Then JavaScript can
--   start games whenever.
defaultWebGame :: (ToJSON a, ToJSON c, FromJSON c, PositionalGame a c) => a -> IO ()
defaultWebGame startState = do
  callback <- jsMakeCallback $ playWeb startState
  jsSetGame (jsonToJSVal "default") callback
  jsReady

-- | Adds a named game to the list of games accessible from JavaScript.
addWebGame :: (ToJSON a, ToJSON c, FromJSON c, PositionalGame a c) => String -> a -> IO ()
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
playWeb :: (ToJSON a, ToJSON c, FromJSON c, PositionalGame a c) => a -> IO ()
playWeb = play putState putTurn getMove putInvalidMove putGameOver
  where
    putState = jsPutState . jsonToJSVal
    putTurn = jsPutTurn . playerToInt
    getMove = jsGetMove <&> jsonFromJSVal >>= \case
      Left _  -> jsPutInvalidInput >> getMove
      Right c -> return c
    putInvalidMove = jsPutInvalidMove
    putGameOver (x, y) = jsPutGameOver (jsonToJSVal x) (jsonToJSVal y)
