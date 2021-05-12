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

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Boardgame (
    PositionalGame(..)
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
