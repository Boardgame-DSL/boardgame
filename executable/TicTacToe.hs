{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module TicTacToe where

import Data.List (intercalate)
  
import Data.Map (
    Map
  , insert
  , member
  , elems
  , lookup
  , (!)
  , fromDistinctAscList
  )

import Prelude hiding (lookup)

import Boardgame (
    Player(..)
  , Position(..)
  , PositionalGame(..)
  , patternMatchingGameOver
  )

#ifdef WASM
import qualified Data.Vector as V ((!), fromList)
import Data.Aeson.Types
#endif

-------------------------------------------------------------------------------
-- * TicTacToe
-------------------------------------------------------------------------------

newtype TicTacToe = TicTacToe (Map (Integer, Integer) Position)

-- Creates an empty TicTacToe board with coordinates `(0..2, 0..2)`
emptyTicTacToe :: TicTacToe
emptyTicTacToe = TicTacToe $
  fromDistinctAscList $
    zip
      [(x, y) | x <- [0..2], y <- [0..2]]
      (repeat Empty)

instance Show TicTacToe where
  show (TicTacToe b) = intercalate "\n" [
      "╔═══╤═══╤═══╗"
    , "║ " ++ intercalate " │ " (row 0) ++ " ║"
    , "╟───┼───┼───╢"
    , "║ " ++ intercalate " │ " (row 1) ++ " ║"
    , "╟───┼───┼───╢"
    , "║ " ++ intercalate " │ " (row 2) ++ " ║"
    , "╚═══╧═══╧═══╝"
    ]
    where
      -- "Shows" the elements of the given row
      row y = map (\x -> showP $ b ! (x, y)) [0..2]
      showP (Occupied Player1) = "\ESC[34mo\ESC[0m"
      showP (Occupied Player2) = "\ESC[31mx\ESC[0m"
      showP Empty = " "

#ifdef WASM
-- Converts the game to a JSON array with three arrays with three integers
-- each. The integers correspond to
-- 0 → Nothing,
-- 1 → Just Player1, and
-- 2 → Just Player2.
instance ToJSON TicTacToe where
  toJSON (TicTacToe b) = Array $ V.fromList $ map row [0..2]
    where
      row y = Array $ V.fromList $ map (\x -> toJSON $ b ! (x, y)) [0..2]
#endif

instance PositionalGame TicTacToe (Integer, Integer) where
  -- Just looks up the coordinate in the underlying Map
  getPosition (TicTacToe b) = flip lookup b
  -- Just returns the elements in the underlying Map
  positions (TicTacToe b) = elems b
  -- If the underlying Map has the given coordinate, update it with the given player
  setPosition (TicTacToe b) c p = if member c b then Just $ TicTacToe $ insert c p b else Nothing
  -- "Creates" a `gameOver` function by supplying all the winning "patterns"
  gameOver = patternMatchingGameOver [
      [(0, 0), (0, 1), (0, 2)]
    , [(1, 0), (1, 1), (1, 2)]
    , [(2, 0), (2, 1), (2, 2)]
    , [(0, 0), (1, 0), (2, 0)]
    , [(0, 1), (1, 1), (2, 1)]
    , [(0, 2), (1, 2), (2, 2)]
    , [(0, 0), (1, 1), (2, 2)]
    , [(2, 0), (1, 1), (0, 2)]
    ]