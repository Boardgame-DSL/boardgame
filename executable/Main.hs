{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.List (intercalate)
import Data.Map (
    Map
  , elems
  , fromDistinctAscList
  , insert
  , lookup
  , member
  , (!)
  )
import MyLib (
    Player(..)
  , PositionalGame(..)
  , StrongPositionalGame(..)
  , strongPositionalGameMakeMove
  , strongPositionalGameGameOver
  , player
  )
import Prelude hiding (lookup)

newtype TicTacToe = TicTacToe (Map (Integer, Integer) (Maybe Player))

-- Creates an empty TicTacToe board with coordinates `(0..2, 0..2)`
emptyTicTacToe :: TicTacToe
emptyTicTacToe = TicTacToe $
  fromDistinctAscList $
    zip
      [(x, y) | x <- [0..2], y <- [0..2]]
      (repeat Nothing)

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
      showP (Just Player1) = "o"
      showP (Just Player2) = "x"
      showP Nothing = " "

instance StrongPositionalGame TicTacToe (Integer, Integer) where
  -- Just looks up the coordinate in the underlying Map
  position (TicTacToe b) = flip lookup b
  -- Just returns the elements in the underlying Map
  positions (TicTacToe b) = elems b
  -- If the underlying Map has the given coordinate, update it with the given player
  setPosition (TicTacToe b) c p = if member c b then Just $ TicTacToe $ insert c (Just p) b else Nothing

instance PositionalGame TicTacToe (Integer, Integer) where
  -- Just uses the "standard" implementation
  makeMove = strongPositionalGameMakeMove
  -- "Creates" a `gameOver` function by supplying all the winning "patterns"
  gameOver = strongPositionalGameGameOver [
      [(0, 0), (0, 1), (0, 2)]
    , [(1, 0), (1, 1), (1, 2)]
    , [(2, 0), (2, 1), (2, 2)]
    , [(0, 0), (1, 0), (2, 0)]
    , [(0, 1), (1, 1), (2, 1)]
    , [(0, 2), (1, 2), (2, 2)]
    , [(0, 0), (1, 1), (2, 2)]
    , [(2, 0), (1, 1), (0, 2)]
    ]

main :: IO ()
main = player emptyTicTacToe
