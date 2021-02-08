{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad (join)
import Data.List (find, intercalate)
import Data.Map (Map, elems, fromDistinctAscList, insert, member, lookup, (!))
import Data.Maybe (isJust)
import MyLib (
    Player(..)
  , PositionalGame(..)
  , StrongPositionalGame(..)
  , nextPlayer
  , strongPositionalGameMakeMove
  , strongPositionalGameGameOver
  , player
  )
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

newtype TicTacToe = TicTacToe (Map (Integer, Integer) (Maybe Player))

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
      row y = map (\x -> showP $ b ! (x, y)) [0..2]
      showP (Just Player1) = "o"
      showP (Just Player2) = "x"
      showP Nothing = " "

instance StrongPositionalGame TicTacToe (Integer, Integer) where
  position (TicTacToe b) = flip lookup b
  positions (TicTacToe b) = elems b
  setPosition (TicTacToe b) c p = if member c b then Just $ TicTacToe $ insert c (Just p) b else Nothing

instance PositionalGame TicTacToe (Integer, Integer) where
  makeMove = strongPositionalGameMakeMove
  gameOver = strongPositionalGameGameOver patterns

patterns :: [[(Integer, Integer)]]
patterns = [
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
