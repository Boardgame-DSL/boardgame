{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.List (elemIndex, intercalate)
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
import System.IO (hFlush, stdout)
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

data ArithmeticProgressionGame = ArithmeticProgressionGame Int [Maybe Player]

createArithmeticProgressionGame :: Int -> Int -> Maybe ArithmeticProgressionGame
createArithmeticProgressionGame n k = if k < n
  then Just $ ArithmeticProgressionGame k (replicate n Nothing)
  else Nothing

instance Show ArithmeticProgressionGame where
  show (ArithmeticProgressionGame _ ps) = (\(is, ps) -> intercalate "," is ++ "\n" ++ intercalate "," ps) $
        unzip $ zipWith (\i p -> (pad $ show i, pad $ showP p)) [1..] ps
    where
      showP Nothing        = "_"
      showP (Just Player1) = "O"
      showP (Just Player2) = "X"
      pad x = replicate (3 - length x) ' ' ++ x

instance StrongPositionalGame ArithmeticProgressionGame Int where
  position (ArithmeticProgressionGame _ l) i = if i <= length l then Just $ l !! (i - 1) else Nothing
  positions (ArithmeticProgressionGame _ l) = l
  setPosition (ArithmeticProgressionGame k l) i p = if i <= length l
    then Just $ ArithmeticProgressionGame k (take (i - 1) l ++ Just p : drop i l)
    else Nothing

instance PositionalGame ArithmeticProgressionGame Int where
  makeMove = strongPositionalGameMakeMove
  gameOver a@(ArithmeticProgressionGame k l) = let n = length l
    in strongPositionalGameGameOver (filter (all (<= n)) $ concat [[take k [i,i+j..] | j <- [1..n-i]] | i <- [1..n]]) a

main :: IO ()
main = do
  putStrLn "1: TicTacToe"
  putStrLn "2: Arithmetic Progression Game"
  putStr "What do you want to play? "
  hFlush stdout
  choice <- read <$> getLine
  case choice of
    1 -> player emptyTicTacToe
    2 -> playAPG
    _ -> putStrLn "Invalid choice!"

playAPG :: IO ()
playAPG = do
  putStr "n: "
  hFlush stdout
  n <- read <$> getLine
  putStr "k: "
  hFlush stdout
  k <- read <$> getLine
  case createArithmeticProgressionGame n k of
    Just a -> player a
    Nothing -> putStrLn "Not valid input (n < k)"
