{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Graph (Graph, buildG, path)
import Data.List (
    elemIndex
  , find
  , findIndex
  , intercalate
  )
import Data.Map (
    Map
  , elems
  , fromDistinctAscList
  , insert
  , lookup
  , member
  , (!)
  )
import Data.Maybe (fromJust, isJust)
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

-------------------------------------------------------------------------------
-- * TicTacToe
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- * Arithmetic Progression Game
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- * Shannon Switching Game
-------------------------------------------------------------------------------

newtype ShannonSwitchingGame = ShannonSwitchingGame (Int, [((Int, Int), Maybe Player)])

-- | Creates a list of all edges, input n gives n*n graph
gridEdges :: Int -> [((Int, Int), Maybe Player)]
gridEdges n =
  concat [[((j+i*n,(j+1)+i*n), Nothing), ((j+i*n,j+(i+1)*n), Nothing)] | i <- [0..n-2], j <- [0..n-2]] ++
  [(((n-1)+i*n, (n-1)+(i+1)*n), Nothing) | i <- [0..n-2]] ++
  [((i+(n-1)*n, (i+1)+(n-1)*n), Nothing) | i <- [0..n-2]]

createShannonSwitchingGame :: Int -> ShannonSwitchingGame
createShannonSwitchingGame n = ShannonSwitchingGame (n, gridEdges n)

-- o───o---o
-- ║   │   :
-- o═══o═══o
-- ║   │   :
-- o───o───o
instance Show ShannonSwitchingGame where
  show a@(ShannonSwitchingGame (n, l)) = intercalate "\n" ([concat ["o" ++ showH (fromJust $ position a (i+j*n, (i+1)+j*n)) | i <- [0 .. n - 2]]
    ++ "o\n" ++ concat [showV (fromJust $ position a (i+j*n, i+(j+1)*n)) ++ "   " | i <- [0 .. n - 2]] ++ showV (fromJust $ position a ((n-1)+j*n, (n-1)+(j+1)*n) ) |
    j <- [0 .. n - 2]]
    ++ [concat ["o" ++ showH (fromJust $ position a (i+(n-1)*n, (i+1)+(n-1)*n)) | i <- [0 .. n - 2]] ++ "o"])
    where
      showH (Just Player1) = "───"
      showH (Just Player2) = "═══"
      showH Nothing        = "---"
      showV (Just Player1) = "│"
      showV (Just Player2) = "║"
      showV Nothing        = ":"

instance StrongPositionalGame ShannonSwitchingGame (Int, Int) where
  position (ShannonSwitchingGame (_, l)) c = snd <$> find ((== c) . fst) l
  positions (ShannonSwitchingGame (_, l)) = map snd l
  setPosition (ShannonSwitchingGame (n, l)) c p = case findIndex ((== c) . fst) l of
    Just i -> Just $ ShannonSwitchingGame (n, take i l ++ (c, Just p) : drop (i + 1) l)
    Nothing -> Nothing

instance PositionalGame ShannonSwitchingGame (Int, Int) where
  makeMove = strongPositionalGameMakeMove
  gameOver (ShannonSwitchingGame (n, l))
    | path g 0 (n * n - 1) = Just (Just Player1)
    | path g (n - 1) (n * n - n) = Just (Just Player2)
    | all (isJust . snd) l = Just Nothing
    | otherwise = Nothing
    where
      g = buildG (0, n * n - 1) (map fst $ filter ((== Just Player1) . snd) l)

-------------------------------------------------------------------------------
-- * CLI interactions
-------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "1: TicTacToe"
  putStrLn "2: Arithmetic Progression Game"
  putStrLn "3: Shannon Switching Game"
  putStr "What do you want to play? "
  hFlush stdout
  choice <- read <$> getLine
  case choice of
    1 -> player emptyTicTacToe
    2 -> playAPG
    3 -> player $ createShannonSwitchingGame 5
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
