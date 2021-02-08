{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad (join)
import Data.List (find, intercalate)
import Data.Map (Map, fromDistinctAscList, insert, lookup, (!))
import Data.Maybe (isJust)
import MyLib (Player(..), PositionalGame(..), nextPlayer)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

newtype TicTacToe = TicTacToe (Map (Integer, Integer) (Maybe Player))

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

instance PositionalGame TicTacToe (Integer, Integer) where
  starting = TicTacToe $
    fromDistinctAscList $
      zip
        [(x, y) | x <- [0..2], y <- [0..2]]
        (repeat Nothing)

  makeMove t@(TicTacToe b) p coord = case lookup coord b of
    Just Nothing -> Just $ TicTacToe $ insert coord (Just p) b
    _            -> Nothing

  gameOver (TicTacToe b) = join $ find isJust $ map match patterns
    where
      match p = find isJust $ map (singleElement . map (join . (`lookup` b))) (movedPattern p)

      singleElement [] = Nothing
      singleElement (x:xs) = join $ singleElement' x xs
        where
          singleElement' e [] = Just e
          singleElement' e (x:xs) = if e == x then singleElement' e xs else Nothing

      movedPattern p = [map (\(x, y) -> (x + dx, y + dy)) p | dx <- [0..2], dy <- [0..2]]
      patterns = [
          [(0, 0), (0, 1), (0, 2)]
        , [(0, 0), (1, 0), (2, 0)]
        , [(0, 0), (1, 1), (2, 2)]
        , [(2, 0), (1, 1), (0, 2)]
        ]

main :: IO ()
main = start (starting :: TicTacToe) Player1
  where
    start :: TicTacToe -> Player -> IO ()
    start t p = print t >> play t p
    play :: TicTacToe -> Player -> IO ()
    play t p = do
      putStr $ "Move for " ++ show p ++ ": "
      hFlush stdout
      c <- readMaybe <$> getLine
      case c >>= makeMove t p of
        Just t -> print t >> case gameOver t of
            Just p -> case p of
              Just p -> putStrLn $ show p ++ " won!"
              Nothing -> putStrLn "It's a draw!"
            Nothing -> play t (nextPlayer p)
        Nothing -> putStrLn "Invalid move, try again" >> play t p
