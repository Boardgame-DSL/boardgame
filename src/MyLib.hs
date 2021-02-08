{-# LANGUAGE FunctionalDependencies #-}

module MyLib (
    Player(..)
  , nextPlayer
  , PositionalGame(..)
  , StrongPositionalGame(..)
  , strongPositionalGameMakeMove
  , strongPositionalGameGameOver
) where

import Control.Monad (join)
import Data.List (find, intercalate)
import Data.Maybe (isJust)
import Debug.Trace (trace)

data Player = Player1 | Player2
  deriving (Show, Eq)

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

class PositionalGame a c | a -> c where
  starting :: a
  makeMove :: a -> Player -> c -> Maybe a
  gameOver :: a -> Maybe (Maybe Player)

class StrongPositionalGame a c | a -> c where
  position :: a -> c -> Maybe (Maybe Player)
  positions :: a -> [Maybe Player]
  setPosition :: a -> c -> Player -> Maybe a

strongPositionalGameMakeMove :: StrongPositionalGame a c => a -> Player -> c -> Maybe a
strongPositionalGameMakeMove a p coord = case position a coord of
  Just Nothing -> setPosition a coord p
  _            -> Nothing

strongPositionalGameGameOver :: (Eq c, StrongPositionalGame a c) => [[c]] -> a -> Maybe (Maybe Player)
strongPositionalGameGameOver patterns a = case find isJust $ map (join . reduceHomogeneousList . map (position a)) patterns of
    o@(Just _) -> o
    Nothing    -> if all isJust (positions a) then Just Nothing else Nothing
  where
    reduceHomogeneousList :: (Eq a) => [Maybe a] -> Maybe a
    reduceHomogeneousList []     = Nothing
    reduceHomogeneousList (x:xs) = if all (== x) xs then x else Nothing
