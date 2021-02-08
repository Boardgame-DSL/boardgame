{-# LANGUAGE FunctionalDependencies #-}

module MyLib (
    Player(..)
  , nextPlayer
  , PositionalGame(..)
) where

data Player = Player1 | Player2
  deriving (Show, Eq)

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

class PositionalGame a c | a -> c where
  starting :: a
  makeMove :: a -> Player -> c -> Maybe a
  gameOver :: a -> Maybe (Maybe Player)
