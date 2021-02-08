{-# LANGUAGE FunctionalDependencies #-}

module MyLib (
    Player(..)
  , PositionalGame(..)
  , StrongPositionalGame(..)
  , nextPlayer
  , player
  , strongPositionalGameGameOver
  , strongPositionalGameMakeMove
) where

import Control.Monad (join)
import Data.List (find, intercalate)
import Data.Maybe (isJust)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- | Represents one of the two players.
data Player = Player1 | Player2
  deriving (Show, Eq)

-- | Returns the "next" player in turn.
nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

-- | A type class for positional games where `a` is the game itself and `c` is
--   its accompanying "coordinate" type.
class PositionalGame a c | a -> c where
  -- | Takes the "current" state, a player, and a coordinate. Returns the new
  --   state if the move is valid.
  makeMove :: a -> Player -> c -> Maybe a
  -- | Takes the "current" state and checks if the game is over, in which case
  --   the victorious player is returned or nothing in case of a draw.
  --
  -- > Nothing       -- Continue the game
  -- > Just (Just p) -- Player p won
  -- > Just Nothing  -- Draw
  gameOver :: a -> Maybe (Maybe Player)

-- | A type class for strong positional games.
class StrongPositionalGame a c | a -> c where
  -- | Returns which player (or nothing) has taken the position at the given
  --   coordinate, or 'Nothing' if the given coordinate is invalid.
  position :: a -> c -> Maybe (Maybe Player)
  -- | Returns a list of all positions. Not in any particular order.
  positions :: a -> [Maybe Player]
  -- | Takes the position at the given coordinate for the given player and
  --   returns the new state, or 'Nothing' if the given coordinate is invalid.
  setPosition :: a -> c -> Player -> Maybe a

-- | An implementation of 'makeMove' for a 'StrongPositionalGame'.
strongPositionalGameMakeMove :: StrongPositionalGame a c => a -> Player -> c -> Maybe a
strongPositionalGameMakeMove a p coord = case position a coord of
  Just Nothing -> setPosition a coord p
  _            -> Nothing

-- | Returns an implementation of 'gameOver' for a 'StrongPositionalGame' when
--   given a set of winning sets.
strongPositionalGameGameOver :: (Eq c, StrongPositionalGame a c) => [[c]] -> a -> Maybe (Maybe Player)
strongPositionalGameGameOver patterns a = case find isJust $ map (join . reduceHomogeneousList . map (position a)) patterns of
    o@(Just _) -> o
    Nothing    -> if all isJust (positions a) then Just Nothing else Nothing
  where
    -- | Returns an element of the homogeneous list, or 'Nothing'.
    reduceHomogeneousList :: (Eq a) => [Maybe a] -> Maybe a
    reduceHomogeneousList []     = Nothing
    reduceHomogeneousList (x:xs) = if all (== x) xs then x else Nothing

-- | Plays a 'PositionalGame' in the console by taking alternating input from
--   the players. Requires that the game is an instance of 'Show' and that its
--   coordinates are instances of 'Read'.
player :: (Show a, Read c, PositionalGame a c) => a -> IO ()
player startState = start startState Player1
  where
    -- | Prints out the starting state, then plays the game.
    start t p = print t >> play t p
    -- | Asks for input, prints out the new state or repeats if the input/move
    --   is invalid. Repeats until the game is over.
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
