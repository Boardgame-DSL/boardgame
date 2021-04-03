{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Boardgame (
    Player(..)
  , Position(..)
  , Outcome(..)
  , PositionalGame(..)
  , nextPlayer
  , mapPosition
  , isOccupied
  , mapOutcome
  , play
  , playerToInt
  , playIO
  , takeEmptyMakeMove
  , patternMatchingGameOver
  , drawIf
  , player1WinsIf
  , player2WinsIf
  , player1LosesIf
  , player2LosesIf
  , criteria
  , symmetric
  , unless
  , ifNotThen
  , makerBreakerGameOver
) where

import Data.Functor ((<&>))
import Data.List (find, intercalate)
import Data.Maybe (isJust, fromJust)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Monad (join, foldM)
import Control.Applicative ((<|>))
#ifdef WASM
import Data.Aeson (ToJSON(toJSON), Value(Number, Null))
import Data.Scientific (fromFloatDigits)
#endif

-- | Represents one of the two players.
data Player = Player1 | Player2
  deriving (Show, Eq)

-- | Returns the "next" player in turn.
nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

-- | Turns a 'Player' into an int. 1 or 2 for the player respectively.
playerToInt :: Player -> Int
playerToInt Player1 = 1
playerToInt Player2 = 2

#ifdef WASM
instance ToJSON Player where
  toJSON = Number . fromFloatDigits . fromIntegral . playerToInt
#endif

-- | A 'Position' can either be 'Occupied' by a 'Player' or be 'Empty'.
data Position = Occupied Player | Empty
  deriving (Eq, Show)

#ifdef WASM
instance ToJSON Position where
  toJSON (Occupied p) = toJSON p
  toJSON Empty     = Null
#endif

-- | Applies the given function to a occupying piece, or does nothing in the case
--   of an 'Empty' 'Position'.
mapPosition :: (Player -> Player) -> Position -> Position
mapPosition f (Occupied p) = Occupied $ f p
mapPosition _ Empty     = Empty

-- | Checks if the position is occupied or not.
isOccupied :: Position -> Bool
isOccupied (Occupied _) = True
isOccupied Empty     = False

-- | The 'Outcome' of a game. Either a 'Win' for one of the players, or a
--   'Draw'.
data Outcome = Win Player | Draw
  deriving (Eq, Show)

#ifdef WASM
instance ToJSON Outcome where
  toJSON (Win p) = toJSON p
  toJSON Draw    = Null
#endif

-- | Applies the given function to a winning player, or does nothing in the
--   case of a draw.
mapOutcome :: (Player -> Player) -> Outcome -> Outcome
mapOutcome f (Win p) = Win $ f p
mapOutcome _ Draw    = Draw

-- | A type class for positional games where `a` is the game itself and `c` is
--   its accompanying "coordinate" type.
class PositionalGame a c | a -> c where
  -- | Takes the "current" state, a player, and a coordinate. Returns the new
  --   state if the move is valid.
  --
  --   The default implementation is 'takeEmptyMakeMove'.
  makeMove :: a -> Player -> c -> Maybe a
  makeMove = takeEmptyMakeMove
  -- | Takes the "current" state and checks if the game is over, in which case
  --   the victorious player is returned or nothing in case of a draw.
  --
  -- > Nothing       -- Continue the game
  -- > Just (Just p) -- Player p won
  -- > Just Nothing  -- Draw
  gameOver :: a -> Maybe Outcome
  -- | Returns a list of all positions. Not in any particular order.
  positions :: a -> [Position]
  -- | Returns which player (or nothing) has taken the position at the given
  --   coordinate, or 'Nothing' if the given coordinate is invalid.
  --
  -- > Nothing       -- Invalid position
  -- > Just (Just p) -- Player p owns this position
  -- > Just Nothing  -- This position is empty
  getPosition :: a -> c -> Maybe Position
  -- | Takes the position at the given coordinate for the given player and
  --   returns the new state, or 'Nothing' if the given coordinate is invalid.
  setPosition :: a -> c -> Position -> Maybe a

-- | A standard implementation of 'makeMove' for a 'PositionalGame'.
--   Only allows move that "take" empty existing positions.
takeEmptyMakeMove :: PositionalGame a c => a -> Player -> c -> Maybe a
takeEmptyMakeMove a p coord = case getPosition a coord of
  Just Empty -> setPosition a coord (Occupied p)
  _          -> Nothing

-- | Returns an implementation of 'gameOver' for a 'PositionalGame' when given
--   a set of winning sets. A player is victorious when they "own" one of the
--   winning sets. The game ends in a draw when all positions on the board are
--   taken.
patternMatchingGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe Outcome
patternMatchingGameOver patterns a = case find isOccupied $ map (reduceHomogeneousList . map (fromJust . getPosition a)) patterns of
    Nothing -> if all isOccupied (positions a) then Just Draw else Nothing
    Just (Occupied winner) -> Just $ Win winner
    Just Empty          -> Just Draw
  where
    -- | Returns an element of the homogeneous list, or 'Nothing'.
    reduceHomogeneousList :: [Position] -> Position
    reduceHomogeneousList []     = Empty
    reduceHomogeneousList (x:xs) = if all (== x) xs then x else Empty

-- | Returns an implementation of 'gameOver' for a 'PositionalGame' when given
--   a set of winning sets. Player1 wins when they "own" one of the winning
--   sets. Player2 wins if Player1 cannot win.
makerBreakerGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe Outcome
makerBreakerGameOver patterns a
  | player1won = Just $ Win Player1
  | player2won = Just $ Win Player2
  | otherwise = Nothing
  where
    player1won = any (all $ (== Occupied Player1) . fromJust . getPosition a) patterns
    player2won = all (any $ (== Occupied Player2) . fromJust . getPosition a) patterns

-- | The skeleton code for "playing" any 'PositionalGame'. When given a set of
--   function for communicating the state of the game and moves, a starting
--   state can be applied to play the game.
play :: (Monad m, PositionalGame a c) =>
  (a -> m ())
  -- ^ Function for outputting the state of the game.
  -> (Player -> m ())
  -- ^ Function for communicating which 'Player's turn it is.
  -> m c
  -- ^ Function for getting a move from a player.
  -> m ()
  -- ^ Function for communicating an invalid move.
  -> (Outcome -> m ())
  -- ^ Function for outputting the end result of the game.
  -> a
  -> m ()
play putState putTurn getMove putInvalidMove putGameOver startingState = putState startingState >> putTurn Player1 >> play' startingState Player1
  where
    play' s p = getMove <&> makeMove s p >>= \case
      Just s' -> putState s' >> case gameOver s' of
        Just v  -> putGameOver v
        Nothing -> (\p' -> putTurn p' >> play' s' p') $ nextPlayer p
      Nothing -> putInvalidMove >> play' s p

-- | Plays a 'PositionalGame' in the console by taking alternating input from
--   the players. Requires that the game is an instance of 'Show' and that its
--   coordinates are instances of 'Read'.
playIO :: (Show a, Read c, PositionalGame a c) => a -> IO ()
playIO = play putState putTurn getMove putInvalidMove putGameOver
  where
    putState s = putStr "\ESC[s\ESC[0;0H" >> print s >> putStr "\ESC[u" >> hFlush stdout
    putTurn p = putStr ("Move for " ++ (case p of
      Player1 -> "player 1"
      Player2 -> "player 2") ++ ": ") >> hFlush stdout
    getMove = getLine <&> readMaybe >>= \case
      Just c  -> return c
      Nothing -> putStr "Invalid input, try again: " >> hFlush stdout >> getMove
    putInvalidMove = putStr "Invalid move, try again: " >> hFlush stdout
    putGameOver = \case
      Win Player1 -> putStrLn "Player 1 won!" >> hFlush stdout
      Win Player2 -> putStrLn "Player 2 won!" >> hFlush stdout
      Draw        -> putStrLn "It's a draw!" >> hFlush stdout


data BiasedPositionalGame a c = BiasedPositionalGame Int Int a

instance PositionalGame a c => PositionalGame (BiasedPositionalGame a c) [c] where
  makeMove (BiasedPositionalGame p q x) player index = BiasedPositionalGame p q <$> foldM (\z w -> makeMove z player w) x index
  gameOver (BiasedPositionalGame p q x) = gameOver x
  positions (BiasedPositionalGame p q x) = positions x
  getPosition (BiasedPositionalGame p q x) = undefined
  setPosition (BiasedPositionalGame p q x) = undefined


data CombinedPositionalGames a b i j = CombinedPositionalGames a b

instance (PositionalGame a i, PositionalGame b j) => PositionalGame (CombinedPositionalGames a b i j) (Either i j) where
  makeMove (CombinedPositionalGames x y) player index = case index of
    Left i -> flip CombinedPositionalGames y <$> makeMove x player i
    Right i -> CombinedPositionalGames x <$> makeMove y player i
  gameOver (CombinedPositionalGames x y) = gameOver x <|> gameOver y
  positions (CombinedPositionalGames x y) = positions x ++ positions y
  getPosition (CombinedPositionalGames x y) = undefined
  setPosition (CombinedPositionalGames x y) = undefined






-- | If the predicate holds, a winning state for player 1 is returned. If
--   not, a "game running" state is returned.
player1WinsIf :: (a -> Bool) -> a -> Maybe Outcome
player1WinsIf pred x = if pred x
  then Just $ Win Player1
  else Nothing

-- | A synonym for 'player1WinsIf'. When player 2 loses, player 1 wins.
player2LosesIf :: (a -> Bool) -> a -> Maybe Outcome
player2LosesIf = player1WinsIf

-- | If the predicate holds, a winning state for player 2 is returned. If
--   not, a "game running" state is returned.
player2WinsIf :: (a -> Bool) -> a -> Maybe Outcome
player2WinsIf pred x = if pred x
  then Just $ Win Player2
  else Nothing

-- | A synonym for 'player2WinsIf'. When player 1 loses, player 2 wins.
player1LosesIf :: (a -> Bool) -> a -> Maybe Outcome
player1LosesIf = player2WinsIf

-- | If the predicate holds, a draw state is returned. If not, a "game running"
--   state is returned.
drawIf :: (a -> Bool) -> (a -> Maybe Outcome)
drawIf pred x = if pred x
  then Just Draw
  else Nothing

-- | Combines two criteria into one where if the first criterion does not
--   return a game over state, the result of the second criterion is used.
ifNotThen :: (a -> Maybe Outcome)
    -> (a -> Maybe Outcome)
    -> (a -> Maybe Outcome)
ifNotThen crit1 crit2 x = crit1 x <|> crit2 x

infixl 8 `unless`
-- | Combines two criteria into one where the first criterions result is
--   returned, unless the second criterion returns a game over state.
unless :: (a -> Maybe Outcome)
       -> (a -> Maybe Outcome)
       -> (a -> Maybe Outcome)
unless = flip ifNotThen

-- | Combines several criteria into one. If two or more of the criteria returns
--   different game over states, an error is raised.
criteria :: [a -> Maybe Outcome] -> a -> Maybe Outcome
criteria = foldl1 ifNotThen

-- | Create a symmetric game from a game defined for only one player.
symmetric :: (a -> a) -> (a -> Maybe Outcome) -> a -> Maybe Outcome
symmetric flipState criterion = criterion `ifNotThen` (fmap (mapOutcome nextPlayer) . criterion . flipState)






























