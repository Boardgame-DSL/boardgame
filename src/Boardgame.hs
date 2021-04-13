{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-|
Module:      Boardgame
Description: The main framework for creating boardgames.

The main framework module for boardgames. Contains the 'PositionalGame' class
implemented by all positional games, and a bunch of helper functions.

The helper functions range from just that, simple helper functions such as
'player1WinsWhen', to right out implementations of functions in the
'PositionalGame's class, such as the 'takeEmptyMakeMove' functions.

It also contains some functions for playing games. 'play' is the implementation
agnostic skeleton code that you can use in any context. And 'playIO' uses
'play' to play the games in the terminal.

= TicTacToe as an example

> -- TicTacToe is a
> newtype TicTacToe = TicTacToe (Map (Integer, Integer) Position)
>
> -- Creates an empty TicTacToe board with coordinates @(0..2, 0..2)@
> emptyTicTacToe = TicTacToe $
>   fromDistinctAscList $
>     zip
>       [(x, y) | x <- [0..2], y <- [0..2]]
>       (repeat Empty)
>
> instance Show TicTacToe where
>   show (TicTacToe b) = intercalate "\n" [
>       "╔═══╤═══╤═══╗"
>     , "║ " ++ intercalate " │ " (row 0) ++ " ║"
>     , "╟───┼───┼───╢"
>     , "║ " ++ intercalate " │ " (row 1) ++ " ║"
>     , "╟───┼───┼───╢"
>     , "║ " ++ intercalate " │ " (row 2) ++ " ║"
>     , "╚═══╧═══╧═══╝"
>     ]
>     where
>       row y = map (\x -> showP $ b ! (x, y)) [0..2]
>       showP (Occupied Player1) = "\ESC[34mo\ESC[0m"
>       showP (Occupied Player2) = "\ESC[31mx\ESC[0m"
>       showP Empty = " "
>
> instance PositionalGame TicTacToe (Integer, Integer) where
>   -- Just looks up the coordinate in the underlying Map
>   getPosition (TicTacToe b) = flip lookup b
>   -- Just returns the elements in the underlying Map
>   positions (TicTacToe b) = elems b
>   -- If the underlying Map has the given coordinate, update it with the given player
>   setPosition (TicTacToe b) c p = if member c b then Just $ TicTacToe $ insert c p b else Nothing
>   -- "Creates" a 'gameOver' function by supplying all the winning "patterns"
>   gameOver = patternMatchingGameOver [
>       [(0, 0), (0, 1), (0, 2)]
>     , [(1, 0), (1, 1), (1, 2)]
>     , [(2, 0), (2, 1), (2, 2)]
>     , [(0, 0), (1, 0), (2, 0)]
>     , [(0, 1), (1, 1), (2, 1)]
>     , [(0, 2), (1, 2), (2, 2)]
>     , [(0, 0), (1, 1), (2, 2)]
>     , [(2, 0), (1, 1), (0, 2)]
>     ]
>   -- 'makeMove' is handled by the default implementation 'takeEmptyMakeMove'
>
> -- Plays the game in the terminal, takes @(x, y)@ as input
> main = playIO emptyTicTacToe
-}
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
  , drawWhen
  , player1WinsWhen
  , player2WinsWhen
  , player1LosesWhen
  , player2LosesWhen
  , criteria
  , symmetric
  , unless
  , ifNotThen
  , makerBreakerGameOver
) where

import Data.Functor ((<&>))
import Data.List (find, intercalate, minimumBy, intersect)
import Data.Maybe (isJust, fromJust)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Monad (join, foldM)
import Control.Applicative ((<|>))
import Data.Bifunctor (first, Bifunctor (second))
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
  --   the victorious player is returned or 'Draw' in case of a draw.
  --
  -- > Nothing       -- Continue the game
  -- > Just (Just p, cs) -- Player p won
  -- > Just (Nothing, cs)  -- Draw
  --
  -- We also return `cs`, a list of coordinates to highlight.
  gameOver :: a -> Maybe (Outcome, [c])
  -- | Returns a list of all positions. Not in any particular order.
  positions :: a -> [Position]
  -- | Returns which player (or Empty) has taken the position at the given
  --   coordinate, or 'Nothing' if the given coordinate is invalid.
  --
  -- > Nothing         -- Invalid position
  -- > Occupied Player -- Player p owns this position
  -- > Empty           -- This position is empty
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
patternMatchingGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe (Outcome, [c])
patternMatchingGameOver patterns a = case find (isOccupied . fst) $ (\pat -> (, pat) $ reduceHomogeneousList (fromJust . getPosition a <$> pat)) <$> patterns of
    Nothing -> if all isOccupied (positions a) then Just (Draw, []) else Nothing
    Just (Occupied winner, coords) -> Just (Win winner, coords)
    Just (Empty, coords)           -> Just (Draw, coords)
  where
    -- | Returns an element of the homogeneous list, or 'Empty'.
    reduceHomogeneousList :: [Position] -> Position
    reduceHomogeneousList []     = Empty
    reduceHomogeneousList (x:xs) = if all (== x) xs then x else Empty

-- | Returns an implementation of 'gameOver' for a 'PositionalGame' when given
--   a set of winning sets. Player1 wins when they "own" one of the winning
--   sets. Player2 wins if Player1 cannot win.
makerBreakerGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe (Outcome, [c])
makerBreakerGameOver patterns a
  | Just coords <- player1won = Just (Win Player1, coords)
  | player2won = Just (Win Player2, player2Coords)
  | otherwise = Nothing
  where
    player1won = find (all $ (== Occupied Player1) . fromJust . getPosition a) patterns
    player2won = all (any $ (== Occupied Player2) . fromJust . getPosition a) patterns

    -- A minimum set of coordinates which Player2 owns and contain atleast one element in every winning set.
    -- This is only valid when `player2won` is `True`.
    player2Coords = minimumBy compareLength $ assignments $ filter ((== Occupied Player2) . fromJust . getPosition a) <$> patterns

    -- A lazy version of `comparing length`.
    compareLength              :: [a] -> [b] -> Ordering
    compareLength []     []     = EQ
    compareLength (_:_)  []     = GT
    compareLength []     (_:_)  = LT
    compareLength (_:xs) (_:ys) = compareLength xs ys

    -- Return all sets which contain atleast one element from every set in the input
    -- and avoiding unneccesary elements.
    -- This is used to solve the hitting set/set cover problem.
    assignments :: Eq c => [[c]] -> [[c]]
    assignments = assignments' []
      where
        assignments' set [] = [set]
        assignments' set (claus:clauses) = if not $ null $ intersect set claus
          then assignments' set clauses
          else concat $ (\c -> assignments' (c:set) clauses) <$> claus

-- | Returns an implementation of 'gameOver' for a 'PositionalGame' when given
--   a set of winning sets. Player1 wins if they can avoid "owning" any of the
--   winning sets. Player2 wins if Player1 owns a winning set.
avoiderEnforcerGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe (Outcome, [c])
avoiderEnforcerGameOver patterns a = first (mapOutcome nextPlayer) <$> makerBreakerGameOver patterns a

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
  -> ((Outcome, [c]) -> m ())
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
      (Win Player1, _) -> putStrLn "Player 1 won!" >> hFlush stdout
      (Win Player2, _) -> putStrLn "Player 2 won!" >> hFlush stdout
      (Draw, _)      -> putStrLn "It's a draw!" >> hFlush stdout

data CombinedPositionalGames a b i j = CombinedPositionalGames a b

instance (PositionalGame a i, PositionalGame b j) => PositionalGame (CombinedPositionalGames a b i j) (Either i j) where
  makeMove (CombinedPositionalGames x y) player index = case index of
    Left i -> flip CombinedPositionalGames y <$> makeMove x player i
    Right i -> CombinedPositionalGames x <$> makeMove y player i
  gameOver (CombinedPositionalGames x y) = (second (fmap Left)  <$> gameOver x)
                                       <|> (second (fmap Right) <$> gameOver y)
  positions (CombinedPositionalGames x y) = positions x ++ positions y
  getPosition (CombinedPositionalGames x y) = either (getPosition x) (getPosition y)
  setPosition (CombinedPositionalGames x y) ij p = case ij of
    Left i -> flip CombinedPositionalGames y <$> setPosition x i p
    Right j -> CombinedPositionalGames x <$> setPosition y j p





-- | If the predicate holds, a winning state for player 1 is returned. If
--   not, a "game running" state is returned.
player1WinsIf :: (a -> Bool) -> a -> Maybe (Outcome, [c])
player1WinsIf pred x = if pred x
  then Just (Win Player1, [])
  else Nothing

-- | A synonym for 'player1WinsIf'. When player 2 loses, player 1 wins.
player2LosesIf :: (a -> Bool) -> a -> Maybe (Outcome, [c])
player2LosesIf = player1WinsIf

-- | If the predicate holds, a winning state for player 2 is returned. If
--   not, a "game running" state is returned.
player2WinsIf :: (a -> Bool) -> a -> Maybe (Outcome, [c])
player2WinsIf pred x = if pred x
  then Just (Win Player2, [])
  else Nothing

-- | A synonym for 'player2WinsIf'. When player 1 loses, player 2 wins.
player1LosesIf :: (a -> Bool) -> a -> Maybe (Outcome, [c])
player1LosesIf = player2WinsIf

-- | If the predicate holds, a draw state is returned. If not, a "game running"
--   state is returned.
drawIf :: (a -> Bool) -> (a -> Maybe (Outcome, [c]))
drawIf pred x = if pred x
  then Just (Draw, [])
  else Nothing

-- | If the predicate holds, a winning state for player 1 is returned. If
--   not, a "game running" state is returned.
player1WinsWhen :: (a -> Maybe [c]) -> a -> Maybe (Outcome, [c])
player1WinsWhen pred x = (Win Player1, ) <$> pred x

-- | A synonym for 'player1WinsIf'. When player 2 loses, player 1 wins.
player2LosesWhen :: (a -> Maybe [c]) -> a -> Maybe (Outcome, [c])
player2LosesWhen = player1WinsWhen

-- | If the predicate holds, a winning state for player 2 is returned. If
--   not, a "game running" state is returned.
player2WinsWhen :: (a -> Maybe [c]) -> a -> Maybe (Outcome, [c])
player2WinsWhen pred x = (Win Player2, ) <$>  pred x

-- | A synonym for 'player2WinsIf'. When player 1 loses, player 2 wins.
player1LosesWhen :: (a -> Maybe [c]) -> a -> Maybe (Outcome, [c])
player1LosesWhen = player2WinsWhen

-- | If the predicate holds, a draw state is returned. If not, a "game running"
--   state is returned.
drawWhen :: (a -> Maybe [c]) -> (a -> Maybe (Outcome, [c]))
drawWhen pred x = (Draw, ) <$> pred x

-- | Combines two criteria into one where if the first criterion does not
--   return a game over state, the result of the second criterion is used.
ifNotThen :: (a -> Maybe (Outcome, [c]))
    -> (a -> Maybe (Outcome, [c]))
    -> (a -> Maybe (Outcome, [c]))
ifNotThen crit1 crit2 x = crit1 x <|> crit2 x

infixl 8 `unless`
-- | Combines two criteria into one where the first criterions result is
--   returned, unless the second criterion returns a game over state.
unless :: (a -> Maybe (Outcome, [c]))
       -> (a -> Maybe (Outcome, [c]))
       -> (a -> Maybe (Outcome, [c]))
unless = flip ifNotThen

-- | Combines several criteria into one. If two or more of the criteria returns
--   different game over states, an error is raised.
criteria :: [a -> Maybe (Outcome, [c])] -> a -> Maybe (Outcome, [c])
criteria = foldl1 ifNotThen

-- | Create a symmetric game from a game defined for only one player.
symmetric :: (a -> a) -> (a -> Maybe (Outcome, [c])) -> a -> Maybe (Outcome, [c])
symmetric flipState criterion = criterion `ifNotThen` (fmap (first $ mapOutcome nextPlayer) . criterion . flipState)






























