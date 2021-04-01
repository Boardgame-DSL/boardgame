{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Boardgame (
    Player(..)
  , PositionalGame(..)
  , nextPlayer
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
import Data.Aeson (ToJSON(toJSON), Value(Number))
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
  gameOver :: a -> Maybe (Maybe Player, [c])
  -- | Returns a list of all positions. Not in any particular order.
  positions :: a -> [Maybe Player]
  -- | Returns which player (or nothing) has taken the position at the given
  --   coordinate, or 'Nothing' if the given coordinate is invalid.
  --
  -- > Nothing       -- Invalid position
  -- > Just (Just p) -- Player p owns this position
  -- > Just Nothing  -- This position is empty
  getPosition :: a -> c -> Maybe (Maybe Player)
  -- | Takes the position at the given coordinate for the given player and
  --   returns the new state, or 'Nothing' if the given coordinate is invalid.
  setPosition :: a -> c -> Maybe Player -> Maybe a

-- | A standard implementation of 'makeMove' for a 'PositionalGame'.
--   Only allows move that "take" empty existing positions.
takeEmptyMakeMove :: PositionalGame a c => a -> Player -> c -> Maybe a
takeEmptyMakeMove a p coord = case getPosition a coord of
  Just Nothing -> setPosition a coord (Just p)
  _            -> Nothing

-- | Returns an implementation of 'gameOver' for a 'PositionalGame' when given
--   a set of winning sets. A player is victorious when they "own" one of the
--   winning sets. The game ends in a draw when all positions on the board are
--   taken.
patternMatchingGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe (Maybe Player, [c])
patternMatchingGameOver patterns a = case find (isJust . fst) $ fmap (\pat -> (, pat) $ join (reduceHomogeneousList $ map (getPosition a) pat)) patterns of
    Nothing -> if all isJust (positions a) then Just (Nothing, []) else Nothing
    just    -> just
  where
    -- | Returns an element of the homogeneous list, or 'Nothing'.
    reduceHomogeneousList :: (Eq a) => [Maybe a] -> Maybe a
    reduceHomogeneousList []     = Nothing
    reduceHomogeneousList (x:xs) = if all (== x) xs then x else Nothing

-- | Returns an implementation of 'gameOver' for a 'PositionalGame' when given
--   a set of winning sets. Player1 wins when they "own" one of the winning
--   sets. Player2 wins if Player1 cannot win.
makerBreakerGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe (Maybe Player, [c])
makerBreakerGameOver patterns a
  | Just coords <- player1won = Just (Just Player1, coords)
  | player2won = Just (Just Player2, player2Coords)
  | otherwise = Nothing
  where
    player1won = find (all $ (== Just Player1) . fromJust . getPosition a) patterns
    player2won = all (any $ (== Just Player2) . fromJust . getPosition a) patterns

    -- A minimum set of coordinates which Player2 owns and contain atleast one element in every winning set.
    -- This is only valid when `player2won` is `True`.
    player2Coords = minimumBy compareLength $ assignments $ filter ((== Just Player2) . fromJust . getPosition a) <$> patterns

    -- A lazy version of `comparing length`.
    compareLength              :: [a] -> [b] -> Ordering
    compareLength []     []     = EQ
    compareLength (_:_)  []     = GT
    compareLength []     (_:_)  = LT
    compareLength (_:xs) (_:ys) = compareLength xs ys

    -- Return all small sets which contain atleast one element from every set in the input.
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
avoiderEnforcerGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe (Maybe Player, [c])
avoiderEnforcerGameOver patterns a = first (fmap nextPlayer) <$> makerBreakerGameOver patterns a

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
  -> ((Maybe Player, [c]) -> m ())
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
      (Just Player1, _) -> putStrLn "Player 1 won!" >> hFlush stdout
      (Just Player2, _) -> putStrLn "Player 2 won!" >> hFlush stdout
      (Nothing, _)      -> putStrLn "It's a draw!" >> hFlush stdout

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
player1WinsIf :: (a -> Bool) -> a -> Maybe (Maybe Player, [c])
player1WinsIf pred x = if pred x
  then Just (Just Player1, [])
  else Nothing

-- | A synonym for 'player1WinsIf'. When player 2 loses, player 1 wins.
player2LosesIf :: (a -> Bool) -> a -> Maybe (Maybe Player, [c])
player2LosesIf = player1WinsIf

-- | If the predicate holds, a winning state for player 2 is returned. If
--   not, a "game running" state is returned.
player2WinsIf :: (a -> Bool) -> a -> Maybe (Maybe Player, [c])
player2WinsIf pred x = if pred x
  then Just (Just Player2, [])
  else Nothing

-- | A synonym for 'player2WinsIf'. When player 1 loses, player 2 wins.
player1LosesIf :: (a -> Bool) -> a -> Maybe (Maybe Player, [c])
player1LosesIf = player2WinsIf

-- | If the predicate holds, a draw state is returned. If not, a "game running"
--   state is returned.
drawIf :: (a -> Bool) -> (a -> Maybe (Maybe Player, [c]))
drawIf pred x = if pred x
  then Just (Nothing, [])
  else Nothing

-- | If the predicate holds, a winning state for player 1 is returned. If
--   not, a "game running" state is returned.
player1WinsWhen :: (a -> Maybe [c]) -> a -> Maybe (Maybe Player, [c])
player1WinsWhen pred x = (Just Player1, ) <$> pred x

-- | A synonym for 'player1WinsIf'. When player 2 loses, player 1 wins.
player2LosesWhen :: (a -> Maybe [c]) -> a -> Maybe (Maybe Player, [c])
player2LosesWhen = player1WinsWhen

-- | If the predicate holds, a winning state for player 2 is returned. If
--   not, a "game running" state is returned.
player2WinsWhen :: (a -> Maybe [c]) -> a -> Maybe (Maybe Player, [c])
player2WinsWhen pred x = (Just Player2, ) <$>  pred x

-- | A synonym for 'player2WinsIf'. When player 1 loses, player 2 wins.
player1LosesWhen :: (a -> Maybe [c]) -> a -> Maybe (Maybe Player, [c])
player1LosesWhen = player2WinsWhen

-- | If the predicate holds, a draw state is returned. If not, a "game running"
--   state is returned.
drawWhen :: (a -> Maybe [c]) -> (a -> Maybe (Maybe Player, [c]))
drawWhen pred x = (Nothing, ) <$> pred x

-- | Combines two criteria into one. If one criterion returns a game over
--   state and the other a game running state, the game over state is returned.
--   If the criteria returns different game over states, different winners or
--   one draw and one winner, an error is raised.
(+|+) :: (a -> Maybe (Maybe Player, [c]))
      -> (a -> Maybe (Maybe Player, [c]))
      -> (a -> Maybe (Maybe Player, [c]))
crit1 +|+ crit2 = \x -> case (crit1 x, crit2 x) of
  (Just x, Just y) | fst x /= fst y -> error "conflicting result"
  (x, y) -> x <|> y

-- | Combines two criteria into one where if the first criterion does not
--   return a game over state, the result of the second criterion is used.
ifNotThen :: (a -> Maybe (Maybe Player, [c]))
    -> (a -> Maybe (Maybe Player, [c]))
    -> (a -> Maybe (Maybe Player, [c]))
ifNotThen crit1 crit2 x = crit1 x <|> crit2 x

infixl 8 `unless`
-- | Combines two criteria into one where the first criterions result is
--   returned, unless the second criterion returns a game over state.
unless :: (a -> Maybe (Maybe Player, [c]))
       -> (a -> Maybe (Maybe Player, [c]))
       -> (a -> Maybe (Maybe Player, [c]))
unless = flip ifNotThen

-- | Combines several criteria into one. If two or more of the criteria returns
--   different game over states, an error is raised.
criteria :: [a -> Maybe (Maybe Player, [c])] -> a -> Maybe (Maybe Player, [c])
criteria = foldl1 (+|+)

-- | Create a symmetric game from a game defined for only one player.
symmetric :: (a -> a) -> (a -> Maybe (Maybe Player, [c])) -> a -> Maybe (Maybe Player, [c])
symmetric flipState criterion = criterion +|+ (fmap (first $ fmap nextPlayer) . criterion . flipState)






























