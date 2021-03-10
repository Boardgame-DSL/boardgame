{-# LANGUAGE FunctionalDependencies #-}

module MyLib (
    Player(..)
  , PositionalGame(..)
  , nextPlayer
  , player
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

import Data.List (find, intercalate)
import Data.Maybe (isJust, fromJust)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Monad (join, foldM)
import Control.Applicative ((<|>))

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
  setPosition :: a -> c -> Player -> Maybe a

-- | A standard implementation of 'makeMove' for a 'PositionalGame'.
--   Only allows move that "take" empty existing positions.
takeEmptyMakeMove :: PositionalGame a c => a -> Player -> c -> Maybe a
takeEmptyMakeMove a p coord = case getPosition a coord of
  Just Nothing -> setPosition a coord p
  _            -> Nothing

-- | Returns an implementation of 'gameOver' for a 'PositionalGame' when given
--   a set of winning sets. A player is victorious when they "own" one of the
--   winning sets. The game ends in a draw when all positions on the board are
--   taken.
patternMatchingGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe (Maybe Player)
patternMatchingGameOver patterns a = case find isJust $ map (join . reduceHomogeneousList . map (getPosition a)) patterns of
    Nothing -> if all isJust (positions a) then Just Nothing else Nothing
    just    -> just
  where
    -- | Returns an element of the homogeneous list, or 'Nothing'.
    reduceHomogeneousList :: (Eq a) => [Maybe a] -> Maybe a
    reduceHomogeneousList []     = Nothing
    reduceHomogeneousList (x:xs) = if all (== x) xs then x else Nothing

-- | Returns an implementation of 'gameOver' for a 'PositionalGame' when given
--   a set of winning sets. Player1 wins when they "own" one of the winning
--   sets. Player2 wins if Player1 cannot win.
makerBreakerGameOver :: (Eq c, PositionalGame a c) => [[c]] -> a -> Maybe (Maybe Player)
makerBreakerGameOver patterns a
  | player1won = Just (Just Player1)
  | player2won = Just (Just Player2)
  | boardisfull = Just (Just Player2)
  | otherwise = Nothing
  where
    player1won = any (all ((== Just Player1) . fromJust . getPosition a)) patterns
    player2won = all (any ((== Just Player2) . fromJust . getPosition a)) patterns
    boardisfull = all isJust $ positions a

-- | Plays a 'PositionalGame' in the console by taking alternating input from
--   the players. Requires that the game is an instance of 'Show' and that its
--   coordinates are instances of 'Read'.
player :: (Show a, Read c, PositionalGame a c) => a -> IO ()
player startState = start startState Player1
  where
    -- | Prints out the starting state, then plays the game.
    start t p = putStr "\ESC[2J" >> printGame t >> play t p
    -- | Prints out the game in the top left corner of the terminal
    printGame t = putStr "\ESC[s\ESC[0;0H" >> print t >> putStr "\ESC[u"
    -- | Asks for input, prints out the new state or repeats if the input/move
    --   is invalid. Repeats until the game is over.
    play t p = do
      putStr $ "Move for " ++ show p ++ ": "
      hFlush stdout
      c <- readMaybe <$> getLine
      case c >>= makeMove t p of
        Just t -> printGame t >> case gameOver t of
            Just p -> case p of
              Just p -> putStrLn $ show p ++ " won!"
              Nothing -> putStrLn "It's a draw!"
            Nothing -> play t (nextPlayer p)
        Nothing -> putStrLn "Invalid move, try again" >> play t p


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






player1WinsIf :: (a -> Bool) -> a -> Maybe (Maybe Player)
player1WinsIf pred x = if pred x
  then Just $ Just Player1
  else Nothing

player2LosesIf :: (a -> Bool) -> a -> Maybe (Maybe Player)
player2LosesIf = player1WinsIf

player2WinsIf :: (a -> Bool) -> a -> Maybe (Maybe Player)
player2WinsIf pred x = if pred x
  then Just $ Just Player2
  else Nothing

player1LosesIf :: (a -> Bool) -> a -> Maybe (Maybe Player)
player1LosesIf = player2WinsIf

drawIf :: (a -> Bool) -> (a -> Maybe (Maybe Player))
drawIf pred x = if pred x
  then Just Nothing
  else Nothing

-- combine two criteria.
(+|+) :: (a -> Maybe (Maybe Player))
      -> (a -> Maybe (Maybe Player))
      -> (a -> Maybe (Maybe Player))
crit1 +|+ crit2 = \x -> case (crit1 x, crit2 x) of
  (Just x, Just y) | x /= y -> error "conflicting result"
  (x, y) -> x <|> y

ifNotThen :: (a -> Maybe (Maybe Player))
    -> (a -> Maybe (Maybe Player))
    -> (a -> Maybe (Maybe Player))
ifNotThen crit1 crit2 x = crit1 x <|> crit2 x

infixl 8 `unless`
-- override the first criterion with the second criterion.
unless :: (a -> Maybe (Maybe Player))
       -> (a -> Maybe (Maybe Player))
       -> (a -> Maybe (Maybe Player))
unless = flip ifNotThen

-- combine several criteria.
criteria :: [a -> Maybe (Maybe Player)] -> a -> Maybe (Maybe Player)
criteria = foldl1 (+|+)

-- create a symmetric game from a game defined for only one player.
symmetric :: (a -> a) -> (a -> Maybe (Maybe Player)) -> a -> Maybe (Maybe Player)
symmetric flipState criterion = criterion +|+ (fmap (fmap nextPlayer) . criterion . flipState)






























