{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ConnectFour where

import Data.Map (
    Map
  , lookup
  , member
  , adjust
  )

import Prelude hiding (lookup)

import Boardgame (
    Player(..)
  , Position(..)
  , Outcome(..)
  , PositionalGame(..)
  , mapPosition
  , player1WinsIf
  , criteria
  , isOccupied
  , nextPlayer
  , drawIf
  , symmetric
  , unless
  )

import Boardgame.ColoredGraph (
    ColoredGraph
  , ColoredGraphTransformer(..)
  , values
  , mapValues
  , mapEdges
  , filterValues
  , rectOctGraph
  , inARow
  , filterEdges
  )

-------------------------------------------------------------------------------
-- * Connect Four
-------------------------------------------------------------------------------

data ConnectFour = ConnectFour Int (ColoredGraph (Int, Int) Position String)

instance Show ConnectFour where
  show (ConnectFour k b) = show b

instance PositionalGame ConnectFour (Int, Int) where
  getPosition (ConnectFour k b) c = fst <$> lookup c b
  positions   (ConnectFour k b) = values b
  setPosition (ConnectFour k b) c p = if member c b
    then Just $ ConnectFour k $ adjust (\(_, xs) -> (p, xs)) c b
    else Nothing
  makeMove = newMakeMove

  gameOver (ConnectFour k b) = criterion b
    where
      criterion =
        drawIf (all isOccupied . values) `unless` -- It's a draw if all tiles are owned.
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ mapPosition nextPlayer)
        -- Player1 wins if there are k or more pieces in a row in any direction.
        (criteria (player1WinsIf . inARow (>=k) <$> directions) . filterValues (== Occupied Player1))

      directions = ["vertical", "horizontal", "diagonal1", "diagonal2"]

-- | Restrict move.
--   Move is only valid if the positon is empty and the position below is occupied.
  -- (row, col)    = Nothing
  -- (row, col-1) /= Nothing
newMakeMove :: ConnectFour -> Player -> (Int, Int) -> Maybe ConnectFour
newMakeMove a p coord = case getPosition a coord of
  -- If we are at bottom row, we can place the piece there.
  Just Empty -> if ((fst coord) == 0)
                    then setPosition a coord (Occupied p)
                    -- Not at bottom row, check to see if position below has been filled.
                    else case getPosition a ((fst coord) -1, snd coord) of
                      Just Empty -> Nothing
                      _          -> setPosition a coord (Occupied p)
  _          -> Nothing

emptyConnectFour :: Int -> Int -> Int -> ConnectFour
emptyConnectFour m n k = ConnectFour k $ mapEdges dirName $ rectOctGraph m n
  where
    dirName (1,0)   = "horizontal"
    dirName (-1,0)  = "horizontal"
    dirName (0,-1)  = "vertical"
    dirName (0,1)   = "vertical"
    dirName (1,-1)  = "diagonal1"
    dirName (-1,1)  = "diagonal1"
    dirName (1,1)   = "diagonal2"
    dirName (-1,-1) = "diagonal2"