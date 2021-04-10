{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module ConnectFour where

import Prelude hiding (lookup)

import Data.Map (
    Map
  , lookup
  , member
  , adjust
  )

import Boardgame (
    Player(..)
  , Position(..)
  , PositionalGame(..)
  , mapPosition
  , criteria
  , isOccupied
  , nextPlayer
  , drawIf
  , symmetric
  , unless
  , player1WinsWhen
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

#ifdef WASM
import Data.Aeson (
    ToJSON(..)
  , object
  , (.=)
  )
#endif

-------------------------------------------------------------------------------
-- * Connect Four
-------------------------------------------------------------------------------

data ConnectFour = ConnectFour Int (ColoredGraph (Int, Int) Position String)

instance Show ConnectFour where
  show (ConnectFour k b) = show b

#ifdef WASM
instance ToJSON ConnectFour where
  toJSON (ConnectFour k b) = object [
      "k"     .= toJSON k
    , "board" .= toJSON b
    ]
#endif

instance PositionalGame ConnectFour (Int, Int) where
  positions   (ConnectFour k b)     = values b
  getPosition (ConnectFour k b) c   = fst <$> lookup c b
  setPosition (ConnectFour k b) c p = if member c b
    then Just $ ConnectFour k $ adjust (\(_, xs) -> (p, xs)) c b
    else Nothing
  makeMove = cfMove

  gameOver (ConnectFour k b) = criterion b
    where
      criterion =
        drawIf (all isOccupied . values) `unless` -- It's a draw if all tiles are owned.
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ mapPosition nextPlayer)
        -- Player1 wins if there are k or more pieces in a row in any direction.
        (criteria (player1WinsWhen . inARow (>=k) <$> directions) . filterValues (== Occupied Player1))

      directions = ["vertical", "horizontal", "diagonal1", "diagonal2"]

-- Restrict move for Connect Four.
-- Move is only valid if the positon is empty and the position below is occupied.
cfMove :: ConnectFour -> Player -> (Int, Int) -> Maybe ConnectFour
cfMove a p coord = case getPosition a coord of
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