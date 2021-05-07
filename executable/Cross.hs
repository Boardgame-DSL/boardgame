{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cross where

import Data.List (intersect)
import Prelude hiding (lookup)  

import Data.Map (
    Map
  , elems
  , keys
  , lookup
  , member
  , adjust
  )

import Boardgame (
    Player(..)
  , Position(..)
  , PositionalGame(..)
  , mapPosition
  , isOccupied
  , takeEmptyMakeMove
  , nextPlayer
  , drawIf
  , criteria
  , symmetric
  , unless
  , player1LosesWhen
  , player1WinsWhen
  )

import Boardgame.ColoredGraph (
    ColoredGraph
  , values
  , mapValues
  , anyConnections
  , filterValues
  , filterG
  , hexHexGraph
  , missingDirections
  , hexDirections
  )

#ifdef WASM
import Data.Aeson (ToJSON(..))
#endif

-------------------------------------------------------------------------------
-- * Cross
-------------------------------------------------------------------------------

newtype Cross = Cross (ColoredGraph (Int, Int) Position (Int, Int))

instance Show Cross where
  show (Cross b) = show b

#ifdef WASM
instance ToJSON Cross where
  toJSON (Cross b) = toJSON b
#endif

instance PositionalGame Cross (Int, Int) where
  positions   (Cross b)     = values b
  getPosition (Cross b) c   = fst <$> lookup c b
  setPosition (Cross b) c p = if member c b
    then Just $ Cross $ adjust (\(_, xs) -> (p, xs)) c b
    else Nothing
  makeMove = takeEmptyMakeMove

  gameOver (Cross b) = criterion b
    where
      criterion =
        drawIf (all isOccupied . values) `unless` -- It's a draw if all tiles are owned.
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ mapPosition nextPlayer)
        (criteria (player1LosesWhen <$> -- you lose if you have connected 2 opposite sides.
          [ anyConnections (==2) [side1, side4] . filterValues (== Occupied Player1)
          , anyConnections (==2) [side2, side5] . filterValues (== Occupied Player1)
          , anyConnections (==2) [side3, side6] . filterValues (== Occupied Player1)
          ]) `unless`
        criteria (player1WinsWhen <$> -- you win if you have connected 3 non-adjacent sides.
          [ anyConnections (==3) [side1, side3, side5] . filterValues (== Occupied Player1)
          , anyConnections (==3) [side2, side4, side6] . filterValues (== Occupied Player1)
          ]))

      -- A list of coordinates for every side based on which neighboring tiles are empty.
      [side1, side2, side3, side4, side5, side6] =
        missingDirections b <$> [[hexDirections !! i, hexDirections !! ((i+1) `mod` 6)] | i <- [0..5]]

emptyCross :: Int -> Cross
emptyCross = Cross . hexHexGraph