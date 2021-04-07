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
  , player1WinsIf
  , player1LosesIf
  , criteria
  , symmetric
  , unless
  )

import Boardgame.ColoredGraph (
    ColoredGraph
  , values
  , mapValues
  , anyConnections
  , filterValues
  , filterG
  , hexHexGraph
  )

-------------------------------------------------------------------------------
-- * Cross
-------------------------------------------------------------------------------

newtype Cross = Cross (ColoredGraph (Int, Int) Position (Int, Int))

instance Show Cross where
  show (Cross b) = show b

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
        (criteria (player1LosesIf <$> -- you lose if you have connected 2 opposite sides.
          [ anyConnections (==2) [side1, side4] . filterValues (== Occupied Player1)
          , anyConnections (==2) [side2, side5] . filterValues (== Occupied Player1)
          , anyConnections (==2) [side3, side6] . filterValues (== Occupied Player1)
          ]) `unless`
        criteria (player1WinsIf <$> -- you win if you have connected 3 non-adjacent sides.
          [ anyConnections (==3) [side1, side3, side5] . filterValues (== Occupied Player1)
          , anyConnections (==3) [side2, side4, side6] . filterValues (== Occupied Player1)
          ]))

      dirs =
        [ (1, 0)
        , (1, -1)
        , (0, -1)
        , (-1, 0)
        , (-1, 1)
        , (0, 1)
        ]
      emptyNeighbours xs = keys $ filterG (null . intersect xs . elems . snd) b

      side1 = emptyNeighbours [dirs !! 0, dirs !! 1]
      side2 = emptyNeighbours [dirs !! 1, dirs !! 2]
      side3 = emptyNeighbours [dirs !! 2, dirs !! 3]
      side4 = emptyNeighbours [dirs !! 3, dirs !! 4]
      side5 = emptyNeighbours [dirs !! 4, dirs !! 5]
      side6 = emptyNeighbours [dirs !! 5, dirs !! 0]

emptyCross :: Int -> Cross
emptyCross = Cross . hexHexGraph

