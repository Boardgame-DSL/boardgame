{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Y where

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
  , takeEmptyMakeMove
  , nextPlayer
  , symmetric
  , player1WinsWhen
  )

import Boardgame.ColoredGraph (
    ColoredGraph
  , values
  , anyConnections
  , mapValues
  , filterValues
  , filterG
  , triHexGraph
  )

-------------------------------------------------------------------------------
-- * Y
-------------------------------------------------------------------------------

newtype Y = Y (ColoredGraph (Int, Int) Position (Int, Int))

instance Show Y where
  show (Y b) = show b

instance PositionalGame Y (Int, Int) where
  positions   (Y b)     = values b
  getPosition (Y b) c   = fst <$> lookup c b
  setPosition (Y b) c p = if member c b
    then Just $ Y $ adjust (\(_, xs) -> (p, xs)) c b
    else Nothing
  makeMove = takeEmptyMakeMove

  gameOver (Y b) = criterion b
    where
      criterion =
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ mapPosition nextPlayer) $
        player1WinsWhen $ anyConnections (==3) [side1, side2, side3] . filterValues (== Occupied Player1)

      dirs :: [(Int, Int)]
      dirs =
        [ (1, 0)
        , (1, -1)
        , (0, -1)
        , (-1, 0)
        , (-1, 1)
        , (0, 1)
        ]
      emptyNeighbour x = keys $ filterG (notElem x . elems . snd) b

      side1 = emptyNeighbour $ dirs !! 0
      side2 = emptyNeighbour $ dirs !! 2
      side3 = emptyNeighbour $ dirs !! 4

emptyY :: Int -> Y
emptyY = Y . triHexGraph