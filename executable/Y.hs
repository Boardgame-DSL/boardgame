{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Y where

import Boardgame
  ( Player (..),
    Position (..),
    PositionalGame (..),
    mapPosition,
    nextPlayer,
    player1WinsWhen,
    symmetric,
    takeEmptyMakeMove,
  )
import Boardgame.ColoredGraph
  ( ColoredGraph,
    anyConnections,
    filterG,
    filterValues,
    hexDirections,
    mapValues,
    missingDirections,
    triHexGraph,
    values,
  )
import Data.Map
  ( Map,
    adjust,
    elems,
    keys,
    lookup,
    member,
  )
import Prelude hiding (lookup)

#ifdef WASM
import Data.Aeson (ToJSON(..))
#endif

-------------------------------------------------------------------------------

-- * Y

-------------------------------------------------------------------------------

newtype Y = Y (ColoredGraph (Int, Int) Position (Int, Int))

instance Show Y where
  show (Y b) = show b

#ifdef WASM
instance ToJSON Y where
  toJSON (Y b) = toJSON b
#endif

instance PositionalGame Y (Int, Int) where
  positions (Y b) = values b
  getPosition (Y b) c = fst <$> lookup c b
  setPosition (Y b) c p =
    if member c b
      then Just $ Y $ adjust (\(_, xs) -> (p, xs)) c b
      else Nothing
  makeMove = takeEmptyMakeMove

  gameOver (Y b) = criterion b
    where
      criterion =
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ mapPosition nextPlayer) $
          player1WinsWhen $ anyConnections (== 3) sides . filterValues (== Occupied Player1)

      -- A list of coordinates for every side based on which neighboring tiles are empty.
      sides = missingDirections b . pure . (hexDirections !!) . (*2) <$> [0 .. 2]

emptyY :: Int -> Y
emptyY = Y . triHexGraph