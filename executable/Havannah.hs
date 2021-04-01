{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Havannah where

import Prelude hiding (lookup)

import Boardgame (
    Player(..)
  , Position(..)
  , Outcome(..)
  , PositionalGame(..)
  , mapPosition
  , isOccupied
  , nextPlayer
  , drawIf
  , player1WinsIf
  , criteria
  , symmetric
  , unless
  )

import Boardgame.ColoredGraph (
    ColoredGraph
  , ColoredGraphTransformer(..)
  , values
  , anyConnections
  , mapValues
  , filterValues
  , filterG
  , components
  , hexHexGraph
  , mapEdges
  , inARow
  , coloredGraphVertexPositions
  , coloredGraphSetVertexPosition
  , coloredGraphGetVertexPosition
  )

-------------------------------------------------------------------------------
-- * Havannah
-------------------------------------------------------------------------------

newtype Havannah = Havannah (ColoredGraph (Int, Int) Position ())
  deriving (ColoredGraphTransformer (Int, Int) Position ())

instance Show Havannah where
  show (Havannah b) = show b

instance PositionalGame Havannah (Int, Int) where
  positions = coloredGraphVertexPositions
  getPosition = coloredGraphGetVertexPosition
  setPosition = coloredGraphSetVertexPosition
  gameOver (Havannah b) = criterion b
    where
      criterion =
        drawIf (all isOccupied . values) `unless` -- It's a draw if all tiles are owned.
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ mapPosition nextPlayer)
        (criteria (player1WinsIf <$> -- Player1 wins if any of these 3 criteria are satisfied.
            -- Player1 has connected 2 corners.
          [ anyConnections (>=2) corners . filterValues (== Occupied Player1)
            -- player1 has connecteed 3 edges (excluding the corners).
          , anyConnections (>=3) edges . filterValues (== Occupied Player1)
            -- player1 has surrounded other tiles such that they can't reach the border.
          , anyConnections (==0) border . filterValues (/= Occupied Player1)
          ]))
      corners = components $ filterG ((==3) . length . snd) b
      edges   = components $ filterG ((==4) . length . snd) b
      border = corners ++ edges

emptyHavannah :: Int -> Havannah
emptyHavannah = Havannah . mapEdges (const ()) . hexHexGraph

