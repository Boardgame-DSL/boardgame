{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Yavalath where

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
  , player1LosesIf
  , unless
  )

import Boardgame.ColoredGraph (
    ColoredGraph
  , ColoredGraphTransformer(..)
  , values
  , mapValues
  , filterValues
  , hexHexGraph
  , inARow
  , mapEdges
  , winningSetPaths
  , coloredGraphVertexPositions
  , coloredGraphGetVertexPosition
  , coloredGraphSetVertexPosition
  )

-------------------------------------------------------------------------------
-- * Yavalath
-------------------------------------------------------------------------------

newtype Yavalath = Yavalath (ColoredGraph (Int, Int) Position String)
  deriving (ColoredGraphTransformer (Int, Int) Position String)

instance Show Yavalath where
  show (Yavalath b) = show b

instance PositionalGame Yavalath (Int, Int) where
  positions = coloredGraphVertexPositions
  getPosition = coloredGraphGetVertexPosition
  setPosition = coloredGraphSetVertexPosition
  gameOver (Yavalath b) = criterion b
    where
      criterion =
        drawIf (all isOccupied . values) `unless` -- It's a draw if all tiles are owned.
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ mapPosition nextPlayer)
        -- Player1 looses if he has 3 in a row but wins if he has 4 or more in a row.
        -- It's important we use `unless` here because otherwise we could have conflicting
        -- outcomes from having both 3 in a row and 4 in a row at the same time.
        (criteria (player1LosesIf . inARow (==3) <$> directions) . filterValues (== Occupied Player1) `unless`
        criteria (player1WinsIf . inARow (>=4) <$> directions) . filterValues (== Occupied Player1))

      directions = ["vertical", "diagonal1", "diagonal2"]

emptyYavalath :: Int -> Yavalath
emptyYavalath = Yavalath . mapEdges dirName . hexHexGraph
  where
    dirName (1,0) = "vertical"
    dirName (-1,0) = "vertical"
    dirName (1,-1) = "diagonal1"
    dirName (-1,1) = "diagonal1"
    dirName (0,-1) = "diagonal2"
    dirName (0,1) = "diagonal2"