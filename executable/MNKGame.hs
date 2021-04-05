{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module MNKGame where

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
  , coloredGraphVertexPositions
  , coloredGraphGetVertexPosition
  , coloredGraphSetVertexPosition
  )

#ifdef WASM
import qualified Data.Vector as V ((!), fromList)
import Data.Aeson
import Data.Aeson.Types
import Boardgame.Web (addWebGame, webReady)
#endif

-------------------------------------------------------------------------------
-- * mnk-game
-------------------------------------------------------------------------------

data MNKGame = MNKGame Int (ColoredGraph (Int, Int) Position String)

instance Show MNKGame where
  show (MNKGame k b) = show b

#if WASM
instance ToJSON MNKGame where
  toJSON (MNKGame _ b) = toJSON b
#endif

instance ColoredGraphTransformer (Int, Int) Position String MNKGame where
  toColoredGraph (MNKGame n b) = b
  fromColoredGraph (MNKGame n _) = MNKGame n

instance PositionalGame MNKGame (Int, Int) where
  positions   = coloredGraphVertexPositions
  getPosition = coloredGraphGetVertexPosition
  setPosition = coloredGraphSetVertexPosition
  gameOver (MNKGame k b) = criterion b
    where
      criterion =
        drawIf (all isOccupied . values) `unless` -- It's a draw if all tiles are owned.
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ mapPosition nextPlayer)
        -- Player1 wins if there are k or more pieces in a row in any direction.
        (criteria (player1WinsIf . inARow (>=k) <$> directions) . filterValues (== Occupied Player1))
      directions = ["vertical", "horizontal", "diagonal1", "diagonal2"]

emptyMNKGame :: Int -> Int -> Int -> MNKGame
emptyMNKGame m n k = MNKGame k $ mapEdges dirName $ rectOctGraph m n
  where
    dirName (1,0)   = "horizontal"
    dirName (-1,0)  = "horizontal"
    dirName (0,-1)  = "vertical"
    dirName (0,1)   = "vertical"
    dirName (1,-1)  = "diagonal1"
    dirName (-1,1)  = "diagonal1"
    dirName (1,1)   = "diagonal2"
    dirName (-1,-1) = "diagonal2"