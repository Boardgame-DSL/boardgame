{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module TicTacToe where

import Data.Graph as Graph (Graph, buildG, path, vertices, indegree, scc, edges)
import Data.List (
    elemIndex
  , find
  , findIndex
  , intersect
  , nub
  , intercalate
  , partition
  , subsequences
  )
import Data.Map (
    Map
  , assocs
  , elems
  , keys
  , fromDistinctAscList
  , fromList
  , insert
  , lookup
  , member
  , (!)
  , adjust
  , alter
  , empty
  )
import Data.Maybe (fromJust, isJust, fromMaybe, mapMaybe)

import Boardgame (
    Player(..)
  , Position(..)
  , Outcome(..)
  , PositionalGame(..)
  , mapPosition
  , isOccupied
  , patternMatchingGameOver
  , playIO
  , takeEmptyMakeMove
  , nextPlayer
  , drawIf
  , ifNotThen
  , player1WinsIf
  , player2WinsIf
  , criteria
  , symmetric
  , player1LosesIf
  , unless
  , makerBreakerGameOver
  )

import System.IO (hFlush, stdout)
import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Data.Tuple (swap)
import qualified Data.Array ((!))
import Data.Foldable (toList)
import Data.Bifunctor (Bifunctor(second))

import Math.Geometry.Grid as Grid ()
import Math.Geometry.Grid.Hexagonal ()
import Boardgame.ColoredGraph (
    ColoredGraph
  , ColoredGraphTransformer(..)
  , paraHexGraph
  , values
  , anyConnections
  , mapValues
  , filterValues
  , filterEdges
  , filterG
  , components
  , hexHexGraph
  , mapEdges
  , rectOctGraph
  , inARow
  , completeGraph
  , filterEdges
  , triHexGraph
  , winningSetPaths
  , coloredGraphVertexPositions
  , coloredGraphGetVertexPosition
  , coloredGraphSetVertexPosition
  , coloredGraphEdgePositions
  , coloredGraphGetEdgePosition
  , coloredGraphSetBidirectedEdgePosition
  )
import Data.Bifunctor (bimap)
import Control.Monad (forM, forM_)
import Data.Tree (Tree, foldTree)

-------------------------------------------------------------------------------
-- * TicTacToe
-------------------------------------------------------------------------------

newtype TicTacToe = TicTacToe (Map (Integer, Integer) Position)

-- Creates an empty TicTacToe board with coordinates `(0..2, 0..2)`
emptyTicTacToe :: TicTacToe
emptyTicTacToe = TicTacToe $
  fromDistinctAscList $
    zip
      [(x, y) | x <- [0..2], y <- [0..2]]
      (repeat Empty)

instance Show TicTacToe where
  show (TicTacToe b) = intercalate "\n" [
      "╔═══╤═══╤═══╗"
    , "║ " ++ intercalate " │ " (row 0) ++ " ║"
    , "╟───┼───┼───╢"
    , "║ " ++ intercalate " │ " (row 1) ++ " ║"
    , "╟───┼───┼───╢"
    , "║ " ++ intercalate " │ " (row 2) ++ " ║"
    , "╚═══╧═══╧═══╝"
    ]
    where
      -- "Shows" the elements of the given row
      row y = map (\x -> showP $ b ! (x, y)) [0..2]
      showP (Occupied Player1) = "\ESC[34mo\ESC[0m"
      showP (Occupied Player2) = "\ESC[31mx\ESC[0m"
      showP Empty = " "

#ifdef WASM
-- Converts the game to a JSON array with three arrays with three integers
-- each. The integers correspond to
-- 0 → Nothing,
-- 1 → Just Player1, and
-- 2 → Just Player2.
instance ToJSON TicTacToe where
  toJSON (TicTacToe b) = Array $ V.fromList $ map row [0..2]
    where
      row y = Array $ V.fromList $ map (\x -> toJSON $ b ! (x, y)) [0..2]
#endif

instance PositionalGame TicTacToe (Integer, Integer) where
  -- Just looks up the coordinate in the underlying Map
  getPosition (TicTacToe b) = flip lookup b
  -- Just returns the elements in the underlying Map
  positions (TicTacToe b) = elems b
  -- If the underlying Map has the given coordinate, update it with the given player
  setPosition (TicTacToe b) c p = if member c b then Just $ TicTacToe $ insert c p b else Nothing
  -- "Creates" a `gameOver` function by supplying all the winning "patterns"
  gameOver = patternMatchingGameOver [
      [(0, 0), (0, 1), (0, 2)]
    , [(1, 0), (1, 1), (1, 2)]
    , [(2, 0), (2, 1), (2, 2)]
    , [(0, 0), (1, 0), (2, 0)]
    , [(0, 1), (1, 1), (2, 1)]
    , [(0, 2), (1, 2), (2, 2)]
    , [(0, 0), (1, 1), (2, 2)]
    , [(2, 0), (1, 1), (0, 2)]
    ]