{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

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

#ifdef WASM
import qualified Data.Vector as V ((!), fromList)
import Data.Aeson
import Data.Aeson.Types
import Boardgame.Web (addWebGame, webReady)
#endif

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

-- Import all board games
--import ArithmeticProgressionGame
import ConnectFour
import Cross
import Gale
import Havannah
import Hex
import MNKGame
import ShannonSwitchingGame
import TicTacToe
import Y
import Yavalath

-------------------------------------------------------------------------------
-- * CLI interactions
-------------------------------------------------------------------------------

#ifdef WASM
main :: IO ()
main = do
  addWebGame "TicTacToe" $ emptyMNKGame 3 3 3
  addWebGame "Hex" $ emptyHex 5
  webReady
#else
main :: IO ()
main = do
  putStrLn "1: TicTacToe"
  putStrLn "2: Arithmetic Progression Game"
  putStrLn "3: Shannon Switching Game"
  putStrLn "4: Gale"
  putStrLn "5: Hex"
  putStrLn "6: Havannah"
  putStrLn "7: Yavalath"
  putStrLn "8: Y"
  putStrLn "9: Cross"
  putStrLn "10: Hex (Alternative Version)"
  putStrLn "11: TicTacToe (Alternative Version)"
  putStrLn "12: Connect Four"
  putStrLn "13: Shannon Switching Game (On a ColoredGraph)"
  putStr "What do you want to play? "
  hFlush stdout
  choice <- read <$> getLine
  putStr "\ESC[2J"
  hFlush stdout
  case choice of
    1 -> playIO emptyTicTacToe
    --2 -> playAPG
    3 -> playIO $ createShannonSwitchingGame 5
    4 -> playIO emptyGale
    5 -> playIO $ emptyHex 5
    6 -> playIO $ emptyHavannah 8
    7 -> playIO $ emptyYavalath 8
    8 -> playIO $ emptyCross 8
    9 -> playIO $ emptyCross 8
    10 -> playIO $ emptyHex2 5
    11 -> playIO $ emptyMNKGame 3 3 3
    12 -> playIO $ emptyConnectFour 6 7 4
    13 -> playIO wikipediaReplica
    _ -> putStrLn "Invalid choice!"

{-
playAPG :: IO ()
playAPG = do
  putStr "n: "
  hFlush stdout
  n <- read <$> getLine
  putStr "k: "
  hFlush stdout
  k <- read <$> getLine
  case createArithmeticProgressionGame n k of
    Just a -> playIO a
    Nothing -> putStrLn "Not valid input (n < k)"
    -}
#endif