module ArithmeticProgressionGame where

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
-- * Arithmetic Progression Game
-------------------------------------------------------------------------------

data ArithmeticProgressionGame = ArithmeticProgressionGame Int [Position]

createArithmeticProgressionGame :: Int -> Int -> Maybe ArithmeticProgressionGame
createArithmeticProgressionGame n k = if k < n
  then Just $ ArithmeticProgressionGame k (replicate n Empty)
  else Nothing

instance Show ArithmeticProgressionGame where
  show (ArithmeticProgressionGame _ ps) = (\(is, ps) -> intercalate "," is ++ "\n" ++ intercalate "," ps) $
        unzip $ zipWith (\i p -> (pad $ show i, pad $ showP p)) [1..] ps
    where
      showP Empty           = "  _"
      showP (Occupied Player1) = "  \ESC[34mO\ESC[0m"
      showP (Occupied Player2) = "  \ESC[31mX\ESC[0m"
      pad x = replicate (3 - length x) ' ' ++ x

instance PositionalGame ArithmeticProgressionGame Int where
  getPosition (ArithmeticProgressionGame _ l) i = if i <= length l then Just $ l !! (i - 1) else Nothing
  positions (ArithmeticProgressionGame _ l) = l
  setPosition (ArithmeticProgressionGame k l) i p = if i <= length l
    then Just $ ArithmeticProgressionGame k (take (i - 1) l ++ p : drop i l)
    else Nothing
  gameOver a@(ArithmeticProgressionGame k l) = let n = length l
    in patternMatchingGameOver (filter (all (<= n)) $ concat [[take k [i,i+j..] | j <- [1..n-i]] | i <- [1..n]]) a