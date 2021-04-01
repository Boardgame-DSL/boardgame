{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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

#ifdef WASM
instance ToJSON ArithmeticProgressionGame where
  toJSON (ArithmeticProgressionGame n ps) = object [
      "n"         .= toJSON n
    , "positions" .= toJSON ps
    ]
#endif

instance PositionalGame ArithmeticProgressionGame Int where
  getPosition (ArithmeticProgressionGame _ l) i = if i <= length l then Just $ l !! (i - 1) else Nothing
  positions (ArithmeticProgressionGame _ l) = l
  setPosition (ArithmeticProgressionGame k l) i p = if i <= length l
    then Just $ ArithmeticProgressionGame k (take (i - 1) l ++ p : drop i l)
    else Nothing
  gameOver a@(ArithmeticProgressionGame k l) = let n = length l
    in patternMatchingGameOver (filter (all (<= n)) $ concat [[take k [i,i+j..] | j <- [1..n-i]] | i <- [1..n]]) a

-------------------------------------------------------------------------------
-- * Shannon Switching Game
-------------------------------------------------------------------------------

newtype ShannonSwitchingGame = ShannonSwitchingGame (Int, [((Int, Int), Position)])

-- | Creates a list of all edges, input n gives n*n graph
gridEdges :: Int -> [((Int, Int), Position)]
gridEdges n =
  concat [[((j+i*n,(j+1)+i*n), Empty), ((j+i*n,j+(i+1)*n), Empty)] | i <- [0..n-2], j <- [0..n-2]] ++
  [(((n-1)+i*n, (n-1)+(i+1)*n), Empty) | i <- [0..n-2]] ++
  [((i+(n-1)*n, (i+1)+(n-1)*n), Empty) | i <- [0..n-2]]

createShannonSwitchingGame :: Int -> ShannonSwitchingGame
createShannonSwitchingGame n = ShannonSwitchingGame (n, gridEdges n)

-- o───o---o
-- ║   │   :
-- o═══o═══o
-- ║   │   :
-- o───o───o
instance Show ShannonSwitchingGame where
  show a@(ShannonSwitchingGame (n, l)) = intercalate "\n" ([concat ["o" ++ showH (fromJust $ getPosition a (i+j*n, (i+1)+j*n)) | i <- [0 .. n - 2]]
    ++ "o\n" ++ concat [showV (fromJust $ getPosition a (i+j*n, i+(j+1)*n)) ++ "   " | i <- [0 .. n - 2]] ++ showV (fromJust $ getPosition a ((n-1)+j*n, (n-1)+(j+1)*n) ) |
    j <- [0 .. n - 2]]
    ++ [concat ["o" ++ showH (fromJust $ getPosition a (i+(n-1)*n, (i+1)+(n-1)*n)) | i <- [0 .. n - 2]] ++ "o"])
    where
      showH (Occupied Player1) = "\ESC[34m───\ESC[0m"
      showH (Occupied Player2) = "\ESC[31m───\ESC[0m"
      showH Empty           = "───"
      showV (Occupied Player1) = "\ESC[34m│\ESC[0m"
      showV (Occupied Player2) = "\ESC[31m│\ESC[0m"
      showV Empty           = "│"

#ifdef WASM
instance ToJSON ShannonSwitchingGame where
  toJSON (ShannonSwitchingGame (n, ps)) = object [
      "n"         .= toJSON n
    , "positions" .= toJSON ps
    ]
#endif

instance PositionalGame ShannonSwitchingGame (Int, Int) where
  getPosition (ShannonSwitchingGame (_, l)) c = snd <$> find ((== c) . fst) l
  positions (ShannonSwitchingGame (_, l)) = map snd l
  setPosition (ShannonSwitchingGame (n, l)) c p = case findIndex ((== c) . fst) l of
    Just i -> Just $ ShannonSwitchingGame (n, take i l ++ (c, p) : drop (i + 1) l)
    Nothing -> Nothing
  gameOver (ShannonSwitchingGame (n, l))
    | path g 0 (n * n - 1) = Just $ Win Player1
    | path g (n - 1) (n * n - n) = Just $ Win Player2
    | all (isOccupied . snd) l = Just Draw
    | otherwise = Nothing
    where
      g = buildG (0, n * n - 1) (map fst $ filter ((== Occupied Player1) . snd) l)

-------------------------------------------------------------------------------
-- * Shannon Switching Game (On a ColoredGraph)
-------------------------------------------------------------------------------

-- Operates under the invariant that all edges in 'graph' are bi-directional
-- and their values are in sync.
data ShannonSwitchingGameCG = ShannonSwitchingGameCG {
    start :: Int
  , goal  :: Int
  , graph :: ColoredGraph Int () Position
  }
  deriving (Show)

#if WASM
instance ToJSON ShannonSwitchingGameCG where
  toJSON ShannonSwitchingGameCG{ start, goal, graph } =
    object [
        "start" .= start
      , "goal" .= goal
      , "graph" .= graph
      ]
#endif

instance ColoredGraphTransformer Int () Position ShannonSwitchingGameCG where
  toColoredGraph = graph
  fromColoredGraph ssg graph = ssg{ graph }

instance PositionalGame ShannonSwitchingGameCG (Int, Int) where
  positions = coloredGraphEdgePositions
  getPosition = coloredGraphGetEdgePosition
  setPosition = coloredGraphSetBidirectedEdgePosition
  gameOver ShannonSwitchingGameCG{ start, goal, graph } =
      ifNotThen (player1WinsIf winPath) (player1LosesIf losePath) graph
    where
      winPath = anyConnections (==2) [[start], [goal]] . filterEdges (== Occupied Player1)
      losePath = not . anyConnections (==2) [[start], [goal]] . filterEdges (/= Occupied Player2)

createEmptyShannonSwitchingGameCG :: [(Int, Int)] -> Int -> Int -> ShannonSwitchingGameCG
createEmptyShannonSwitchingGameCG pairs start goal = ShannonSwitchingGameCG{
      start
    , goal
    , graph = foldl addPathToMap empty $ pairs >>= (\(from, to) -> [(from, to), (to, from)])
  }
  where
    addPathToMap m (from, to) = alter updateOrInsert from m
      where
        updateOrInsert existing = case existing of
          Just (a, edges) -> Just (a, insert to Empty edges)
          Nothing -> Just ((), fromList [(to, Empty)])

-- Creates a 'ShannonSwitchingGameCG' on a graph like the one from Wikipedia.
-- https://en.wikipedia.org/wiki/Shannon_switching_game#/media/File:Shannon_game_graph.svg
wikipediaReplica :: ShannonSwitchingGameCG
wikipediaReplica = createEmptyShannonSwitchingGameCG connections 0 3
  where
    connections = [
        (0, 1)
      , (0, 4)
      , (0, 7)
      , (1, 2)
      , (1, 5)
      , (2, 3)
      , (4, 5)
      , (4, 6)
      , (4, 7)
      , (5, 3)
      , (5, 6)
      , (6, 3)
      , (7, 6)
      ]

-------------------------------------------------------------------------------
-- * Gale
-------------------------------------------------------------------------------

newtype Gale = Gale (Map (Integer, Integer) Position)

-- Creates an empty Gale playfield. Even "rows" have 4 "columns" and odd ones
-- have 3.
emptyGale :: Gale
emptyGale = Gale $
  fromList $
    zip
      ([0..8] >>= (\y -> [(x, y) | x <- [0..(4 - y `rem` 2)]]))
      (repeat Empty)

--    0 1 2 3 4 5 6 7 8
--    ╔═══╦═══╦═══╦═══╗
-- 0┌   ┬   ┬   ┬   ┬   ┐
-- 1│ ╠   ╬   ╬   ╬   ╣ │
-- 2├   ┼   ┼   ┼   ┼   ┤
-- 3│ ╠   ╬   ╬   ╬   ╣ │
-- 4├   ┼   ┼   ┼   ┼   ┤
-- 5│ ╠   ╬   ╬   ╬   ╣ │
-- 6├   ┼   ┼   ┼   ┼   ┤
-- 7│ ╠   ╬   ╬   ╬   ╣ │
-- 8└   ┴   ┴   ┴   ┴   ┘
--    ╚═══╩═══╩═══╩═══╝
instance Show Gale where
  show (Gale b) = intercalate "\n" [
        "   0 1 2 3 4 5 6 7 8  "
      , "   \ESC[31m╔═══╦═══╦═══╦═══╗\ESC[0m  "
      , "0\ESC[34m┌" ++ intercalate "\ESC[34m┬" (row 0) ++ "\ESC[34m┐\ESC[0m"
      , "1\ESC[34m│ \ESC[31m╠" ++ intercalate "\ESC[31m╬" (row 1) ++ "\ESC[31m╣ \ESC[34m│\ESC[0m"
      , "2\ESC[34m├" ++ intercalate "\ESC[34m┼" (row 2) ++ "\ESC[34m┤\ESC[0m"
      , "3\ESC[34m│ \ESC[31m╠" ++ intercalate "\ESC[31m╬" (row 3) ++ "\ESC[31m╣ \ESC[34m│\ESC[0m"
      , "4\ESC[34m├" ++ intercalate "\ESC[34m┼" (row 4) ++ "\ESC[34m┤\ESC[0m"
      , "5\ESC[34m│ \ESC[31m╠" ++ intercalate "\ESC[31m╬" (row 5) ++ "\ESC[31m╣ \ESC[34m│\ESC[0m"
      , "6\ESC[34m├" ++ intercalate "\ESC[34m┼" (row 6) ++ "\ESC[34m┤\ESC[0m"
      , "7\ESC[34m│ \ESC[31m╠" ++ intercalate "\ESC[31m╬" (row 7) ++ "\ESC[31m╣ \ESC[34m│\ESC[0m"
      , "8\ESC[34m└" ++ intercalate "\ESC[34m┴" (row 8) ++ "\ESC[34m┘\ESC[0m"
      , "   \ESC[31m╚═══╩═══╩═══╩═══╝\ESC[0m  "
      ]
    where
      -- "Shows" the elements of the given row
      row y = map (\x -> showP (b ! (x, y)) y) [0..(4 - y `rem` 2)]
      showP (Occupied Player1) y
        | even y      = "\ESC[34m───"
        | otherwise   = " \ESC[34m│ "
      showP (Occupied Player2) y
        | even y      = " \ESC[31m║ "
        | otherwise   = "\ESC[31m═══"
      showP Empty _   = "   "

#ifdef WASM
instance ToJSON Gale where
  toJSON (Gale b) = toJSON b
#endif

instance PositionalGame Gale (Integer, Integer) where
  getPosition (Gale b) (x, y) = if x `rem` 2 == y `rem` 2 then lookup c b else Nothing
    where c = (x `div` 2, y)
  positions (Gale b) = elems b
  setPosition (Gale b) (x, y) p = if x `rem` 2 == y `rem` 2 && member c b then Just $ Gale $ insert c p b else Nothing
    where c = (x `div` 2, y)
  gameOver (Gale b)
    | path player1Graph (-1) (-2) = Just $ Win Player1
    | path player2Graph (-1) (-2) = Just $ Win Player2
    | all isOccupied (elems b) = Just Draw
    | otherwise            = Nothing
    where
      playerGraph from to p = buildG (-2, 19) $
        filter ((Occupied p ==) . snd) (assocs b) >>=
          ((\(x, y) -> [(x, y), (y, x)]) . (\((x, y), _) -> (fromInteger $ from x y, fromInteger $ to x y)))
      player1Graph = playerGraph
        (\x y -> if x == 0 then -1 else (y `div` 2) + 5 * (x + (y `rem` 2) - 1))
        (\x y -> if x == 4 then -2 else (y `div` 2) + (y `rem` 2) + 5 * x)
        Player1
      player2Graph = playerGraph
        (\x y -> if y == 0 then -1 else x + 5 * ((y `div` 2) + (y `rem` 2) - 1))
        (\x y -> if y == 8 then -2 else x + (y `rem` 2) + 5 * (y `div` 2))
        Player2

-------------------------------------------------------------------------------
-- * Hex
-------------------------------------------------------------------------------

data Hex = Hex Int (ColoredGraph (Int, Int) Position (Int, Int))

emptyHex :: Int -> Hex
emptyHex n = Hex n $ paraHexGraph n

instance Show Hex where
  show (Hex n b) =
    replicate (2*(n-1)) ' ' ++ concat (replicate n "  _ ") ++ "\n"
    ++
    intercalate "\n" [intercalate "\n" (gridShowLine (Hex n b) r) | r <- [0..n-1]]
    ++
    "\n" ++ concat (replicate n " \\_/")

#ifdef WASM
instance ToJSON Hex where
  toJSON (Hex n b) = object [
      "n"     .= toJSON n
    , "board" .= toJSON b
    ]
#endif

gridShowLine :: Hex -> Int -> [String]
gridShowLine (Hex n b) y  = [rowOffset ++ tileTop ++ [x | y/=0, x <- " /"]
                          ,rowOffset ++ "| " ++ intercalate " | " (map (\x -> showP $ fst $ fromJust $ lookup (x, n-1-y) b) [0..(n-1)]) ++ " |"
                          ] where
  showP (Occupied Player1) = "1"
  showP (Occupied Player2) = "2"
  showP Empty           = " "
  rowOffset = replicate (2*(n-y-1)) ' '
  tileTop = concat $ replicate n " / \\"

instance ColoredGraphTransformer (Int, Int) Position (Int, Int) Hex where
  toColoredGraph (Hex n b) = b
  fromColoredGraph (Hex n _) = Hex n

instance PositionalGame Hex (Int, Int) where
  positions = coloredGraphVertexPositions
  getPosition = coloredGraphGetVertexPosition
  setPosition = coloredGraphSetVertexPosition
  gameOver (Hex n b) = criterion b
    where
      criterion =
        criteria
          -- There is a connection between 2 components, the left and right.
          [ player1WinsIf (anyConnections (==2) [left, right]) . filterValues (== Occupied Player1)
           -- There is a connection between 2 components, the top and bottom.
          , player2WinsIf (anyConnections (==2) [top, bottom]) . filterValues (== Occupied Player2)
          ]
      left   = [(0,  i) | i <- [0..n-1]]
      right  = [(n-1,i) | i <- [0..n-1]]
      top    = [(i,  0) | i <- [0..n-1]]
      bottom = [(i,n-1) | i <- [0..n-1]]

-------------------------------------------------------------------------------
-- * Hex2
-------------------------------------------------------------------------------

data Hex2 = Hex2 Int (ColoredGraph (Int, Int) Position (Int, Int))

emptyHex2 :: Int -> Hex2
emptyHex2 n = Hex2 n $ paraHexGraph n

instance Show Hex2 where
  show (Hex2 n b) =
    replicate (2*(n-1)) ' ' ++ concat (replicate n "  _ ") ++ "\n"
    ++
    intercalate "\n" [intercalate "\n" (gridShowLine2 (Hex2 n b) r) | r <- [0..n-1]]
    ++
    "\n" ++ concat (replicate n " \\_/")

#ifdef WASM
instance ToJSON Hex2 where
  toJSON (Hex2 n b) = object [
      "n"     .= toJSON n
    , "board" .= toJSON b
    ]
#endif

gridShowLine2 :: Hex2 -> Int -> [String]
gridShowLine2 (Hex2 n b) y  = [rowOffset ++ tileTop ++ [x | y/=0, x <- " /"]
                          ,rowOffset ++ "| " ++ intercalate " | " (map (\x -> showP $ fst $ fromJust $ lookup (x, n-1-y) b) [0..(n-1)]) ++ " |"
                          ] where
  showP (Occupied Player1) = "1"
  showP (Occupied Player2) = "2"
  showP Empty           = " "
  rowOffset = replicate (2*(n-y-1)) ' '
  tileTop = concat $ replicate n " / \\"

instance PositionalGame Hex2 (Int, Int) where
  getPosition (Hex2 n b) c = fst <$> lookup c b
  positions (Hex2 n b) = values b
  setPosition (Hex2 n b) c p = if member c b
    then Just $ Hex2 n $ adjust (\(_, xs) -> (p, xs)) c b
    else Nothing
  makeMove = takeEmptyMakeMove
  gameOver (Hex2 n b) = makerBreakerGameOver (allWinningHexPaths n) (Hex2 n b)

allWinningHexPaths :: Int -> [[(Int, Int)]]
allWinningHexPaths n = winningSetPaths (paraHexGraph n) left right
  where
    left   = [(0,  i) | i <- [0..n-1]]
    right  = [(n-1,i) | i <- [0..n-1]]

-------------------------------------------------------------------------------
-- * Havannah
-------------------------------------------------------------------------------

newtype Havannah = Havannah (ColoredGraph (Int, Int) Position ())
  deriving (ColoredGraphTransformer (Int, Int) Position ())

instance Show Havannah where
  show (Havannah b) = show b

#ifdef WASM
instance ToJSON Havannah where
  toJSON (Havannah b) = toJSON b
#endif

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

-------------------------------------------------------------------------------
-- * Yavalath
-------------------------------------------------------------------------------

newtype Yavalath = Yavalath (ColoredGraph (Int, Int) Position String)
  deriving (ColoredGraphTransformer (Int, Int) Position String)

instance Show Yavalath where
  show (Yavalath b) = show b

#ifdef WASM
instance ToJSON Yavalath where
  toJSON (Yavalath b) = toJSON b
#endif

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

-------------------------------------------------------------------------------
-- * mnk-game
-------------------------------------------------------------------------------

data MNKGame = MNKGame Int (ColoredGraph (Int, Int) Position String)

instance Show MNKGame where
  show (MNKGame k b) = show b

#if WASM
instance ToJSON MNKGame where
  toJSON (MNKGame k b) = object [
      "k"     .= toJSON k
    , "board" .= toJSON b
    ]
#endif

instance ColoredGraphTransformer (Int, Int) Position String MNKGame where
  toColoredGraph (MNKGame n b) = b
  fromColoredGraph (MNKGame n _) = MNKGame n

instance PositionalGame MNKGame (Int, Int) where
  positions = coloredGraphVertexPositions
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
    dirName (1,0) = "horizontal"
    dirName (-1,0) = "horizontal"
    dirName (0,-1) = "vertical"
    dirName (0,1) = "vertical"
    dirName (1,-1) = "diagonal1"
    dirName (-1,1) = "diagonal1"
    dirName (1,1) = "diagonal2"
    dirName (-1,-1) = "diagonal2"

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
  getPosition (Y b) c = fst <$> lookup c b
  positions (Y b) = values b
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
        player1WinsIf $ anyConnections (==3) [side1, side2, side3] . filterValues (== Occupied Player1)

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
  getPosition (Cross b) c = fst <$> lookup c b
  positions (Cross b) = values b
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

      side1 = emptyNeighbours [dirs !! 0, dirs !! 1, dirs !! 2]
      side2 = emptyNeighbours [dirs !! 1, dirs !! 2, dirs !! 3]
      side3 = emptyNeighbours [dirs !! 2, dirs !! 3, dirs !! 4]
      side4 = emptyNeighbours [dirs !! 3, dirs !! 4, dirs !! 5]
      side5 = emptyNeighbours [dirs !! 4, dirs !! 5, dirs !! 0]
      side6 = emptyNeighbours [dirs !! 5, dirs !! 0, dirs !! 1]

emptyCross :: Int -> Cross
emptyCross = Cross . hexHexGraph

-------------------------------------------------------------------------------
-- * Connect Four
-------------------------------------------------------------------------------

data ConnectFour = ConnectFour Int (ColoredGraph (Int, Int) Position String)

instance Show ConnectFour where
  show (ConnectFour k b) = show b

#ifdef WASM
instance ToJSON ConnectFour where
  toJSON (ConnectFour k b) = object [
      "k"     .= toJSON k
    , "board" .= toJSON b
    ]
#endif

instance PositionalGame ConnectFour (Int, Int) where
  getPosition (ConnectFour k b) c = fst <$> lookup c b
  positions   (ConnectFour k b) = values b
  setPosition (ConnectFour k b) c p = if member c b
    then Just $ ConnectFour k $ adjust (\(_, xs) -> (p, xs)) c b
    else Nothing
  makeMove = newMakeMove

  gameOver (ConnectFour k b) = criterion b
    where
      criterion =
        drawIf (all isOccupied . values) `unless` -- It's a draw if all tiles are owned.
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ mapPosition nextPlayer)
        -- Player1 wins if there are k or more pieces in a row in any direction.
        (criteria (player1WinsIf . inARow (>=k) <$> directions) . filterValues (== Occupied Player1))

      directions = ["vertical", "horizontal", "diagonal1", "diagonal2"]

-- | Restrict move.
--   Move is only valid if the positon is empty and the position below is occupied.
  -- (row, col)    = Nothing
  -- (row, col-1) /= Nothing
newMakeMove :: ConnectFour -> Player -> (Int, Int) -> Maybe ConnectFour
newMakeMove a p coord = case getPosition a coord of
  -- If we are at bottom row, we can place the piece there.
  Just Empty -> if ((fst coord) == 0)
                    then setPosition a coord (Occupied p)
                    -- Not at bottom row, check to see if position below has been filled.
                    else case getPosition a ((fst coord) -1, snd coord) of
                      Just Empty -> Nothing
                      _          -> setPosition a coord (Occupied p)
  _          -> Nothing

emptyConnectFour :: Int -> Int -> Int -> ConnectFour
emptyConnectFour m n k = ConnectFour k $ mapEdges dirName $ rectOctGraph m n
  where
    dirName (1,0)   = "horizontal"
    dirName (-1,0)  = "horizontal"
    dirName (0,-1)  = "vertical"
    dirName (0,1)   = "vertical"
    dirName (1,-1)  = "diagonal1"
    dirName (-1,1)  = "diagonal1"
    dirName (1,1)   = "diagonal2"
    dirName (-1,-1) = "diagonal2"

-------------------------------------------------------------------------------
-- * CLI interactions
-------------------------------------------------------------------------------

#ifdef WASM
main :: IO ()
main = do
  addWebGame "TicTacToe" emptyTicTacToe
  addWebGame "Arithmetic Progression Game" $ fromJust $ createArithmeticProgressionGame 35 5
  addWebGame "Shannon Switching Game" $ createShannonSwitchingGame 5
  addWebGame "Gale" emptyGale
  addWebGame "Hex" $ emptyHex 5
  addWebGame "Havannah" $ emptyHavannah 8
  addWebGame "Yavalath" $ emptyYavalath 8
  addWebGame "Y" $ emptyY 8
  addWebGame "Cross" $ emptyCross 8
  addWebGame "Hex (Alternative Version)" $ emptyHex2 5
  addWebGame "TicTacToe (Alternative Version)" $ emptyMNKGame 3 3 3
  addWebGame "Connect Four" $ emptyConnectFour 6 7 4
  addWebGame "Shannon Switching Game (On a ColoredGraph)" $ wikipediaReplica
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
    2 -> playAPG
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
#endif
