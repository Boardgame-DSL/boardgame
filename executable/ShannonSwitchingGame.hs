{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ShannonSwitchingGame where

import Data.Graph as Graph (buildG, path)

import Data.List (
    find
  , findIndex
  , intercalate
  , subsequences
  , sortOn
  )

import Data.Map (
    fromList
  , insert
  , alter
  , empty
  , assocs
  , keys
  )
  
import Data.Maybe (
    fromJust
  , isNothing
  , mapMaybe
  )

import Boardgame (
    Player(..)
  , Position(..)
  , Outcome(..)
  , PositionalGame(..)
  , isOccupied
  , ifNotThen
  , player1WinsWhen
  , player1LosesWhen
  )

import Boardgame.ColoredGraph (
    ColoredGraph
  , ColoredGraphTransformer(..)
  , anyConnections
  , edgePath
  , mapEdges
  , filterEdges
  , coloredGraphEdgePositions
  , coloredGraphGetEdgePosition
  , coloredGraphSetBidirectedEdgePosition
  )

#ifdef WASM
import Data.Aeson (
    ToJSON(..)
  , object
  , (.=)
  )
#endif

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
  show a@(ShannonSwitchingGame (n, _)) = intercalate "\n" ([concat ["o" ++ showH (fromJust $ getPosition a (i+j*n, (i+1)+j*n)) | i <- [0 .. n - 2]]
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
  positions   (ShannonSwitchingGame (_, l))     = map snd l
  getPosition (ShannonSwitchingGame (_, l)) c   = snd <$> find ((== c) . fst) l
  setPosition (ShannonSwitchingGame (n, l)) c p = case findIndex ((== c) . fst) l of
    Just i -> Just $ ShannonSwitchingGame (n, take i l ++ (c, p) : drop (i + 1) l)
    Nothing -> Nothing
  gameOver (ShannonSwitchingGame (n, l))
    | path g 0 (n * n - 1) = Just (Win Player1, [])
    | path g (n - 1) (n * n - n) = Just (Win Player2, [])
    | all (isOccupied . snd) l = Just (Draw, [])
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
      , "goal"  .= goal
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
      ifNotThen (player1WinsWhen winPath) (player1LosesWhen losePath) graph
    where
      winPath  = fmap edgePath . anyConnections (==2) [[start], [goal]] . filterEdges (== Occupied Player1)
      losePath g = do
        -- Gets the (from, to) coordinates of edges Occupied by Player2
        let cut = assocs (filterEdges (== Occupied Player2) g) >>= \(f, (_, ts)) -> mapMaybe (\t -> if f <= t then Just (f, t) else Nothing) $ keys ts
        -- Get all subsequences from shortest to longest
        let ss = sortOn length $ subsequences cut
        -- A ColoredGraph where all edges Occupied by Player2 are cleared
        let clearedG = mapEdges (\case Occupied Player2 -> Empty; p -> p) g
        -- Returns the first subsequence (the shortest) that successfully
        -- prevent Player1 from winning
        find (losePathTest . foldl (\g c -> fromJust $ coloredGraphSetBidirectedEdgePosition g c (Occupied Player2)) clearedG) ss
      losePathTest = isNothing . anyConnections (==2) [[start], [goal]] . filterEdges (/= Occupied Player2)

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