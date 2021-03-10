{-# LANGUAGE TupleSections #-}

module ColoredGraph (
    ColoredGraph
  , hexHexGraph
  , paraHexGraph
  , rectOctGraph
  , triHexGraph
  , kGraph
  , mapValues
  , mapEdges
  , filterValues
  , filterEdges
  , filterG
  , components
  , anyConnections
  , inARow
  , values
  , subgraph
  , winningSetPaths
  , winningSetPaths'
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List ( find, intersect, (\\) )
import Data.Maybe ( fromJust, isJust, listToMaybe, mapMaybe )
import Data.Tree (Tree(..), foldTree)
import Data.Bifunctor ( bimap, Bifunctor (first, second) )


-- a Graph with colored vertices and edges.
type ColoredGraph i a b = Map i (a, [(b, i)])

type Coordinate = (Int, Int)

hexDirections :: [Coordinate]
hexDirections =
  [ (1, 0)
  , (1, -1)
  , (0, -1)
  , (-1, 0)
  , (-1, 1)
  , (0, 1)
  ]

hexNeighbors :: Coordinate -> [Coordinate]
hexNeighbors (i, j) = bimap (+ i) (+ j) <$> hexDirections

octoDirections :: [Coordinate]
octoDirections =
  [ (1, 0)
  , (1, -1)
  , (0, -1)
  , (-1, -1)
  , (-1, 0)
  , (-1, 1)
  , (0, 1)
  , (1, 1)
  ]

octoNeighbors :: Coordinate -> [Coordinate]
octoNeighbors (i, j) = bimap (+ i) (+ j) <$> octoDirections




mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

binaryOp :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
binaryOp op (x, y) (z, w) = (op x z, op y w)


hexHexGraphRing :: Int -> [Coordinate]
hexHexGraphRing base = concat [oneSide k | k <- [0..5]]
  where
    oneSide :: Int -> [Coordinate]
    oneSide i = [binaryOp (\z w -> base*z + k*w) (hexDirections !! i) (hexDirections !! ((i+2) `mod` 6)) | k <- [1..base]]

distance :: Coordinate -> Coordinate -> Int
distance (x, y) (i, j) = (abs(x - i) + abs(x + y - i - j) + abs(y - j)) `div` 2

hexHexGraph :: Int -> ColoredGraph Coordinate (Maybe a) Coordinate
hexHexGraph radius = Map.fromList ((\z -> (z , (Nothing, filter ((< radius) . distance (0, 0) . snd) $ (\i -> (hexDirections !! i, hexNeighbors z !! i )) <$> [0..5]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = (0, 0) : concatMap hexHexGraphRing [1..radius-1]





paraHexGraph :: Int -> ColoredGraph Coordinate (Maybe a) Coordinate
paraHexGraph n = Map.fromList ((\z -> (z , (Nothing, filter ((\(i, j) -> i < n && i >= 0 && j < n && j >= 0) . snd) $ (\i -> (hexDirections !! i, hexNeighbors z !! i )) <$> [0..5]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = [(i, j) | i <- [0..n-1], j <- [0..n-1]]

rectOctGraph :: Int -> Int -> ColoredGraph (Int, Int) (Maybe a) (Int, Int)
rectOctGraph m n = Map.fromList ((\z -> (z , (Nothing, filter ((\(i, j) -> i < m && i >= 0 && j < n && j >= 0) . snd) $ (\i -> (octoDirections !! i, octoNeighbors z !! i )) <$> [0..7]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = [(i, j) | i <- [0..m-1], j <- [0..n-1]]

triHexGraph :: Int -> ColoredGraph (Int, Int) (Maybe a) (Int, Int)
triHexGraph n = Map.fromList ((\z -> (z , (Nothing, filter ((\(i, j) -> i < n && i >= 0 && j < n && j >= 0 && i+j>=n) . snd) $ (\i -> (hexDirections !! i, hexNeighbors z !! i )) <$> [0..7]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = [(i, j) | i <- [0..n-1], j <- [0..n-1], i+j>=n]

kGraph :: Int -> ColoredGraph Int () ()
kGraph n = Map.fromList [ (i, ((), [((), j) | j <- [0..n-1], i /= j]) )| i <- [0..n-1]]







firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

mapMaybeG :: Ord i => (i -> (a, [(b, i)]) -> Maybe c) -> ColoredGraph i a b -> ColoredGraph i c b
mapMaybeG f g = Map.mapMaybeWithKey (\j (a, xs) -> (, filter (\(_, k) -> isJust $ f k $ fromJust $ Map.lookup k g) xs) <$> f j (a, xs)) g

filterG :: Ord i => (i -> (a, [(b, i)]) -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterG pred = mapMaybeG (\k (z, w) -> if pred k (z, w) then Just z else Nothing)

filterValues :: Ord i => (a -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterValues pred = filterG $ const $ pred . fst

mapValues :: Ord i => (a -> c) -> ColoredGraph i a b -> ColoredGraph i c b
mapValues = fmap . first

mapEdges :: Ord i => (b -> c) -> ColoredGraph i a b -> ColoredGraph i a c
mapEdges = fmap . second . fmap . first

nodesPred :: (a -> [(b, i)] -> Bool) -> ColoredGraph i a b -> [i]
nodesPred pred g = fst <$> filter (uncurry pred . snd) (Map.toList g)

filterEdges :: (b -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterEdges pred = fmap $ second $ filter $ pred . fst

-- Give a path from i to j, including what edgecolor/direction to take.
path :: Ord i => ColoredGraph i a b -> i -> i -> Maybe [(b, i)]
path = path' Set.empty

path' :: Ord i => Set i -> ColoredGraph i a b -> i -> i -> Maybe [(b, i)]
path' s g i j
  | i == j = Just []
  | otherwise = firstJust (\(d, k) -> ((d, k):) <$> path' s' g k j) $ filter (\(_, k) -> not $ k `Set.member` s') neighbours
  where
    (_, neighbours) = g Map.! i
    s' = Set.insert i s


-- a list of all nodes grouped by connected components.
components :: (Eq i, Ord i) => ColoredGraph i a b -> [[i]]
components = components' []
  where
    components' :: (Eq i, Ord i) => [[i]] -> ColoredGraph i a b -> [[i]]
    components' state  g = case find (\k -> all (notElem k) state) (Map.keys g) of
      Just i -> components' (component  g i : state)  g
      Nothing -> state


-- List all the connected nodes starting from one node, also known as a connected component.
component :: Ord i => ColoredGraph i a b -> i -> [i]
component  g = fst . component' Set.empty  g
  where
    component' :: Ord i => Set i -> ColoredGraph i a b -> i -> ([i], Set i)
    component' inputState  g i = (i : xs, newState)
      where
        (_, neighbours) = g Map.! i
        (xs, newState) = foldl tmp ([], Set.insert i inputState) (snd <$> neighbours)

        tmp (ks, state) k
          | k `Set.member` state = (ks, state)
          | otherwise = let (x, y) = component' state  g k in (ks ++ x, y)

values :: ColoredGraph i a b -> [a]
values = fmap fst . Map.elems

-- For every component of G, count how many groups of nodes they overlap with and check if the predicate holds on the count.
-- If it matches on any component then return true. Otherwise return false.
anyConnections :: Ord i => (Int -> Bool) -> [[i]] -> ColoredGraph i a b -> Bool
anyConnections pred groups g = any (\z -> pred $ length $ filter (not . Prelude.null . intersect z) groups) $ components g


-- is there a component along edges `dir` that has a length matching `pred`.
inARow :: (Ord i, Eq b) => (Int -> Bool) -> b -> ColoredGraph i a b -> Bool
inARow pred dir = any (pred . length) . components . filterEdges (==dir)


subgraph :: ColoredGraph i a b -> ColoredGraph i c d -> Bool
subgraph = undefined

-- returns the winning sets representing paths from one set to another on a graph.
winningSetPaths :: Ord i => ColoredGraph i a b -> [i] -> [i] -> [[i]]
winningSetPaths g is js = concat [foldTree (\(isLeaf, z) xs -> if isLeaf then [[z]] else concatMap (fmap (z:)) xs) $ winningSetPaths' g start i goal | i <- is]
  where
    allTrue = True <$ g
    start = foldr (`Map.insert` False) allTrue is

    allFalse = False <$ g
    goal = foldr (`Map.insert` True) allFalse js

winningSetPaths' :: Ord i => ColoredGraph i a b -> Map i Bool -> i -> Map i Bool -> Tree (Bool, i)
winningSetPaths' g allowed i goal = Node (False, i) $ (\k -> if fromJust $ Map.lookup k goal then Node (True, k) [] else winningSetPaths' g allowed' k goal) <$> neighbourIndices
  where
    neighbourIndices = filter (fromJust . flip Map.lookup allowed) $ fmap snd (snd $ fromJust $ Map.lookup i g)
    allowed' = foldr (`Map.insert` False) allowed neighbourIndices
