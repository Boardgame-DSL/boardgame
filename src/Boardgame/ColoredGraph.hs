{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module:      Boardgame.ColoredGraph
Description: A graph library specialized for boardgames. Colored graphs have
             colors, or values, on each vertex and each edge.

This module contains helper functions for games that can be modeled as graphs.

It contains a few functions for creating graphs of different shapes and with
different properties. 'hexHexGraph', 'paraHexGraph', 'rectOctGraph', and more.

It also contains a few functions that can automatically implement
'Boardgame.PositionalGame' for most cases. These are named after the function
they implement, prefixed with @coloredGraph@ and the addition of how the
implement them. For example 'coloredGraphGetVertexPosition' and
'coloredGraphSetBidirectedEdgePosition'.
-}
module Boardgame.ColoredGraph (
    ColoredGraph
  , ColoredGraphTransformer(..)
  , hexDirections
  , octoDirections
  , hexHexGraph
  , paraHexGraph
  , rectOctGraph
  , triHexGraph
  , completeGraph
  , mapValues
  , mapEdges
  , filterValues
  , filterEdges
  , filterG
  , components
  , anyConnections
  , edgePath
  , inARow
  , values
  , missingEdgeColors
  , missingDirections
  , winningSetPaths
  , winningSetPaths'
  , coloredGraphVertexPositions
  , coloredGraphSetVertexPosition
  , coloredGraphGetVertexPosition
  , coloredGraphEdgePositions
  , coloredGraphGetEdgePosition
  , coloredGraphSetEdgePosition
  , coloredGraphSetBidirectedEdgePosition
) where

import Data.Map (Map, mapMaybeWithKey, filterWithKey)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List ( find, intersect )
import Data.Maybe ( fromJust, listToMaybe, mapMaybe )
import Data.Tree (Tree(..), foldTree)
import Control.Monad ((<=<))
import Data.Bifunctor ( bimap, Bifunctor (first, second) )
import Boardgame (Position(..))


-- | A Graph with colored vertices and edges. The key of the map is 'i', the
--   "coordinates". The value of the map is a tuple of vertices color 'a', and
--   a list of edges. The edges are tuples of edge color 'b' and
--   "target coordinate" 'i'.
type ColoredGraph i a b = Map i (a, Map i b)

type Coordinate = (Int, Int)

-- | The six directions of neighbours on a hexagonal grid.
hexDirections :: [Coordinate]
hexDirections =
  [ (1, 0)
  , (1, -1)
  , (0, -1)
  , (-1, 0)
  , (-1, 1)
  , (0, 1)
  ]

-- Returns the six neighboring coordinates of the given coordinate on a
-- hexagonal grid.
hexNeighbors :: Coordinate -> [Coordinate]
hexNeighbors (i, j) = bimap (+ i) (+ j) <$> hexDirections

-- | The eight directions of neighbours on a square grid.
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

-- Returns the eight neighboring coordinates of the given coordinate on a
-- square grid.
octoNeighbors :: Coordinate -> [Coordinate]
octoNeighbors (i, j) = bimap (+ i) (+ j) <$> octoDirections





-- Combines two tuples using the given function.
binaryOp :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
binaryOp op (x, y) (z, w) = (op x z, op y w)

hexHexGraphRing :: Int -> [Coordinate]
hexHexGraphRing base = concat [oneSide k | k <- [0..5]]
  where
    oneSide :: Int -> [Coordinate]
    oneSide i = [binaryOp (\z w -> base*z + k*w) (hexDirections !! i) (hexDirections !! ((i+2) `mod` 6)) | k <- [1..base]]

-- Returns the distance between two hexagonal coordinates.
distance :: Coordinate -> Coordinate -> Int
distance (x, y) (i, j) = (abs(x - i) + abs(x + y - i - j) + abs(y - j)) `div` 2

-- | Creates a hexagon shaped graph of hexagon vertices (each vertex has six
--   outgoing edges) with the given radius.
--
--   The "coordinates" of the graph will be '(Int, Int)' where '(0, 0)' is at
--   the center. The color of edges will also be a '(Int, Int)' tuple that
--   shows the "direction" of the edge.
hexHexGraph :: Int -> ColoredGraph (Int, Int) Position (Int, Int)
hexHexGraph radius = Map.fromList ((\z -> (z , (Empty, Map.fromList $ filter ((< radius) . distance (0, 0) . fst) $ (\i -> (hexNeighbors z !! i, hexDirections !! i)) <$> [0..5]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = (0, 0) : concatMap hexHexGraphRing [1..radius-1]





-- | Creates a parallelogram shaped graph of hexagon vertices (each vertex has
--   six outgoing edges) with the given side length.
--
--   The "coordinates" of the graph will be '(Int, Int)' where '(0, 0)' is at
--   the center. The color of edges will also be a '(Int, Int)' tuple that
--   shows the "direction" of the edge.
paraHexGraph :: Int -> ColoredGraph (Int, Int) Position (Int, Int)
paraHexGraph n = Map.fromList ((\z -> (z , (Empty, Map.fromList $ filter ((\(i, j) -> i < n && i >= 0 && j < n && j >= 0) . fst) $ (\i -> (hexNeighbors z !! i, hexDirections !! i)) <$> [0..5]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = [(i, j) | i <- [0..n-1], j <- [0..n-1]]

-- | Creates a rectangular shaped graph of octagon vertices (each vertex has
--   eight outgoing edges) with the given width and height.
--
--   The "coordinates" of the graph will be '(Int, Int)' where '(0, 0)' the top
--   left vertex. The color of edges will also be a '(Int, Int)' tuple that
--   shows the "direction" of the edge.
rectOctGraph :: Int -> Int -> ColoredGraph (Int, Int) Position (Int, Int)
rectOctGraph m n = Map.fromList ((\z -> (z , (Empty, Map.fromList $ filter ((\(i, j) -> i < m && i >= 0 && j < n && j >= 0) . fst) $ (\i -> (octoNeighbors z !! i, octoDirections !! i)) <$> [0..7]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = [(i, j) | i <- [0..m-1], j <- [0..n-1]]

-- | Creates a triangular shaped graph of hexagon vertices (each vertex has
--   six outgoing edges) with the given side length.
--
--   The "coordinates" of the graph will be '(Int, Int)' where '(1, n-1)',
--   '(n-1, 1)' and '(n-1, n-1)' are the 3 corners. The color of edges will
--   also be a '(Int, Int)' tuple that shows the "direction" of the edge.
triHexGraph :: Int -> ColoredGraph (Int, Int) Position (Int, Int)
triHexGraph n = Map.fromList ((\z -> (z, (Empty, Map.fromList $ filter ((\(i, j) -> i < n && i >= 0 && j < n && j >= 0 && i + j >= n) . fst) $ (\i -> (hexNeighbors z !! i, hexDirections !! i)) <$> [0 .. 5]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = [(i, j) | i <- [0 .. n -1], j <- [0 .. n -1], i + j >= n]

-- | Creates a complete graph with n vertices.
completeGraph :: Int -> ColoredGraph Int () ()
completeGraph n = Map.fromList [ (i, ((), Map.fromList [(j, ()) | j <- [0..n-1], i /= j])) | i <- [0..n-1]]







-- Returns the first value that is accepted by the predicate, or 'Nothing'.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

-- Maps the vertices, and their outgoing edges with values, and collects the
-- 'Just' results.
mapMaybeG :: Ord i => ((a, Map i b) -> Maybe c) -> ColoredGraph i a b -> ColoredGraph i c b
mapMaybeG f g = fmap (second (Map.filterWithKey (\k _ -> Map.member k g'))) g'
  where
    g' = Map.mapMaybe (\(a, xs) -> (, xs) <$> f (a, xs)) g

-- | Filters out any vertices whose value, and their outgoing edges with
--   values, is not accepted by the predicate.
filterG :: Ord i => ((a, Map i b) -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterG predicate = mapMaybeG (\(z, w) -> if predicate (z, w) then Just z else Nothing)

-- | Filters out any vertices whose value is not accepted by the predicate.
filterValues :: Ord i => (a -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterValues predicate = filterG $ predicate . fst

-- | Maps the values of vertices with the given function.
mapValues :: Ord i => (a -> c) -> ColoredGraph i a b -> ColoredGraph i c b
mapValues = fmap . first

-- | Maps the values of edges with the given function.
mapEdges :: Ord i => (b -> c) -> ColoredGraph i a b -> ColoredGraph i a c
mapEdges = fmap . second . fmap

-- Returns a list of "coordinates" for vertices whose value, and their outgoing
-- edges with values, are accepted by the predicate.
nodesPred :: (a -> Map i b -> Bool) -> ColoredGraph i a b -> [i]
nodesPred predicate g = fst <$> filter (uncurry predicate . snd) (Map.toList g)

-- | Filters out any edges whose value is not accepted by the predicate.
filterEdges :: (b -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterEdges = fmap . second . Map.filter

-- Returns a path from i to j, including what edge value to take.
path :: Ord i => ColoredGraph i a b -> i -> i -> Maybe [(b, i)]
path = path' Set.empty

-- Returns a path from i to j, including what edge value to take. With a set of
-- already visited "coordinates".
path' :: Ord i => Set i -> ColoredGraph i a b -> i -> i -> Maybe [(b, i)]
path' s g i j
  | i == j = Just []
  | otherwise = firstJust (\(k, d) -> ((d, k):) <$> path' s' g k j) $ filter (\(k, _) -> not $ k `Set.member` s') neighbours
  where
    neighbours = Map.assocs $ snd $ g Map.! i
    s' = Set.insert i s


-- | A list of all vertices grouped by connected components.
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
    component' inputState g i = (i : xs, newState)
      where
        neighbours = Map.assocs $ snd $ g Map.! i
        (xs, newState) = foldl tmp ([], Set.insert i inputState) (fst <$> neighbours)

        tmp (ks, state) k
          | k `Set.member` state = (ks, state)
          | otherwise = let (x, y) = component' state  g k in (ks ++ x, y)

-- | Returns a list of vertex values from the given graph.
values :: ColoredGraph i a b -> [a]
values = fmap fst . Map.elems

-- | Returns a graph formed from a subset of vertices and
--   all edges connecting those vertices in the original graph.
inducedSubgraph :: Eq i => ColoredGraph i a b -> [i] -> ColoredGraph i a b
inducedSubgraph g nodes = mapMaybeWithKey tmp g
  where
    tmp i (a, xs) = if i `elem` nodes
      then Just (a, filterWithKey (const . flip elem nodes) xs)
      else Nothing

-- | For every component of G, count how many groups of nodes they overlap with
--   and check if the predicate holds on the count. If it matches on any
--   component then return that component. We also try to return only the parts
--   of the component that are necessary for our predicate to hold.
anyConnections :: Ord i => (Int -> Bool) -> [[i]] -> ColoredGraph i a b -> Maybe [i]
anyConnections predicate groups = findComponent cond
  where
    cond z = predicate $ length $ filter (not . Prelude.null . intersect z) groups

-- | Is there a component along edges with value `dir` that has a length
--   accepted by `pred`? If there is we return a subset of that component that
--   matches the predicate
inARow :: (Ord i, Eq b) => (Int -> Bool) -> b -> ColoredGraph i a b -> Maybe [i]
inARow predicate dir = findComponent (predicate . length) . filterEdges (==dir)

-- | Try to find a component of the graph that matches the predicate.
--   The component that is returned is minimized using a greedy
--   search while still matching our predicate.
findComponent :: Ord i => ([i] -> Bool) -> ColoredGraph i a b -> Maybe [i]
findComponent predicate g = minimizeComponent <$> find predicate (components g)
  where
    -- Remove elements from xs while the condition holds.
    minimizeComponent xs = maybe xs minimizeComponent $ find cond $ oneRemoved xs
      where
        -- The condition we want to hold is our
        -- predicate and that we only have one component.
        cond z = predicate z && 1 == length (components $ inducedSubgraph g z)
        -- Lists where we have removed one element from the input.
        oneRemoved :: [i] -> [[i]]
        oneRemoved [] = []
        oneRemoved [_] = [[]]
        oneRemoved (x:xs) = xs : ((x:) <$> oneRemoved xs)

-- | Returns the winning sets representing paths from one set of nodes to
--   another on a graph.
winningSetPaths :: Ord i => ColoredGraph i a b -> [i] -> [i] -> [[i]]
winningSetPaths g is js = concat [foldTree (\(isLeaf, z) xs -> if isLeaf then [[z]] else concatMap (fmap (z:)) xs) $ winningSetPaths' g start i goal | i <- is]
  where
    allTrue = True <$ g
    start = foldr (`Map.insert` False) allTrue is

    allFalse = False <$ g
    goal = foldr (`Map.insert` True) allFalse js

-- | Returns a tree representing all paths from a starting node too any node in
--   the goal. The paths do not "touch" themselves and they only use a set of
--   allowed nodes. That they don't touch means that we generate exactly the
--   minimum set of winning sets that cover reaching from our starting node to
--   the goal.
winningSetPaths' :: Ord i => ColoredGraph i a b -> Map i Bool -> i -> Map i Bool -> Tree (Bool, i)
winningSetPaths' g allowed i goal = Node (False, i) $ (\k -> if fromJust $ Map.lookup k goal then Node (True, k) [] else winningSetPaths' g allowed' k goal) <$> neighbourIndices
  where
    neighbourIndices = filter (fromJust . flip Map.lookup allowed) $ Map.keys $ snd $ fromJust $ Map.lookup i g
    allowed' = foldr (`Map.insert` False) allowed neighbourIndices

-- | Returns all nodes avoiding all of the provided edge colors.
missingEdgeColors :: (Ord i, Eq b) => ColoredGraph i a b -> [b] -> [i]
missingEdgeColors g xs = Map.keys $ filterG (null . intersect xs . Map.elems . snd) g

-- | A synonym for `missingEdgeColors` for cases where the color of an edge
--   represents the direction of that edge.
missingDirections :: (Ord i, Eq b) => ColoredGraph i a b -> [b] -> [i]
missingDirections = missingEdgeColors

-- | Takes a path of vertices and returns a path of edges. Where the edges are
--   pairs of from and to vertices.
edgePath :: [a] -> [(a, a)]
edgePath a = zip a (tail a)

-- | A standard implementation of 'MyLib.positions' for games
--   with an underlying 'ColoredGraph' played on the vertices.
--
--   For 'ColoredGraph's, this is a synonym of 'values'.
coloredGraphVertexPositions :: (ColoredGraphTransformer i a b g, Ord i) => g -> [a]
coloredGraphVertexPositions = values . toColoredGraph

-- | A standard implementation of 'MyLib.getPosition' for games
--   with an underlying 'ColoredGraph' played on the vertices.
coloredGraphGetVertexPosition :: (ColoredGraphTransformer i a b g, Ord i) => g -> i -> Maybe a
coloredGraphGetVertexPosition g i = fst <$> Map.lookup i (toColoredGraph g)

-- | A standard implementation of 'MyLib.setPosition' for games
--   with an underlying 'ColoredGraph' played on the vertices.
coloredGraphSetVertexPosition :: (ColoredGraphTransformer i a b g, Ord i) => g -> i -> a -> Maybe g
coloredGraphSetVertexPosition g i p = if Map.member i c
    then Just $ fromColoredGraph g $ Map.adjust (\(_, xs) -> (p, xs)) i c
    else Nothing
  where
    c = toColoredGraph g

-- | A standard implementation of 'MyLib.positions' for games
--   with an underlying 'ColoredGraph' played on the edges.
coloredGraphEdgePositions :: (ColoredGraphTransformer i a b g, Ord i) => g -> [b]
coloredGraphEdgePositions = Map.elems . snd <=< Map.elems . toColoredGraph

-- | A standard implementation of 'MyLib.getPosition' for games
--   with an underlying 'ColoredGraph' played on the edges.
coloredGraphGetEdgePosition :: (ColoredGraphTransformer i a b g, Ord i) => g -> (i, i) -> Maybe b
coloredGraphGetEdgePosition g (from, to) = Map.lookup from (toColoredGraph g) >>= (Map.lookup to . snd)

-- | A standard implementation of 'MyLib.setPosition' for games
--   with an underlying 'ColoredGraph' played on the vertices.
coloredGraphSetEdgePosition :: (ColoredGraphTransformer i a b g, Ord i) => g -> (i, i) -> b -> Maybe g
coloredGraphSetEdgePosition g (from, to) p = Map.lookup from c >>=
    \(a, edges) -> if Map.member to edges
      then Just $ fromColoredGraph g $ Map.insert from (a, Map.insert to p edges) c
      else Nothing
  where
    c = toColoredGraph g

-- | Like 'coloredGraphSetEdgePosition' but sets the value to the edges in both
--   directions.
coloredGraphSetBidirectedEdgePosition :: (ColoredGraphTransformer i a b g, Ord i) => g -> (i, i) -> b -> Maybe g
coloredGraphSetBidirectedEdgePosition c (from, to) p = coloredGraphSetEdgePosition c (from, to) p >>=
  \c' -> coloredGraphSetEdgePosition c' (to, from) p

-- | A utility class for transforming to and from 'ColoredGraph'.
--
--   New-types of 'ColoredGraph' can derive this using the
--   'GeneralizedNewtypeDeriving' language extension.
class ColoredGraphTransformer i a b g | g -> i, g -> a, g -> b where
  -- | "Extracts" the 'ColoredGraph' from a container type.
  toColoredGraph :: g -> ColoredGraph i a b
  -- | "Inserts" the 'ColoredGraph' into an already existing container type.
  fromColoredGraph :: g -> ColoredGraph i a b -> g

instance ColoredGraphTransformer i a b (ColoredGraph i a b) where
  toColoredGraph c = c
  fromColoredGraph _ = id
