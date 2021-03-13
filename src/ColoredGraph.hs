{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module ColoredGraph (
    ColoredGraph
  , ColoredGraphTransformer(..)
  , hexHexGraph
  , paraHexGraph
  , rectOctGraph
  , mapValues
  , mapEdges
  , filterValues
  , filterEdges
  , filterG
  , components
  , anyConnections
  , inARow
  , values
  , coloredGraphSetVertexPosition
  , coloredGraphGetVertexPosition
  , coloredGraphEdgePositions
  , coloredGraphGetEdgePosition
  , coloredGraphSetEdgePosition
  , coloredGraphSetBidirectedEdgePosition
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List ( find, intersect )
import Data.Maybe ( fromJust, isJust, listToMaybe, mapMaybe )
import Data.Tree ()
import Data.Bifunctor ( bimap, Bifunctor (first, second) )


-- | A Graph with colored vertices and edges. The key of the map is 'i', the
--   "coordinates". The value of the map is a tuple of vertices color 'a', and
--   a list of edges. The edges are tuples of edge color 'b' and
--   "target coordinate" 'i'.
type ColoredGraph i a b = Map i (a, Map i b)

type Coordinate = (Int, Int)

-- The six directions of neighbours on a hexagonal grid.
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

-- The eight directions of neighbours on a square grid.
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




-- Maps over the individual values of a tuple.
mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

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
hexHexGraph :: Int -> ColoredGraph (Int, Int) (Maybe a) (Int, Int)
hexHexGraph radius = Map.fromList ((\z -> (z , (Nothing, Map.fromList $ filter ((< radius) . distance (0, 0) . fst) $ (\i -> (hexNeighbors z !! i, hexDirections !! i)) <$> [0..5]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = (0, 0) : concatMap hexHexGraphRing [1..radius-1]





-- | Creates a parallelogram shaped graph of hexagon vertices (each vertex has
--   six outgoing edges) with the given side length.
--
--   The "coordinates" of the graph will be '(Int, Int)' where '(0, 0)' is at
--   the center. The color of edges will also be a '(Int, Int)' tuple that
--   shows the "direction" of the edge.
paraHexGraph :: Int -> ColoredGraph (Int, Int) (Maybe a) (Int, Int)
paraHexGraph n = Map.fromList ((\z -> (z , (Nothing, Map.fromList $ filter ((\(i, j) -> i < n && i >= 0 && j < n && j >= 0) . fst) $ (\i -> (hexNeighbors z !! i, hexDirections !! i)) <$> [0..5]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = [(i, j) | i <- [0..n-1], j <- [0..n-1]]

-- | Creates a rectangular shaped graph of octagon vertices (each vertex has
--   eight outgoing edges) with the given width and height.
--
--   The "coordinates" of the graph will be '(Int, Int)' where '(0, 0)' the top
--   left vertex. The color of edges will also be a '(Int, Int)' tuple that
--   shows the "direction" of the edge.
rectOctGraph :: Int -> Int -> ColoredGraph (Int, Int) (Maybe a) (Int, Int)
rectOctGraph m n = Map.fromList ((\z -> (z , (Nothing, Map.fromList $ filter ((\(i, j) -> i < m && i >= 0 && j < n && j >= 0) . snd) $ (\i -> (octoNeighbors z !! i, octoDirections !! i)) <$> [0..7]))) <$> nodes)
  where
    nodes :: [Coordinate]
    nodes = [(i, j) | i <- [0..m-1], j <- [0..n-1]]






-- Returns the first value that is accepted by the predicate, or 'Nothing'.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

-- Maps the vertices, and their outgoing edges with values, and collects the
-- 'Just' results.
mapMaybeG :: Ord i => ((a, Map i b) -> Maybe c) -> ColoredGraph i a b -> ColoredGraph i c b
mapMaybeG f g = Map.map (second (Map.filterWithKey (\k _ -> Map.member k g'))) g'
  where
    g' = Map.mapMaybe (\(a, xs) -> (, xs) <$> f (a, xs)) g

-- | Filters out any vertices whose value, and their outgoing edges with
--   values, is not accepted by the predicate.
filterG :: Ord i => ((a, Map i b) -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterG pred = mapMaybeG (\(z, w) -> if pred (z, w) then Just z else Nothing)

-- | Filters out any vertices whose value is not accepted by the predicate.
filterValues :: Ord i => (a -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterValues pred = filterG $ pred . fst

-- | Maps the values of vertices with the given function.
mapValues :: Ord i => (a -> c) -> ColoredGraph i a b -> ColoredGraph i c b
mapValues = fmap . first

-- | Maps the values of edges with the given function.
mapEdges :: Ord i => (b -> c) -> ColoredGraph i a b -> ColoredGraph i a c
mapEdges = fmap . second . fmap

-- Returns a list of "coordinates" for vertices whose value, and their outgoing
-- edges with values, are accepted by the predicate.
nodesPred :: (a -> Map i b -> Bool) -> ColoredGraph i a b -> [i]
nodesPred pred g = fst <$> filter (uncurry pred . snd) (Map.toList g)

-- | Filters out any edges whose value is not accepted by the predicate.
filterEdges :: (b -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterEdges pred = Map.map $ second $ Map.filter pred

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
    component' inputState  g i = (i : xs, newState)
      where
        neighbours = Map.assocs $ snd $ g Map.! i
        (xs, newState) = foldl tmp ([], Set.insert i inputState) (fst <$> neighbours)

        tmp (ks, state) k
          | k `Set.member` state = (ks, state)
          | otherwise = let (x, y) = component' state  g k in (ks ++ x, y)

-- | Returns a list of vertex values from the given graph.
values :: ColoredGraph i a b -> [a]
values = fmap fst . Map.elems

-- | For every component of G, count how many groups of nodes they overlap with
--   and check if the predicate holds on the count. If it matches on any
--   component then return true. Otherwise return false.
anyConnections :: Ord i => (Int -> Bool) -> [[i]] -> ColoredGraph i a b -> Bool
anyConnections pred groups g = any (\z -> pred $ length $ filter (not . Prelude.null . intersect z) groups) $ components g


-- | Is there a component along edges with value `dir` that has a length
--   accepted by `pred`.
inARow :: (Ord i, Eq b) => (Int -> Bool) -> b -> ColoredGraph i a b -> Bool
inARow pred dir = any (pred . length) . components . filterEdges (==dir)

-- | A standard implementation of 'MyLib.getPosition' for games
--   with an underlying 'ColoredGraph' played on the vertices.
coloredGraphGetVertexPosition :: Ord i => ColoredGraph i a b -> i -> Maybe a
coloredGraphGetVertexPosition c i = fst <$> Map.lookup i c

-- | A standard implementation of 'MyLib.setPosition' for games
--   with an underlying 'ColoredGraph' played on the vertices.
coloredGraphSetVertexPosition :: Ord i => (ColoredGraph i a b -> c) -> ColoredGraph i a b -> i -> a -> Maybe c
coloredGraphSetVertexPosition constructor c i p = if Map.member i c
    then Just $ constructor $ Map.adjust (\(_, xs) -> (p, xs)) i c
    else Nothing

-- | A standard implementation of 'MyLib.positions' for games
--   with an underlying 'ColoredGraph' played on the edges.
coloredGraphEdgePositions :: Ord i => ColoredGraph i a b -> [b]
coloredGraphEdgePositions c = Map.elems c >>= (Map.elems . snd)

-- | A standard implementation of 'MyLib.getPosition' for games
--   with an underlying 'ColoredGraph' played on the edges.
coloredGraphGetEdgePosition :: Ord i => ColoredGraph i a b -> (i, i) -> Maybe b
coloredGraphGetEdgePosition c (from, to) = Map.lookup from c >>= (Map.lookup to . snd)

-- | A standard implementation of 'MyLib.setPosition' for games
--   with an underlying 'ColoredGraph' played on the vertices.
coloredGraphSetEdgePosition :: Ord i => (ColoredGraph i a b -> c) -> ColoredGraph i a b -> (i, i) -> b -> Maybe c
coloredGraphSetEdgePosition constructor c (from, to) p = Map.lookup from c >>=
  \(a, edges) -> if Map.member to edges
    then Just $ constructor $ Map.insert from (a, Map.insert to p edges) c
    else Nothing

-- | Like 'coloredGraphSetEdgePosition' but sets the value to the edges in both
--   directions.
coloredGraphSetBidirectedEdgePosition :: Ord i => (ColoredGraph i a b -> c) -> ColoredGraph i a b -> (i, i) -> b -> Maybe c
coloredGraphSetBidirectedEdgePosition constructor c (from, to) p = Map.lookup from c >>=
  (\(a, edges) -> if Map.member to edges
    then Just $ Map.insert from (a, Map.insert to p edges) c
    else Nothing) >>=
  \c' -> Map.lookup to c' >>=
    \(a, edges) -> if Map.member from edges
      then Just $ constructor $ Map.insert to (a, Map.insert from p edges) c'
      else Nothing

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
























