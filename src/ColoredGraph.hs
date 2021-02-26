{-# LANGUAGE TupleSections #-}

module ColoredGraph where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Tree

-- a Graph with colored vertices and edges.
type ColoredGraph i a b = Map i (a, [(b, i)])

type HexCoordinate = (Int, Int)

axialDirections :: [HexCoordinate]
axialDirections =
  [ (1, 0)
  , (1, -1)
  , (0, -1)
  , (-1, 0)
  , (-1, 1)
  , (0, 1)
  ]


hexNeighbors :: HexCoordinate -> [HexCoordinate]
hexNeighbors (i, j) = (\(x, y) -> (i+x, j+y)) <$> axialDirections

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

binaryOp :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
binaryOp op (x, y) (z, w) = (op x z, op y w)


hexHexGraphRing :: Int -> [HexCoordinate]
hexHexGraphRing base = concat [oneSide k | k <- [0..5]]
  where
    oneSide :: Int -> [HexCoordinate]
    oneSide i = [binaryOp (\z w -> base*z + k*w) (axialDirections !! i) (axialDirections !! ((i+2) `mod` 6)) | k <- [1..base]]

distance :: HexCoordinate -> HexCoordinate -> Int
distance (x, y) (i, j) = (abs(x - i) + abs(x + y - i - j) + abs(y - j)) `div` 2

hexHexGraph :: Int -> ColoredGraph HexCoordinate (Maybe a) HexCoordinate
hexHexGraph radius = Map.fromList ((\z -> (z , (Nothing, filter ((< radius) . distance (0, 0) . snd) $ (\i -> (axialDirections !! i, hexNeighbors z !! i )) <$> [0..5]))) <$> nodes)
  where
    nodes :: [HexCoordinate]
    nodes = (0, 0) : concatMap hexHexGraphRing [1..radius-1]



paraHexGraph :: Int -> ColoredGraph HexCoordinate (Maybe a) HexCoordinate
paraHexGraph n = Map.fromList ((\z -> (z , (Nothing, filter ((\(i, j) -> i < n && i >= 0 && j < n && j >= 0) . snd) $ (\i -> (axialDirections !! i, hexNeighbors z !! i )) <$> [0..5]))) <$> nodes)
  where
    nodes :: [HexCoordinate]
    nodes = [(i, j) | i <- [0..n-1], j <- [0..n-1]]









firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

mapMaybeG :: Ord i => (a -> Maybe c) -> ColoredGraph i a b -> ColoredGraph i c b
mapMaybeG f g = Map.mapMaybe (\(a, xs) -> (, filter (\(_, k) -> isJust $ f $ fst (fromJust (Map.lookup k g))) xs) <$> f a) g

filterG :: Ord i => (a -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterG pred = mapMaybeG (\z -> if pred z then Just z else Nothing)

mapValues :: Ord i => (a -> c) -> ColoredGraph i a b -> ColoredGraph i c b
mapValues f = Map.map (\(a, xs) -> (f a, xs))

nodesPred :: (a -> [(b, i)] -> Bool) -> ColoredGraph i a b -> [i]
nodesPred pred g = fst <$> filter (uncurry pred . snd) (Map.toList g)


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
componentsPred :: (Eq i, Ord i) => (a -> [(b, i)] -> Bool) -> ColoredGraph i a b -> [[i]]
componentsPred = componentsPred' []
  where
    componentsPred' :: (Eq i, Ord i) => [[i]] -> (a -> [(b, i)] -> Bool) -> ColoredGraph i a b -> [[i]]
    componentsPred' state pred g = case find (\k -> uncurry pred (fromJust $ Map.lookup k g) && all (notElem k) state) (Map.keys g) of
      Just i -> componentsPred' (componentPred pred g i : state) pred g
      Nothing -> state


-- List all the connected nodes starting from one node, also known as a connected component.
componentPred :: Ord i => (a -> [(b, i)] -> Bool) -> ColoredGraph i a b -> i -> [i]
componentPred pred g = fst . componentPred' Set.empty pred g
  where
    componentPred' :: Ord i => Set i -> (a -> [(b, i)] -> Bool) -> ColoredGraph i a b -> i -> ([i], Set i)
    componentPred' inputState pred g i = if uncurry pred (fromJust $ Map.lookup i g)
      then (i : xs, newState)
      else ([], inputState)
      where
        (_, neighbours) = g Map.! i
        (xs, newState) = foldl tmp ([], Set.insert i inputState) (snd <$> neighbours)

        tmp (ks, state) k
          | k `Set.member` state = (ks, state)
          | otherwise = let (x, y) = componentPred' state pred g k in (ks ++ x, y)

values :: ColoredGraph i a b -> [a]
values g = fst <$> Map.elems g

-- For every component of G, count how many groups of nodes they overlap with and check if the predicate holds on the count.
-- If it matches on any component then return true. Otherwise return false.
anyConnections :: Ord i => (Int -> Bool) -> [[i]] -> ColoredGraph i Bool b -> Bool
anyConnections pred groups g = any (\z -> pred $ length (filter (not . Prelude.null . intersect z) groups)) $ componentsPred const g




























