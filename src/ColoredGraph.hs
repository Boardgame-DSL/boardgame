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

testGraph :: ColoredGraph (Int, Int) (Maybe a) String
testGraph = Map.fromList
  [ ((-1, 0), (Nothing, [("ur",( 0,-1)), ("dr",( 0, 0)), ("d",(-1, 1))]))
  , (( 0,-1), (Nothing, [("dr",( 1,-1)), ("d",( 0, 0)), ("dl",(-1, 0))]))
  , ((-1, 1), (Nothing, [("u",(-1, 0)), ("ur",( 0, 0)), ("dr",( 0, 1))]))
  , (( 0, 0), (Nothing, [("u",( 0,-1)), ("ur",( 1,-1)), ("dr",( 1, 0)), ("d",( 0, 1)), ("dl",(-1, 1)), ("ul",(-1, 0))]))
  , (( 1,-1), (Nothing, [("d",( 1, 0)), ("dl",( 0, 0)), ("ul",( 0,-1))]))
  , (( 0, 1), (Nothing, [("u",( 0, 0)), ("ur",( 1, 0)), ("ul",(-1, 1))]))
  , (( 1, 0), (Nothing, [("u",( 1,-1)), ("dl",( 0, 1)), ("ul",( 0, 0))]))
  ]


firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

mapMaybeG :: Ord i => (a -> Maybe c) -> ColoredGraph i a b -> ColoredGraph i c b
mapMaybeG f g = Map.mapMaybe (\(a, xs) -> (, filter (\(_, k) -> isJust $ f $ fst $ fromJust $ Map.lookup k g) xs) <$> f a) g

filterG :: Ord i => (a -> Bool) -> ColoredGraph i a b -> ColoredGraph i a b
filterG pred = mapMaybeG (\z -> if pred z then Just z else Nothing)

mapValues :: Ord i => (a -> c) -> ColoredGraph i a b -> ColoredGraph i c b
mapValues f g = Map.map (\(a, xs) -> (f a, xs)) g

nodesPred :: (a -> [(b, i)] -> Bool) -> ColoredGraph i a b -> [i]
nodesPred pred g = fst <$> (filter (uncurry pred . snd) $ Map.toList g)


path :: Ord i => ColoredGraph i a b -> i -> i -> Maybe [(b, i)]
path = path' Set.empty

path' :: Ord i => Set i -> ColoredGraph i a b -> i -> i -> Maybe [(b, i)]
path' s g i j
  | i == j = Just []
  | otherwise = firstJust (\(d, k) -> ((d, k):) <$> path' s' g k j) $ filter (\(_, k) -> not $ k `Set.member` s') neighbours
  where
    (_, neighbours) = g Map.! i
    s' = Set.insert i s


componentsPred :: (Eq i, Ord i) => (a -> [(b, i)] -> Bool) -> ColoredGraph i a b -> [[i]]
componentsPred = componentsPred' []
  where
    componentsPred' :: (Eq i, Ord i) => [[i]] -> (a -> [(b, i)] -> Bool) -> ColoredGraph i a b -> [[i]]
    componentsPred' state pred g = case find (\k -> uncurry pred (fromJust $ Map.lookup k g) && all (not . elem k) state) (Map.keys g) of
      Just i -> componentsPred' (componentPred pred g i : state) pred g
      Nothing -> state



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





























