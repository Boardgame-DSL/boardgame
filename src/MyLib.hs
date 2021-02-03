module MyLib (someFunc) where

import Data.Maybe
import Data.List


data Player = Player1 | Player2
  deriving (Show, Eq)


-- Based on the definition from https://en.wikipedia.org/wiki/Positional_game
data PositionalGame = PositionalGame
  { pgX :: [String]
  , pgF :: [[String]] -- could also maybe be `[String] -> Bool` so you don't need to construct and store every subset
  , pgCriterion :: [String] -> [[String]] -> [Maybe Player] -> Maybe Player
  }


-- If a player controls one of the subsets they win.
strongPositionalGameCriterion :: [String] -> [[String]] -> [Maybe Player] -> Maybe Player
strongPositionalGameCriterion positions fs colorings = listToMaybe $ mapMaybe listColor fs
  where
    reduceHomogeneousList :: (Eq a) => [a] -> Maybe a
    reduceHomogeneousList (x:xs) = if all (== x) xs then Just x else Nothing

    -- Give the color of a subset if it is homogeneous.
    listColor :: [String] -> Maybe Player
    listColor ss = mapM color ss >>= reduceHomogeneousList

    -- Give the color of a position.
    -- We assume that the position exists in our list of positions
    color :: String -> Maybe Player
    color position = fromJust $ snd <$> find (\(z, _) -> z == position) (zip positions colorings)

ticTacToe = PositionalGame
  { pgX = ["00","01","02"
          ,"10","11","12"
          ,"20","21","22"
          ]
  , pgF = [["00","01","02"]
          ,["10","11","12"]
          ,["20","21","22"]

          ,["00","10","20"]
          ,["01","11","21"]
          ,["02","12","22"]

          ,["00","11","22"]
          ,["02","11","20"]
          ]
  , pgCriterion = strongPositionalGameCriterion
  }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
