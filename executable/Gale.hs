{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gale where

import Data.Graph as Graph (Graph, buildG, path)
import Data.List (intercalate)
import Data.Maybe (fromJust, mapMaybe)

import Boardgame (
    Player(..)
  , Position(..)
  , PositionalGame(..)
  )

import ShannonSwitchingGame
#ifdef WASM
import Data.Aeson (ToJSON(..))
#endif

-------------------------------------------------------------------------------
-- * Gale
-------------------------------------------------------------------------------

newtype Gale = Gale ShannonSwitchingGameCG

emptyGale :: Gale
emptyGale = Gale $ createEmptyShannonSwitchingGameCG connections (-1) (-2)
  where
    connections = mapMaybe galeCoordinatesToId [(x, y) | x <- [0..8], y <- [0..8]]

galeCoordinatesToId :: (Int, Int) -> Maybe (Int, Int)
galeCoordinatesToId (x, y) | even x && even y = Just (if x == 0 then -1 else x + y * 4 - 2, if x == 8 then -2 else x + y * 4)
                           | odd x && odd y   = Just (x + (y - 1) * 4 - 1, x + (y - 1) * 4 + 7)
                           | otherwise        = Nothing

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
  show g = intercalate "\n" [
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
      row y = mapMaybe (\x -> flip showP y <$> getPosition g (x, y)) [0..8]
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

instance PositionalGame Gale (Int, Int) where
  getPosition (Gale b) coords = galeCoordinatesToId coords >>= getPosition b
  positions (Gale b) = positions b
  setPosition (Gale b) coords p = Gale <$> (galeCoordinatesToId coords >>= flip (setPosition b) p)
  gameOver (Gale b) = gameOver b
