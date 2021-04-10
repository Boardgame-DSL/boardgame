{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Hex where

import Data.List (intercalate)

import Data.Map (
    Map
  , lookup
  , adjust
  , member
  )

import Prelude hiding (lookup)
import Data.Maybe (fromJust)

#ifdef WASM
import Data.Aeson (
    ToJSON(..)
  , object
  , (.=)
  )
#endif

import Boardgame (
    Player(..)
  , Position(..)
  , PositionalGame(..)
  , takeEmptyMakeMove
  , criteria
  , makerBreakerGameOver
  , player1WinsWhen
  , player2WinsWhen
  )

import Boardgame.ColoredGraph (
    ColoredGraph
  , ColoredGraphTransformer(..)
  , paraHexGraph
  , values
  , anyConnections
  , filterValues
  , filterG
  , winningSetPaths
  , coloredGraphVertexPositions
  , coloredGraphGetVertexPosition
  , coloredGraphSetVertexPosition
  )

-------------------------------------------------------------------------------
-- * Hex
-------------------------------------------------------------------------------

data Hex = Hex Int (ColoredGraph (Int, Int) Position (Int, Int))

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
  showP Empty              = " "
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
          [ player1WinsWhen (anyConnections (==2) [left, right]) . filterValues (==Occupied Player1)
           -- There is a connection between 2 components, the top and bottom.
          , player2WinsWhen (anyConnections (==2) [top, bottom]) . filterValues (==Occupied Player2)
          ]
      left   = [(0,  i) | i <- [0..n-1]]
      right  = [(n-1,i) | i <- [0..n-1]]
      top    = [(i,  0) | i <- [0..n-1]]
      bottom = [(i,n-1) | i <- [0..n-1]]

emptyHex :: Int -> Hex
emptyHex n = Hex n $ paraHexGraph n

-------------------------------------------------------------------------------
-- * Hex2
-------------------------------------------------------------------------------

data Hex2 = Hex2 Int (ColoredGraph (Int, Int) Position (Int, Int))

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
gridShowLine2 (Hex2 n b) y = [rowOffset ++ tileTop ++ [x | y/=0, x <- " /"]
                          ,rowOffset ++ "| " ++ intercalate " | " (map (\x -> showP $ fst $ fromJust $ lookup (x, n-1-y) b) [0..(n-1)]) ++ " |"
                          ] where
  showP (Occupied Player1) = "1"
  showP (Occupied Player2) = "2"
  showP Empty           = " "
  rowOffset = replicate (2*(n-y-1)) ' '
  tileTop = concat $ replicate n " / \\"

instance PositionalGame Hex2 (Int, Int) where
  positions   (Hex2 n b)     = values b
  getPosition (Hex2 n b) c   = fst <$> lookup c b
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

emptyHex2 :: Int -> Hex2
emptyHex2 n = Hex2 n $ paraHexGraph n