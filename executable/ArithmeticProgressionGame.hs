{-# LANGUAGE MultiParamTypeClasses #-}

module ArithmeticProgressionGame where

import Data.List  (intercalate)

import Boardgame (
    Player(..)
  , Position(..)
  , PositionalGame(..)
  , patternMatchingGameOver
  )

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
      showP Empty              = "  _"
      showP (Occupied Player1) = "  \ESC[34mO\ESC[0m"
      showP (Occupied Player2) = "  \ESC[31mX\ESC[0m"
      pad x = replicate (3 - length x) ' ' ++ x

instance PositionalGame ArithmeticProgressionGame Int where
  positions   (ArithmeticProgressionGame _ l)     = l
  getPosition (ArithmeticProgressionGame _ l) i   = if i <= length l then Just $ l !! (i - 1) else Nothing
  setPosition (ArithmeticProgressionGame k l) i p = if i <= length l
    then Just $ ArithmeticProgressionGame k (take (i - 1) l ++ p : drop i l)
    else Nothing
  gameOver a@(ArithmeticProgressionGame k l) = let n = length l
    in patternMatchingGameOver (filter (all (<= n)) $ concat [[take k [i,i+j..] | j <- [1..n-i]] | i <- [1..n]]) a