{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.Graph as Graph (Graph, buildG, path, vertices, indegree, scc, edges)
import Data.List (
    elemIndex
  , find
  , findIndex
  , intersect
  , nub
  , intercalate
  , partition
  , subsequences
  )
import Data.Map (
    Map
  , assocs
  , elems
  , keys
  , fromDistinctAscList
  , fromList
  , insert
  , lookup
  , member
  , (!)
  , adjust
  )
import Data.Maybe (fromJust, isJust, fromMaybe)
import MyLib (
    Player(..)
  , PositionalGame(..)
  , patternMatchingGameOver
  , playIO
  , takeEmptyMakeMove
  , nextPlayer
  , drawIf
  , player1WinsIf
  , player2WinsIf
  , criteria
  , symmetric
  , player1LosesIf
  , unless
  )
import System.IO (hFlush, stdout)
import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Data.Tuple (swap)
import qualified Data.Array ((!))
import Data.Foldable (toList)

#ifdef WASM
import qualified Data.Vector as V ((!), fromList)
import Data.Aeson
import Data.Aeson.Types
import MyLib.Web (webDefaultMain)
#endif

import Math.Geometry.Grid as Grid ()
import Math.Geometry.Grid.Hexagonal ()
import ColoredGraph (
    ColoredGraph
  , paraHexGraph
  , values
  , anyConnections
  , mapValues
  , filterValues
  , filterG
  , components
  , hexHexGraph
  , mapEdges
  , rectOctGraph
  , coloredGraphSetPosition
  , coloredGraphGetPosition
  , inARow)
-------------------------------------------------------------------------------
-- * TicTacToe
-------------------------------------------------------------------------------

newtype TicTacToe = TicTacToe (Map (Integer, Integer) (Maybe Player))

-- Creates an empty TicTacToe board with coordinates `(0..2, 0..2)`
emptyTicTacToe :: TicTacToe
emptyTicTacToe = TicTacToe $
  fromDistinctAscList $
    zip
      [(x, y) | x <- [0..2], y <- [0..2]]
      (repeat Nothing)

instance Show TicTacToe where
  show (TicTacToe b) = intercalate "\n" [
      "╔═══╤═══╤═══╗"
    , "║ " ++ intercalate " │ " (row 0) ++ " ║"
    , "╟───┼───┼───╢"
    , "║ " ++ intercalate " │ " (row 1) ++ " ║"
    , "╟───┼───┼───╢"
    , "║ " ++ intercalate " │ " (row 2) ++ " ║"
    , "╚═══╧═══╧═══╝"
    ]
    where
      -- "Shows" the elements of the given row
      row y = map (\x -> showP $ b ! (x, y)) [0..2]
      showP (Just Player1) = "\ESC[34mo\ESC[0m"
      showP (Just Player2) = "\ESC[31mx\ESC[0m"
      showP Nothing = " "

#ifdef WASM
-- Converts the game to a JSON array with three arrays with three integers
-- each. The integers correspond to
-- 0 → Nothing,
-- 1 → Just Player1, and
-- 2 → Just Player2.
instance ToJSON TicTacToe where
  toJSON (TicTacToe b) = Array $ V.fromList $ map row [0..2]
    where
      row y = Array $ V.fromList $ map (\x -> toJSONP $ b ! (x, y)) [0..2]
      toJSONP (Just Player1) = toJSON (1 :: Int)
      toJSONP (Just Player2) = toJSON (2 :: Int)
      toJSONP Nothing = toJSON (0 :: Int)
#endif

instance PositionalGame TicTacToe (Integer, Integer) where
  -- Just looks up the coordinate in the underlying Map
  getPosition (TicTacToe b) = flip lookup b
  -- Just returns the elements in the underlying Map
  positions (TicTacToe b) = elems b
  -- If the underlying Map has the given coordinate, update it with the given player
  setPosition (TicTacToe b) c p = if member c b then Just $ TicTacToe $ insert c (Just p) b else Nothing
  -- "Creates" a `gameOver` function by supplying all the winning "patterns"
  gameOver = patternMatchingGameOver [
      [(0, 0), (0, 1), (0, 2)]
    , [(1, 0), (1, 1), (1, 2)]
    , [(2, 0), (2, 1), (2, 2)]
    , [(0, 0), (1, 0), (2, 0)]
    , [(0, 1), (1, 1), (2, 1)]
    , [(0, 2), (1, 2), (2, 2)]
    , [(0, 0), (1, 1), (2, 2)]
    , [(2, 0), (1, 1), (0, 2)]
    ]

-------------------------------------------------------------------------------
-- * Arithmetic Progression Game
-------------------------------------------------------------------------------

data ArithmeticProgressionGame = ArithmeticProgressionGame Int [Maybe Player]

createArithmeticProgressionGame :: Int -> Int -> Maybe ArithmeticProgressionGame
createArithmeticProgressionGame n k = if k < n
  then Just $ ArithmeticProgressionGame k (replicate n Nothing)
  else Nothing

instance Show ArithmeticProgressionGame where
  show (ArithmeticProgressionGame _ ps) = (\(is, ps) -> intercalate "," is ++ "\n" ++ intercalate "," ps) $
        unzip $ zipWith (\i p -> (pad $ show i, pad $ showP p)) [1..] ps
    where
      showP Nothing        = "  _"
      showP (Just Player1) = "  \ESC[34mO\ESC[0m"
      showP (Just Player2) = "  \ESC[31mX\ESC[0m"
      pad x = replicate (3 - length x) ' ' ++ x

instance PositionalGame ArithmeticProgressionGame Int where
  getPosition (ArithmeticProgressionGame _ l) i = if i <= length l then Just $ l !! (i - 1) else Nothing
  positions (ArithmeticProgressionGame _ l) = l
  setPosition (ArithmeticProgressionGame k l) i p = if i <= length l
    then Just $ ArithmeticProgressionGame k (take (i - 1) l ++ Just p : drop i l)
    else Nothing
  gameOver a@(ArithmeticProgressionGame k l) = let n = length l
    in patternMatchingGameOver (filter (all (<= n)) $ concat [[take k [i,i+j..] | j <- [1..n-i]] | i <- [1..n]]) a

-------------------------------------------------------------------------------
-- * Shannon Switching Game
-------------------------------------------------------------------------------

newtype ShannonSwitchingGame = ShannonSwitchingGame (Int, [((Int, Int), Maybe Player)])

-- | Creates a list of all edges, input n gives n*n graph
gridEdges :: Int -> [((Int, Int), Maybe Player)]
gridEdges n =
  concat [[((j+i*n,(j+1)+i*n), Nothing), ((j+i*n,j+(i+1)*n), Nothing)] | i <- [0..n-2], j <- [0..n-2]] ++
  [(((n-1)+i*n, (n-1)+(i+1)*n), Nothing) | i <- [0..n-2]] ++
  [((i+(n-1)*n, (i+1)+(n-1)*n), Nothing) | i <- [0..n-2]]

createShannonSwitchingGame :: Int -> ShannonSwitchingGame
createShannonSwitchingGame n = ShannonSwitchingGame (n, gridEdges n)

-- o───o---o
-- ║   │   :
-- o═══o═══o
-- ║   │   :
-- o───o───o
instance Show ShannonSwitchingGame where
  show a@(ShannonSwitchingGame (n, l)) = intercalate "\n" ([concat ["o" ++ showH (fromJust $ getPosition a (i+j*n, (i+1)+j*n)) | i <- [0 .. n - 2]]
    ++ "o\n" ++ concat [showV (fromJust $ getPosition a (i+j*n, i+(j+1)*n)) ++ "   " | i <- [0 .. n - 2]] ++ showV (fromJust $ getPosition a ((n-1)+j*n, (n-1)+(j+1)*n) ) |
    j <- [0 .. n - 2]]
    ++ [concat ["o" ++ showH (fromJust $ getPosition a (i+(n-1)*n, (i+1)+(n-1)*n)) | i <- [0 .. n - 2]] ++ "o"])
    where
      showH (Just Player1) = "\ESC[34m───\ESC[0m"
      showH (Just Player2) = "\ESC[31m───\ESC[0m"
      showH Nothing        = "───"
      showV (Just Player1) = "\ESC[34m│\ESC[0m"
      showV (Just Player2) = "\ESC[31m│\ESC[0m"
      showV Nothing        = "│"

instance PositionalGame ShannonSwitchingGame (Int, Int) where
  getPosition (ShannonSwitchingGame (_, l)) c = snd <$> find ((== c) . fst) l
  positions (ShannonSwitchingGame (_, l)) = map snd l
  setPosition (ShannonSwitchingGame (n, l)) c p = case findIndex ((== c) . fst) l of
    Just i -> Just $ ShannonSwitchingGame (n, take i l ++ (c, Just p) : drop (i + 1) l)
    Nothing -> Nothing
  gameOver (ShannonSwitchingGame (n, l))
    | path g 0 (n * n - 1) = Just (Just Player1)
    | path g (n - 1) (n * n - n) = Just (Just Player2)
    | all (isJust . snd) l = Just Nothing
    | otherwise = Nothing
    where
      g = buildG (0, n * n - 1) (map fst $ filter ((== Just Player1) . snd) l)

-------------------------------------------------------------------------------
-- * Gale
-------------------------------------------------------------------------------

newtype Gale = Gale (Map (Integer, Integer) (Maybe Player))

-- Creates an empty Gale playfield. Even "rows" have 4 "columns" and odd ones
-- have 3.
emptyGale :: Gale
emptyGale = Gale $
  fromList $
    zip
      ([0..8] >>= (\y -> [(x, y) | x <- [0..(4 - y `rem` 2)]]))
      (repeat Nothing)

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
  show (Gale b) = intercalate "\n" [
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
      row y = map (\x -> showP (b ! (x, y)) y) [0..(4 - y `rem` 2)]
      showP (Just Player1) y
        | even y      = "\ESC[34m───"
        | otherwise   = " \ESC[34m│ "
      showP (Just Player2) y
        | even y      = " \ESC[31m║ "
        | otherwise   = "\ESC[31m═══"
      showP Nothing _ = "   "

instance PositionalGame Gale (Integer, Integer) where
  getPosition (Gale b) (x, y) = if x `rem` 2 == y `rem` 2 then lookup c b else Nothing
    where c = (x `div` 2, y)
  positions (Gale b) = elems b
  setPosition (Gale b) (x, y) p = if x `rem` 2 == y `rem` 2 && member c b then Just $ Gale $ insert c (Just p) b else Nothing
    where c = (x `div` 2, y)
  gameOver (Gale b)
    | all isJust (elems b) = Just Nothing
    | path player1Graph (-1) (-2) = Just $ Just Player1
    | path player2Graph (-1) (-2) = Just $ Just Player2
    | otherwise            = Nothing
    where
      playerGraph from to p = buildG (-2, 19) $
        filter ((Just p ==) . snd) (assocs b) >>=
          ((\(x, y) -> [(x, y), (y, x)]) . (\((x, y), _) -> (fromInteger $ from x y, fromInteger $ to x y)))
      player1Graph = playerGraph
        (\x y -> if x == 0 then -1 else (y `div` 2) + 5 * (x + (y `rem` 2) - 1))
        (\x y -> if x == 4 then -2 else (y `div` 2) + (y `rem` 2) + 5 * x)
        Player1
      player2Graph = playerGraph
        (\x y -> if y == 0 then -1 else x + 5 * ((y `div` 2) + (y `rem` 2) - 1))
        (\x y -> if y == 8 then -2 else x + (y `rem` 2) + 5 * (y `div` 2))
        Player2

-------------------------------------------------------------------------------
-- * Hex
-------------------------------------------------------------------------------

hexSize :: Int
hexSize = 5

data Hex = Hex Int (ColoredGraph (Int, Int) (Maybe Player) (Int, Int))

emptyHex :: Int -> Hex
emptyHex n = Hex n $ paraHexGraph n

instance Show Hex where
  show (Hex n b) =
    replicate (2*(hexSize-1)) ' ' ++ concat (replicate hexSize "  _ ") ++ "\n"
    ++
    intercalate "\n" [intercalate "\n" (gridShowLine (Hex n b) r) | r <- [0..hexSize-1]]
    ++
    "\n" ++ concat (replicate hexSize " \\_/")

#ifdef WASM
instance ToJSON Hex where
  toJSON (Hex _ m) = toJSON m
#endif

gridShowLine :: Hex -> Int -> [String]
gridShowLine (Hex n b) y  = [rowOffset ++ tileTop ++ [x | y/=0, x <- " /"]
                          ,rowOffset ++ "| " ++ intercalate " | " (map (\x -> showP $ fst $ fromJust $ lookup (x, hexSize-1-y) b) [0..(hexSize-1)]) ++ " |"
                          ] where
  showP (Just Player1) = "1"
  showP (Just Player2) = "2"
  showP Nothing = " "
  rowOffset = replicate (2*(hexSize-y-1)) ' '
  tileTop = concat $ replicate hexSize " / \\"

instance PositionalGame Hex (Int, Int) where
  getPosition (Hex n b) = coloredGraphGetPosition b
  positions (Hex n b) = values b
  setPosition (Hex n b) = coloredGraphSetPosition (Hex n) b
  makeMove = takeEmptyMakeMove
  gameOver (Hex n b) = criterion b
    where
      criterion =
        criteria
          -- There is a connection between 2 components, the left and right.
          [ player1WinsIf (anyConnections (==2) [left, right]) . filterValues (==Just Player1)
           -- There is a connection between 2 components, the top and bottom.
          , player2WinsIf (anyConnections (==2) [top, bottom]) . filterValues (==Just Player2)
          ]
      left   = [(0,  i) | i <- [0..n-1]]
      right  = [(n-1,i) | i <- [0..n-1]]
      top    = [(i,  0) | i <- [0..n-1]]
      bottom = [(i,n-1) | i <- [0..n-1]]

-------------------------------------------------------------------------------
-- * Havannah
-------------------------------------------------------------------------------

newtype Havannah = Havannah (ColoredGraph (Int, Int) (Maybe Player) ())

instance Show Havannah where
  show (Havannah b) = show b

instance PositionalGame Havannah (Int, Int) where
  getPosition (Havannah b) = coloredGraphGetPosition b
  positions (Havannah b) = values b
  setPosition (Havannah b) = coloredGraphSetPosition Havannah b
  makeMove = takeEmptyMakeMove

  gameOver (Havannah b) = criterion b
    where
      criterion =
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues (nextPlayer <$>)) $
        drawIf (all isJust . values) `unless` -- It's a draw if all tiles are owned.
        criteria (player1WinsIf <$> -- Player1 wins if any of these 3 criteria are satisfied.
            -- Player1 has connected 2 corners.
          [ anyConnections (>=2) corners . filterValues (== Just Player1)
            -- player1 has connecteed 3 edges (excluding the corners).
          , anyConnections (>=3) edges . filterValues (== Just Player1)
            -- player1 has surrounded other tiles such that they can't reach the border.
          , anyConnections (==0) border . filterValues (/= Just Player1)
          ])
      corners = components $ filterG ((==3) . length . snd) b
      edges   = components $ filterG ((==4) . length . snd) b
      border = corners ++ edges

emptyHavannah :: Int -> Havannah
emptyHavannah = Havannah . mapEdges (const ()) . hexHexGraph

-------------------------------------------------------------------------------
-- * Yavalath
-------------------------------------------------------------------------------

newtype Yavalath = Yavalath (ColoredGraph (Int, Int) (Maybe Player) String)

instance Show Yavalath where
  show (Yavalath b) = show b

instance PositionalGame Yavalath (Int, Int) where
  getPosition (Yavalath b) = coloredGraphGetPosition b
  positions (Yavalath b) = values b
  setPosition (Yavalath b) = coloredGraphSetPosition Yavalath b
  makeMove = takeEmptyMakeMove

  gameOver (Yavalath b) = criterion b
    where
      criterion =
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ fmap nextPlayer) $
        drawIf (all isJust . values) `unless` -- It's a draw if all tiles are owned.
        -- Player1 looses if he has 3 in a row but wins if he has 4 or more in a row.
        -- It's important we use `unless` here because otherwise we could have conflicting
        -- outcomes from having both 3 in a row and 4 in a row at the same time.
        criteria (player1LosesIf . inARow (==3) <$> directions) . filterValues (== Just Player1) `unless`
        criteria (player1WinsIf . inARow (>=4) <$> directions) . filterValues (== Just Player1)

      directions = ["vertical", "diagonal1", "diagonal2"]



emptyYavalath :: Int -> Yavalath
emptyYavalath = Yavalath . mapEdges dirName . hexHexGraph
  where
    dirName (1,0) = "vertical"
    dirName (-1,0) = "vertical"
    dirName (1,-1) = "diagonal1"
    dirName (-1,1) = "diagonal1"
    dirName (0,-1) = "diagonal2"
    dirName (0,1) = "diagonal2"

-------------------------------------------------------------------------------
-- * mnk-game
-------------------------------------------------------------------------------

data MNKGame = MNKGame Int (ColoredGraph (Int, Int) (Maybe Player) String)

instance Show MNKGame where
  show (MNKGame k b) = show b

instance PositionalGame MNKGame (Int, Int) where
  getPosition (MNKGame k b) = coloredGraphGetPosition b
  positions (MNKGame k b) = values b
  setPosition (MNKGame k b) = coloredGraphSetPosition (MNKGame k) b
  makeMove = takeEmptyMakeMove

  gameOver (MNKGame k b) = criterion b
    where
      criterion =
        -- Here we say that in any position where one player wins,
        -- the other player would win instead if the pieces were swapped.
        symmetric (mapValues $ fmap nextPlayer) $
        drawIf (all isJust . values) `unless` -- It's a draw if all tiles are owned.
        -- Player1 wins if there are k or more pieces in a row in any direction.
        criteria (player1WinsIf . inARow (>=k) <$> directions)
          

      directions = ["vertical", "horizontal", "diagonal1", "diagonal2"]



emptyMNKGame :: Int -> Int -> Int -> MNKGame
emptyMNKGame m n k = MNKGame k $ mapEdges dirName $ rectOctGraph m n
  where
    dirName (1,0) = "horizontal"
    dirName (-1,0) = "horizontal"
    dirName (0,-1) = "vertical"
    dirName (0,1) = "vertical"
    dirName (1,-1) = "diagonal1"
    dirName (-1,1) = "diagonal1"
    dirName (1,1) = "diagonal2"
    dirName (-1,-1) = "diagonal2"

-------------------------------------------------------------------------------
-- * CLI interactions
-------------------------------------------------------------------------------

#ifdef WASM
main :: IO ()
main = webDefaultMain $ emptyHex 5
#else
main :: IO ()
main = do
  putStrLn "1: TicTacToe"
  putStrLn "2: Arithmetic Progression Game"
  putStrLn "3: Shannon Switching Game"
  putStrLn "4: Gale"
  putStrLn "5: Hex"
  putStrLn "6: Havannah"
  putStrLn "7: Yavalath"
  putStr "What do you want to play? "
  hFlush stdout
  choice <- read <$> getLine
  putStr "\ESC[2J"
  hFlush stdout
  case choice of
    1 -> playIO emptyTicTacToe
    2 -> playAPG
    3 -> playIO $ createShannonSwitchingGame 5
    4 -> playIO emptyGale
    5 -> playIO $ emptyHex 5
    6 -> playIO $ emptyHavannah 8
    7 -> playIO $ emptyYavalath 2
    _ -> putStrLn "Invalid choice!"

playAPG :: IO ()
playAPG = do
  putStr "n: "
  hFlush stdout
  n <- read <$> getLine
  putStr "k: "
  hFlush stdout
  k <- read <$> getLine
  case createArithmeticProgressionGame n k of
    Just a -> playIO a
    Nothing -> putStrLn "Not valid input (n < k)"
#endif
