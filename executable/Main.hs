{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  )
import Data.Maybe (fromJust, isJust, fromMaybe)
import MyLib (
    Player(..)
  , PositionalGame(..)
  , patternMatchingGameOver
  , player
  , takeEmptyMakeMove
  , nextPlayer
  )
import System.IO (hFlush, stdout)
import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Data.Tuple (swap)
import qualified Data.Array ((!))
import Data.Foldable (toList)

import Math.Geometry.Grid as Grid
import Math.Geometry.Grid.Hexagonal
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

instance PositionalGame TicTacToe (Integer, Integer) where
  -- Just looks up the coordinate in the underlying Map
  getPosition (TicTacToe b) = flip lookup b
  -- Just returns the elements in the underlying Map
  positions (TicTacToe b) = elems b
  -- If the underlying Map has the given coordinate, update it with the given player
  setPosition (TicTacToe b) c p = if member c b then Just $ TicTacToe $ insert c (Just p) b else Nothing
  -- Just uses the "standard" implementation
  makeMove = takeEmptyMakeMove
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
  makeMove = takeEmptyMakeMove
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
  makeMove = takeEmptyMakeMove
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
  makeMove = takeEmptyMakeMove
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

newtype Hex = Hex (Map (Int, Int) (Maybe Player))

emptyHex :: Hex
emptyHex = Hex $
  fromDistinctAscList $
    zip
      (indices (paraHexGrid hexSize hexSize))
      (repeat Nothing)

instance Show Hex where
  show (Hex b) =
    replicate (2*(hexSize-1)) ' ' ++ concat (replicate hexSize "  _ ") ++ "\n"
    ++
    intercalate "\n" [intercalate "\n" (gridShowLine (Hex b) r) | r <- [0..hexSize-1]]
    ++
    "\n" ++ concat (replicate hexSize " \\_/")

gridShowLine :: Hex -> Int -> [String]
gridShowLine (Hex b) y  = [rowOffset ++ tileTop ++ [x | y/=0, x <- " /"]
                          ,rowOffset ++ "| " ++ intercalate " | " (map (\x -> showP $ b ! (x, hexSize-1-y)) [0..(hexSize-1)]) ++ " |"
                          ] where
  showP (Just Player1) = "1"
  showP (Just Player2) = "2"
  showP Nothing = " "
  rowOffset = replicate (2*(hexSize-y-1)) ' '
  tileTop = concat $ replicate hexSize " / \\"

instance PositionalGame Hex (Int, Int) where
  getPosition (Hex b) = flip lookup b
  positions (Hex b) = elems b
  setPosition (Hex b) c p = if member c b then Just $ Hex $ insert c (Just p) b else Nothing
  makeMove = takeEmptyMakeMove
  gameOver (Hex b)
    | True `elem` (map (\(x, y) -> path (getPlayerGraph Player1 (Hex b)) x y) (edgePaths Player1)) = Just (Just Player1)
    | True `elem` (map (\(x, y) -> path (getPlayerGraph Player2 (Hex b)) x y) (edgePaths Player2)) = Just (Just Player2)
    | otherwise = Nothing

edgeVertexes :: Player -> ([Int], [Int])
edgeVertexes Player1 = ([positionToVertex (x, 0) | x <- [0..hexSize-1]], [positionToVertex (x,hexSize-1) | x <- [0..hexSize-1]])
edgeVertexes Player2 = ([positionToVertex (0, y) | y <- [0..hexSize-1]], [positionToVertex (hexSize-1,y) | y <- [0..hexSize-1]])

edgePaths :: Player -> [(Int,Int)]
edgePaths p = [(v1, v2) | v1 <- ev1, v2 <- ev2, v1 /= v2]
  where ev1 = fst (edgeVertexes p)
        ev2 = snd (edgeVertexes p)


positionToVertex :: (Int, Int) -> Int
positionToVertex pos = fromMaybe (-1) (elemIndex pos (indices (paraHexGrid hexSize hexSize)))

getPlayerGraph :: Player -> Hex -> Graph
getPlayerGraph p b = buildG (0, hexSize*hexSize) $
                      concatMap (\(x, y) -> zip (repeat x) y) $
                        zip (map positionToVertex playerPos) 
                            (map ((map positionToVertex . (`intersect` playerPos)) . neighbours (paraHexGrid hexSize hexSize)) playerPos)
  where playerPos = getPlayerPositions p b

getPlayerPositions :: Player -> Hex -> [(Int,Int)]
getPlayerPositions p (Hex b) = keys $ fromList $ filter ((== Just p) . snd) (zip (keys b) (elems b))

-------------------------------------------------------------------------------
-- * Havannah
-------------------------------------------------------------------------------

newtype Havannah = Havannah (Graph, [Maybe Player])

instance Show Havannah where
  show (Havannah (g, b)) = show b

instance PositionalGame Havannah Int where
  getPosition (Havannah (g, b)) i = if i < length b
                                     then Just $ b !! i
                                     else Nothing
  positions (Havannah (g, b)) = b
  setPosition (Havannah (g, b)) i p = if i < length b
                                     then Just $ Havannah (g, take i b ++ Just p : drop (i+1) b)
                                     else Nothing
  makeMove = takeEmptyMakeMove
  gameOver (Havannah (g, b)) = Just <$> (symmetric (bridgeCriterion g . pieces Player1)
                           +|+ symmetric (forkCriterion g . pieces Player1)
                           +|+ symmetric (ringCriterion g . pieces Player1)) b

pieces :: Player -> [Maybe Player] -> [Bool]
pieces p s = (== Just p) <$> s

dirToUndir :: [(Int, Int)] -> [(Int, Int)]
dirToUndir e = e ++ (swap <$> e)


(+|+) :: (a -> Maybe Player)
      -> (a -> Maybe Player)
      -> (a -> Maybe Player)
crit1 +|+ crit2 = \x -> case (crit1 x, crit2 x) of
  (Just p1, Just p2) | p1 == p2 -> Just p1
  (Just _, Just _) -> error "conflicting winner"
  (p1, p2) -> p1 <|> p2


symmetric :: ([Maybe Player] -> Bool) -> [Maybe Player] -> Maybe Player
symmetric criterion state = case (criterion state, criterion (fmap nextPlayer <$> state)) of
  (True, True) -> error "conflicting winner"
  (True, False) -> Just Player1
  (False, True) -> Just Player2
  (False, False) -> Nothing

emptyHavannah :: Int -> Havannah
emptyHavannah n = Havannah (g, replicate (length $ vertices g) Nothing)
  where
    g = buildG (0, 3*n*(n-1)) (dirToUndir $ hexagonalEdges n)

    hexagonalEdges n = (\(x, y) -> (fromJust $ elemIndex x nodes, fromJust $ elemIndex y nodes)) <$> Grid.edges grid

    grid = hexHexGrid n
    nodes = indices grid


filterGraphE :: ((Int, Int) -> Bool) -> Graph -> Graph
filterGraphE f g = buildG (0, (length $ vertices g) - 1) (filter f $ Graph.edges g)

filterGraphN :: (Int -> Bool) -> Graph -> Graph
filterGraphN f = filterGraphE (\(i, j) -> f i && f j)

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) $ subsequences ns

bridgeCriterion :: Graph -> [Bool] -> Bool
bridgeCriterion g state = or [ path filteredG i j | [i, j] <- combinations 2 corners ]
  where
    -- the corners are the nodes which are only reachable by 3 nodes
    corners = filter (\z -> indegree g Data.Array.! z == 3) $ toList $ vertices g
    filteredG = filterGraphN (state !!) g

forkCriterion :: Graph -> [Bool] -> Bool
forkCriterion g state = or
    [ path filteredG i j && path filteredG j k | [is, js, ks] <- combinations 3 edgesNodes, i <- is, j <- js, k <- ks ]
  where
    isEdge = (\z -> indegree g Data.Array.! z == 4)
    edgesNodes = filter (all isEdge) $ toList <$> (scc $ filterGraphN isEdge g)

    filteredG = filterGraphN (state !!) g


ringCriterion :: Graph -> [Bool] -> Bool
ringCriterion g state = any (\tree -> all (\n -> not (n `elem` tree)) outerNodes)
                        $ filter (not . any (state !!))
                        $ toList <$> scc filteredG
  where
    (innerNodes, outerNodes) = partition (\z -> indegree g Data.Array.! z == 6) $ toList $ vertices g
    filteredG = filterGraphN (not . (state !!)) g

-------------------------------------------------------------------------------
-- * CLI interactions
-------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "1: TicTacToe"
  putStrLn "2: Arithmetic Progression Game"
  putStrLn "3: Shannon Switching Game"
  putStrLn "4: Gale"
  putStrLn "5: Hex"
  putStrLn "6: Havannah"
  putStr "What do you want to play? "
  hFlush stdout
  choice <- read <$> getLine
  case choice of
    1 -> player emptyTicTacToe
    2 -> playAPG
    3 -> player $ createShannonSwitchingGame 5
    4 -> player emptyGale
    5 -> player emptyHex
    6 -> player $ emptyHavannah 8
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
    Just a -> player a
    Nothing -> putStrLn "Not valid input (n < k)"
