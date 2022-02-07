module Tools
  (
    house,
    replace,    
    getBoardCell,
    getCellType,
    printBoard,
    isMember,
    replaceInBoard,
    replaceInBoardList,
    swapPosition,
    getPosition,
    runRandom,
    getDrop,
    getRow,
    getColumn,
    rand,
    randomRange,
    generateBoard
  )
where

import System.Random
import Control.Monad.State (State, evalState, get, put)
import Constant

type R a = State StdGen a
type Position = (Int, Int)
type CellType = String
maxConstant = 99999
type PositionBoardCell = (Position, (CellType, Bool, Bool))
type Board = [[PositionBoardCell]]

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: R Int
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r



replace :: [a] -> Int -> a -> [a]
replace list index element = let (first, x : xs) = splitAt index list in first ++ (element : xs)

generateRow :: Int -> Int -> Int -> [[(Position, Int)]]
generateRow r row col
  | r == row = []
  | otherwise = generateCol r 0 col : generateRow (r + 1) row col

swapPosition :: (Int, Int) -> (Int, Int) -> [[((Int, Int), (String, Bool, Bool))]] -> [[((Int, Int), (String, Bool, Bool))]]
swapPosition (originR, originC) (destinyR, destinyC) house =
  replaceInBoard ((originR, originC), y) (replaceInBoard ((destinyR, destinyC), x) house)
  where
    x = getBoardCell (house !! originR !! originC)
    y = getBoardCell (house !! destinyR !! destinyC)

getPosition :: (p, c) -> p
getPosition (p, c) = p

generateCol :: Int -> Int -> Int -> [(Position, Int)]
generateCol r c col
  | c == col = []
  | otherwise = ((r, c), maxConstant) : generateCol r (c + 1) col


getCellType :: (String, Bool, Bool) -> String
getCellType (c, p, d) = c

isMember :: Eq t => t -> [t] -> Bool
isMember n [] = False
isMember n (x : xs)
  | n == x    = True
  | otherwise = isMember n xs

houseFormer :: Int -> Int -> Int -> Board
houseFormer row rowEnd columnEnd
  | row == rowEnd = []
  | otherwise = houseRow row 0 columnEnd : houseFormer (row + 1) rowEnd columnEnd



getRow :: (Int, Int) -> Int
getRow (r, c) = r

printBoard :: [[((Int, Int), (String, Bool, Bool))]] -> IO ()
printBoard [] = putStr "\n"
printBoard (x : xs) = do
  printColumn x
  printBoard xs

replaceInBoard :: ((Int, Int), a) -> [[((Int, Int), a)]] -> [[((Int, Int), a)]]
replaceInBoard ((r, c), x) house = replace house r (replace (house !! r) c ((r, c), x))



printColumn :: [((Int, Int), (String, Bool, Bool))] -> IO ()
printColumn [] = putStr "\n"
printColumn ((_, (label, _, _)) : xs) = do
  putStr
    ( let p
            | label == "empty"              = "|_____|"
            | label == "child"              = "|__N__|"            
            | label == "robot"              = "|__R__|"
            | label == "corral"             = "|__C__|"            
            | label == "obstacle"           = "|__O__|"
            | label == "trash"              = "|__B__|"
            | label == "robot-corral"       = "|_RC__|"
            | label == "robot-trash"        = "|_RB__|"
            | label == "robot-child"        = "|_RN__|"
            | label == "child-corral"       = "|_NC__|"
            | label == "robot-child-trash"  = "|_RNB_|"
            | label == "robot-child-corral" = "|_RNC_|"
            | otherwise                 = ""
       in p
    )
  printColumn xs

generateBoard :: Int -> Int -> [[(Position, Int)]]
generateBoard row col
  | row == 0 || col == 0 = []
  | otherwise              = generateRow 0 row col

getColumn :: (Int, Int) -> Int
getColumn (r, c) = c


replaceInBoardList :: [((Int, Int), a)] -> [[((Int, Int), a)]] -> [[((Int, Int), a)]]
replaceInBoardList [] house = house
replaceInBoardList (((r, c), x) : xs) house = replaceInBoardList xs (replace house r (replace (house !! r) c ((r, c), x)))

house :: Int -> Int -> Board
house rowEnd columnEnd
  | rowEnd == 0 || columnEnd == 0 = []
  | otherwise              = houseFormer 0 rowEnd columnEnd

getBoardCell :: (p, c) -> c
getBoardCell (p, c) = c

houseRow :: Int -> Int -> Int -> [PositionBoardCell]
houseRow row column columnEnd
  | column == columnEnd = []
  | otherwise = ((row, column), ("empty", False, False)) : houseRow row (column + 1) columnEnd

getDrop :: (String, Bool, Bool) -> Bool
getDrop (_, _, d) = d

randomRange :: Int -> Int -> Int -> Int
randomRange low up n
  | low == up = low
  | a > 0     = low + a
  | otherwise = low + (-1 * a)
  where
    a = mod n (up - low)