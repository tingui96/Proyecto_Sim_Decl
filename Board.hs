module Board
  ( 
    Board,
    Position,
    CellType,
    PositionBoardCell,
    addCorralTablero,
    generaXCelltype,
    getNeighbor,
    houseCellTypeEncounter,
    getFirstCellLine,
    getAllNeighborWithMe    
  )
where

import Tools
import Constant

type CellType = String

type Position = (Int, Int)

type PositionBoardCell = (Position, (CellType, Bool, Bool))

type Board = [[PositionBoardCell]]


generaXCelltype :: Int -> CellType -> Int -> Board -> Board
generaXCelltype seed cellType amount house
  | amount == 0 = house
  | otherwise =
    let emptyCells = houseCellTypeEncounter "empty" house
        randomCell = randomRange 0 (length emptyCells - 1) (runRandom rand seed)
        ((rndRow, rndCol), _) = emptyCells !! randomCell
        houseR = replace house rndRow (replace (house !! rndRow) rndCol ((rndRow, rndCol), (cellType, False, False)))
     in generaXCelltype rndRow cellType (amount - 1) houseR

getNeighbor :: Position -> [[(Position, a)]] -> [(Position, a)]
getNeighbor (row, column) house = result
  where
    rows = length house
    columns = length (head house)
    north
      | row == 0 = []
      | otherwise = [ house !! (row - 1) !! column ]
    south
      | row == (rows - 1) = []
      | otherwise = [ house !! (row + 1) !! column ]
    west
      | column == 0 = []
      | otherwise = [ house !! row !! (column - 1) ]
    east
      | column == (columns - 1) = []
      | otherwise = [ house !! row !! (column + 1) ]
    result = north ++ south ++ west ++ east

addCorralTablero :: Int -> Int -> Board -> Board
addCorralTablero seed amount house
  | amount == 0 = house
  | null corralsAlready =
    let emptyCells = houseCellTypeEncounter "empty" house
        randomCell = randomRange 0 (length emptyCells - 1) (runRandom rand seed)
        ((rndRow, rndCol), _) = emptyCells !! randomCell
        houseR = replace house rndRow (replace (house !! rndRow) rndCol ((rndRow, rndCol), ("corral", False, False)))
     in addCorralTablero rndRow (amount - 1) houseR
  | otherwise =
    let corralCells = houseCellTypeEncounter "corral" house
        corralWithAdyacents = map (\((row, column), x) -> (((row, column), x), getNeighbor (row, column) house)) corralCells
        corralWithEmptyAdyacents = map (\(x, l) -> (x, fitraPor "empty" l)) corralWithAdyacents
        corralWithEmptyAdyacentsNotNull = filter (\(_, l) -> not (null l)) corralWithEmptyAdyacents

        randomCorral = randomRange 0 (length corralWithEmptyAdyacentsNotNull - 1) (runRandom rand seed)
        (((rndRow, rndCol), _), l) = corralWithEmptyAdyacentsNotNull !! randomCorral

        randomEmpty = randomRange 0 (length l - 1) (runRandom rand rndRow)
        ((rER, rEC), _) = l !! randomEmpty

        houseR = replace house rER (replace (house !! rER) rEC ((rER, rEC), ("corral", False, False)))
     in addCorralTablero rER (amount - 1) houseR
  where
    corralsAlready = houseCellTypeEncounter "corral" house

getNeighborCross :: Position -> Board -> [PositionBoardCell]
getNeighborCross (row, column) house = result
  where
    rows = length house
    columns = length (head house)
    upLeft
      | row == 0 || column == 0 = []
      | otherwise = [ house !! (row - 1) !! (column - 1) ]
    downLeft
      | row == (rows - 1) || column == 0 = []
      | otherwise = [ house !! (row + 1) !! (column - 1) ]
    upRight
      | row == 0 || column == (columns - 1) = []
      | otherwise = [ house !! (row - 1) !! (column + 1) ]
    downRight
      | row == (rows - 1) || column == (columns - 1) = []
      | otherwise = [ house !! (row + 1) !! (column + 1) ]
    result = upLeft ++ downLeft ++ upRight ++ downRight

houseCellTypeEncounter :: CellType -> Board -> [PositionBoardCell]
houseCellTypeEncounter cellType house = houseCellTypeEncounterRow cellType house 0 (length house)

getAllNeighbor :: Position -> Board -> [PositionBoardCell]
getAllNeighbor p house = getNeighbor p house ++ getNeighborCross p house

houseCellTypeEncounterColmn :: CellType -> Board -> Int -> Int -> Int -> [PositionBoardCell]
houseCellTypeEncounterColmn cellType house row column columnEnd
  | column == columnEnd = []
  | getCellType (getBoardCell (house !! row !! column)) == cellType = (house !! row !! column) : houseCellTypeEncounterColmn cellType house row (column + 1) columnEnd
  | otherwise = houseCellTypeEncounterColmn cellType house row (column + 1) columnEnd

getFirstCellLine :: Position -> Position -> Position -> CellType -> CellType -> Board -> PositionBoardCell
getFirstCellLine positionIni position direction cellTypeObjetive cellTypeObstacle house =
  let rows = length house
      columns = length (head house)
      result
        | getRow position < 0 || getRow position >= rows || getColumn position < 0 || getColumn position >= columns = house !! getRow positionIni !! getColumn positionIni
        | getCellType (getBoardCell (house !! getRow position !! getColumn position)) == cellTypeObjetive = house !! getRow position !! getColumn position
        | getCellType (getBoardCell (house !! getRow position !! getColumn position)) == cellTypeObstacle = getFirstCellLine positionIni (getRow position + getRow direction, getColumn position + getColumn direction) direction cellTypeObjetive cellTypeObstacle house
        | otherwise = house !! getRow positionIni !! getColumn positionIni
   in result

fitraPor :: CellType -> [PositionBoardCell] -> [PositionBoardCell]
fitraPor _ [] = []
fitraPor cellType ((p, x) : xs)
  | getCellType x == cellType = (p, x) : fitraPor cellType xs
  | otherwise = fitraPor cellType xs

houseCellTypeEncounterRow :: CellType -> Board -> Int -> Int -> [PositionBoardCell]
houseCellTypeEncounterRow cellType house row rowEnd
  | row == rowEnd = []
  | otherwise = houseCellTypeEncounterColmn cellType house row 0 (length (house !! row)) ++ houseCellTypeEncounterRow cellType house (row + 1) rowEnd

getAllNeighborWithMe :: Position -> Board -> [PositionBoardCell]
getAllNeighborWithMe p house = getAllNeighbor p house ++ [house !! getRow p !! getColumn p]

