module Child
  (
    moveChilds
  )
where
 
import Board
import Tools


trashCell :: Int -> Float -> Int -> [PositionBoardCell] -> Board -> Board
trashCell _ _ 0 _ house = house
trashCell _ _ _ [] house = house
trashCell seed trashProbability trashToGenerate ((p, c) : xs) house =
  let randomChoice = randomRange 0 100 (runRandom rand seed)
      houseR
        | randomChoice <= round (trashProbability * 100) =
          trashCell randomChoice trashProbability (trashToGenerate - 1) xs (replaceInBoard (p, ("trash", False, False)) house)
        | otherwise = trashCell randomChoice trashProbability trashToGenerate xs house
   in houseR

generateTrash :: Int -> Float -> Position -> Board -> Board
generateTrash seed trashProbability p house = generateTrashList seed trashProbability (map getPosition (getAllNeighborWithMe p house)) house


moveChildsList :: Int -> Float -> Float -> [PositionBoardCell] -> Board -> Board
moveChildsList _ _ _ [] house = house
moveChildsList seed moveProbability trashProbability ((p, cell) : xs) house =
  let randomChoice = randomRange 0 100 (runRandom rand seed)
      houseR
        | randomChoice <= round (moveProbability * 100) =
          let moveChoices = getNeighbor p house
              randomMoveIndex = randomRange 0 (length moveChoices) (runRandom rand randomChoice)
              cellDestiny = moveChoices !! randomMoveIndex
              houseRR
                | getCellType (getBoardCell cellDestiny) == "empty" || getCellType (getBoardCell cellDestiny) == "obstacle" =
                  let direction = (getRow (getPosition cellDestiny) - getRow p, getColumn (getPosition cellDestiny) - getColumn p)
                      destinySwap = getFirstCellLine p (getRow p + getRow direction, getColumn p + getColumn direction) direction "empty" "obstacle" house
                      houseAfterMove = swapPosition p (getPosition cellDestiny) (swapPosition (getPosition cellDestiny) (getPosition destinySwap) house)
                   in generateTrash (seed + 1) trashProbability p houseAfterMove
                | otherwise = house
           in houseRR
        | otherwise = house
   in moveChildsList randomChoice moveProbability trashProbability xs houseR


generateTrashList :: Int -> Float -> [Position] -> Board -> Board
generateTrashList _ _ [] house = house
generateTrashList seed trashProbability ((r, c) : xs) house =
  let adyacents = getAllNeighborWithMe (r, c) house
      houseR
        | length adyacents == 9 =
          let amountChild = length (filter (\(p, c) -> getCellType c == "child") adyacents)
              amountTrash = length (filter (\(p, c) -> getCellType c == "trash") adyacents)
              trashToGenerate
                | amountChild == 1 && amountTrash < 1 = 1 - amountTrash
                | amountChild == 2 && amountTrash < 3 = 3 - amountTrash
                | amountChild >= 3 && amountTrash < 6 = 6 - amountTrash
                | otherwise = 0
              emptyCells = filter (\(p, c) -> getCellType c == "empty") adyacents
           in trashCell seed trashProbability trashToGenerate emptyCells house
        | otherwise = house
   in generateTrashList (seed + 1) trashProbability xs houseR

moveChilds :: Int -> Float -> Float -> Board -> Board
moveChilds seed moveProbability trashProbability house = 
  moveChildsList seed moveProbability trashProbability (houseCellTypeEncounter "child" house) house
