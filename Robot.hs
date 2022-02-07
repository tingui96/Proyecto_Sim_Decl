module Robot
  (
    moveRobots,
    generateDistanceTable,
    expandDistance
  )
where

import Board
import Tools
import Constant

maxConstant = 999999

leaveChild :: [PositionBoardCell] -> Board -> Board
leaveChild [] house = house
leaveChild ((p, (c, pick, drop)) : xs) house =
  if not drop
    then
      let posRobot = replaceInBoard (p, (robotChildCorralConstant, False, True)) house
       in leaveChild xs posRobot
    else leaveChild xs house

calculateLowerDistanceList :: Position -> [Position] -> [[(Position, Int)]] -> Int -> (Position, Int)
calculateLowerDistanceList p [] _ mini = (p, mini)
calculateLowerDistanceList p (position : xs) tableroDistance mini
  | minInAdyacents < mini = calculateLowerDistanceList position xs tableroDistance minInAdyacents
  | otherwise = calculateLowerDistanceList p xs tableroDistance mini
  where
    adyacents = getNeighbor position tableroDistance
    minInAdyacents = foldr min maxConstant (map (\(_, x) -> x) adyacents)


moveRobots :: Board -> Int -> Board
moveRobots house ia =
  let houseRobot 
        | ia == 1 = firstChild (houseCellTypeEncounter robotConstant house) house
        | ia == 2 = firstTrash (houseCellTypeEncounter robotConstant house) house
        | otherwise = house
      houseWithRC
        | ia == 1 = firstChild (houseCellTypeEncounter robotCorralConstant house) houseRobot
        | ia == 2 = firstTrash (houseCellTypeEncounter robotCorralConstant house) houseRobot
        | otherwise = houseRobot
      houseLeaveChild
        | ia == 1 = firstChild (houseCellTypeEncounter robotChildCorralConstant house) houseWithRC
        | ia == 2 = firstTrash (houseCellTypeEncounter robotChildCorralConstant house) houseWithRC
        | otherwise = houseWithRC
      houseRC = toCorrals (houseCellTypeEncounter robotChildConstant house) houseLeaveChild
      houseRCH = leaveChild (houseCellTypeEncounter robotChildCorralConstant house) houseRC
      houseRobotTrash = clearTrash (houseCellTypeEncounter robotTrashConstant house) houseRCH
      houseRCT = clearTrashWithChild (houseCellTypeEncounter robotChildTrashConstant house) houseRobotTrash
   in houseRCT

clearTrashWithChild :: [PositionBoardCell] -> Board -> Board
clearTrashWithChild [] house = house
clearTrashWithChild ((p, c) : xs) house =
  let posRobot = replaceInBoard (p, (robotChildConstant, False, False)) house
   in clearTrashWithChild xs posRobot

firstChild :: [PositionBoardCell] -> Board -> Board
firstChild [] house = house
firstChild ((p, (c, pick, drop)) : xs) house =
  if c == robotChildCorralConstant && not drop
    then firstChild xs house
    else
      let tableroDistance = generateDistanceTable p [childConstant, trashConstant] cantPass house
          childsList = houseCellTypeEncounter "child" house
          trashList = houseCellTypeEncounter "trash" house
          positionRobot =
            if null childsList
              then
                if null trashList
                  then house
                  else
                    let positionTrash = map getPosition trashList
                        (trashWithLowerDistance, trashDistance) = calculateLowerDistanceList (head positionTrash) positionTrash tableroDistance maxConstant
                     in if trashDistance == maxConstant
                          then house
                          else
                            let path = getPathFromDistance trashWithLowerDistance tableroDistance []
                                (rDestiny, cDestiny) = walkNCells 1 path
                                (start, finish) = upgradeCell (p, (c, pick, drop)) (house !! rDestiny !! cDestiny)
                             in replaceInBoardList [start, finish] house
              else
                let childsPositions = map getPosition childsList
                    (childWithLowerDistance, childDistance) = calculateLowerDistanceList (head childsPositions) childsPositions tableroDistance maxConstant
                 in if childDistance == maxConstant
                      then
                        let positionTrash = map getPosition trashList
                            (trashWithLowerDistance, trashDistance) = calculateLowerDistanceList (head positionTrash) positionTrash tableroDistance maxConstant
                         in if trashDistance == maxConstant
                              then house
                              else
                                let path = getPathFromDistance trashWithLowerDistance tableroDistance []
                                    (rDestiny, cDestiny) = walkNCells 1 path
                                    (start, end) = upgradeCell (p, (c, pick, drop)) (house !! rDestiny !! cDestiny)
                                 in replaceInBoardList [start, end] house
                      else
                        let path = getPathFromDistance childWithLowerDistance tableroDistance []
                            (rDestiny, cDestiny) = walkNCells 1 path
                            (start, end) = upgradeCell (p, (c, pick, drop)) (house !! rDestiny !! cDestiny)
                         in replaceInBoardList [start, end] house
       in firstChild xs positionRobot

generateDistanceTable :: Position -> [CellType] -> [CellType] -> Board -> [[(Position, Int)]]
generateDistanceTable p destinationList obstacleList house =
  let r = length house
      c = length (head house)
      db = generateBoard r c
      dbWith0 = replaceInBoard (p, 0) db
   in expandDistance [(p, 0)] destinationList obstacleList house dbWith0



toCorrals :: [PositionBoardCell] -> Board -> Board
toCorrals [] house = house
toCorrals ((p, (c, pick, drop)) : xs) house =
  let tableroDistance = generateDistanceTable p [corralConstant, trashConstant] cantPass house
      corrals = houseCellTypeEncounter corralConstant house
      trash = houseCellTypeEncounter "trash" house
      posRobot =
        if null corrals
          then house
          else
            let posCorral = map getPosition corrals
                (corralWithLowerDistance, corralDistance) = calculateLowerDistanceList (head posCorral) posCorral tableroDistance maxConstant
             in if corralDistance == maxConstant
                  then
                    let posTrash = map getPosition trash
                        (trashWithLowerDistance, trashDistance) = calculateLowerDistanceList (head posTrash) posTrash tableroDistance maxConstant
                     in if trashDistance == maxConstant
                          then house
                          else
                            let path = getPathFromDistance trashWithLowerDistance tableroDistance []
                                (rDestiny, cDestiny) = walkNCells 2 path
                                (start, finish) = upgradeCell (p, (c, pick, drop)) (house !! rDestiny !! cDestiny)
                             in replaceInBoardList [start, finish] house
                  else
                    let path = getPathFromDistance corralWithLowerDistance tableroDistance []
                        (rDestiny, cDestiny) = walkNCells 2 path
                        (start, finish) = upgradeCell (p, (c, pick, drop)) (house !! rDestiny !! cDestiny)
                     in replaceInBoardList [start, finish] house
   in toCorrals xs posRobot



clearTrash :: [PositionBoardCell] -> Board -> Board
clearTrash [] house = house
clearTrash ((p, c) : xs) house =
  let posRobot = replaceInBoard (p, (robotConstant, False, False)) house
   in clearTrash xs posRobot


walkNCells :: Int -> [Position] -> Position
walkNCells steps path
  | steps > length path = path !! (length path - 1)
  | otherwise           = path !! (steps - 1)


getPathFromDistance :: Position -> [[(Position, Int)]] -> [Position] -> [Position]
getPathFromDistance (r, c) tableroDistance path
  | getBoardCell (tableroDistance !! r !! c) == 0 = path
  | otherwise =
    let pathR = (r, c) : path
        adyacents = getNeighbor (r, c) tableroDistance
        (d, pR) = minimum (map (\(p, x) -> (x, p)) adyacents)
     in getPathFromDistance pR tableroDistance pathR

upgradeCell :: PositionBoardCell -> PositionBoardCell -> (PositionBoardCell, PositionBoardCell)
upgradeCell start finish
  | ini == robotConstant && end == emptyConstant = ((pini, (emptyConstant, False, False)), (pend, (robotConstant, False, False)))
  | ini == robotConstant && end == childConstant = ((pini, (emptyConstant, False, False)), (pend, (robotChildConstant, True, False)))
  | ini == robotConstant && end == trashConstant = ((pini, (emptyConstant, False, False)), (pend, (robotTrashConstant, False, False)))
  | ini == robotConstant && end == corralConstant = ((pini, (emptyConstant, False, False)), (pend, (robotCorralConstant, False, False)))
  | ini == robotChildConstant && end == emptyConstant = ((pini, (emptyConstant, False, False)), (pend, (robotChildConstant, False, False)))
  | ini == robotChildConstant && end == trashConstant = ((pini, (emptyConstant, False, False)), (pend, (robotChildTrashConstant, False, False)))
  | ini == robotChildConstant && end == corralConstant = ((pini, (emptyConstant, False, False)), (pend, (robotChildCorralConstant, False, False)))
  | ini == robotTrashConstant && end == emptyConstant = ((pini, (trashConstant, False, False)), (pend, (robotConstant, False, False)))
  | ini == robotTrashConstant && end == childConstant = ((pini, (trashConstant, False, False)), (pend, (robotChildConstant, True, False)))
  | ini == robotTrashConstant && end == trashConstant = ((pini, (trashConstant, False, False)), (pend, (robotTrashConstant, False, False)))
  | ini == robotTrashConstant && end == corralConstant = ((pini, (trashConstant, False, False)), (pend, (robotCorralConstant, False, False)))
  | ini == robotCorralConstant && end == emptyConstant = ((pini, (corralConstant, False, False)), (pend, (robotConstant, False, False)))
  | ini == robotCorralConstant && end == childConstant = ((pini, (corralConstant, False, False)), (pend, (robotChildConstant, True, False)))
  | ini == robotCorralConstant && end == trashConstant = ((pini, (corralConstant, False, False)), (pend, (robotTrashConstant, False, False)))
  | ini == robotCorralConstant && end == corralConstant = ((pini, (corralConstant, False, False)), (pend, (robotCorralConstant, False, False)))
  | ini == robotChildTrashConstant && end == emptyConstant = ((pini, (trashConstant, False, False)), (pend, (robotChildConstant, False, False)))
  | ini == robotChildTrashConstant && end == trashConstant = ((pini, (trashConstant, False, False)), (pend, (robotChildTrashConstant, False, False)))
  | ini == robotChildTrashConstant && end == corralConstant = ((pini, (trashConstant, False, False)), (pend, (robotChildCorralConstant, False, False)))
  | drop && ini == robotChildCorralConstant && end == emptyConstant = ((pini, (childCorralConstant, False, False)), (pend, (robotConstant, False, False)))
  | drop && ini == robotChildCorralConstant && end == childConstant = ((pini, (childCorralConstant, False, False)), (pend, (robotChildConstant, True, False)))
  | drop && ini == robotChildCorralConstant && end == trashConstant = ((pini, (childCorralConstant, False, False)), (pend, (robotTrashConstant, False, False)))
  | drop && ini == robotChildCorralConstant && end == corralConstant = ((pini, (childCorralConstant, False, False)), (pend, (robotCorralConstant, False, False)))
  | ini == robotChildCorralConstant && end == emptyConstant = ((pini, (corralConstant, False, False)), (pend, (robotChildConstant, False, False)))
  | ini == robotChildCorralConstant && end == trashConstant = ((pini, (corralConstant, False, False)), (pend, (robotChildTrashConstant, False, False)))
  | ini == robotChildCorralConstant && end == corralConstant = ((pini, (corralConstant, False, False)), (pend, (robotChildCorralConstant, False, False)))
  --
  | otherwise = (start, finish)
  where
    ini = getCellType (getBoardCell start)
    drop = getDrop (getBoardCell start)
    end = getCellType (getBoardCell finish)
    pini = getPosition start
    pend = getPosition finish

firstTrash :: [PositionBoardCell] -> Board -> Board
firstTrash [] house = house
firstTrash ((p, (c, pick, drop)) : xs) house =
  if c == robotChildCorralConstant && not drop
    then firstTrash xs house
    else
      let tableroDistance = generateDistanceTable p [childConstant, trashConstant] cantPass house
          childs = houseCellTypeEncounter "child" house
          trash = houseCellTypeEncounter "trash" house
          posRobot =
            if null trash
              then
                if null childs
                  then house
                  else
                    let childsPositions = map getPosition childs
                        (childWithLowerDistance, childDistance) = calculateLowerDistanceList (head childsPositions) childsPositions tableroDistance maxConstant
                     in if childDistance == maxConstant
                          then house
                          else
                            let path = getPathFromDistance childWithLowerDistance tableroDistance []
                                (rDestiny, cDestiny) = walkNCells 1 path
                                (start, finish) = upgradeCell (p, (c, pick, drop)) (house !! rDestiny !! cDestiny)
                             in replaceInBoardList [start, finish] house
              else
                let posTrash = map getPosition trash
                    (trashWithLowerDistance, trashDistance) = calculateLowerDistanceList (head posTrash) posTrash tableroDistance maxConstant
                 in if trashDistance == maxConstant
                      then
                        let childPositions = map getPosition childs
                            (childhWithLowerDistance, childDistance) = calculateLowerDistanceList (head childPositions) childPositions tableroDistance maxConstant
                         in if childDistance == maxConstant
                              then house
                              else
                                let path = getPathFromDistance childhWithLowerDistance tableroDistance []
                                    (rDestiny, cDestiny) = walkNCells 1 path
                                    (start, finish) = upgradeCell (p, (c, pick, drop)) (house !! rDestiny !! cDestiny)
                                 in replaceInBoardList [start, finish] house
                      else
                        let path = getPathFromDistance trashWithLowerDistance tableroDistance []
                            (rDestiny, cDestiny) = walkNCells 1 path
                            (start, finish) = upgradeCell (p, (c, pick, drop)) (house !! rDestiny !! cDestiny)
                         in replaceInBoardList [start, finish] house
       in firstTrash xs posRobot

expandDistance :: [(Position, Int)] -> [CellType] -> [CellType] -> Board -> [[(Position, Int)]] -> [[(Position, Int)]]
expandDistance [] _ _ _ tableroDistance = tableroDistance
expandDistance (((row, column), distance) : xs) destinationList obstaclesList house tableroDistance =
  let adyacents = getNeighbor (row, column) house

      adyacentsFDestinationCellType = filter (\(_, (c, _, _)) -> isMember c destinationList) adyacents
      adyacentsFDestinationDistance = filter (\((r, c), _) -> distance + 1 < getBoardCell (tableroDistance !! r !! c)) adyacentsFDestinationCellType
      adyacentsDestinationDistance = map (\(p, _) -> (p, distance + 1)) adyacentsFDestinationDistance
      dhouseRR = replaceInBoardList adyacentsDestinationDistance tableroDistance

      adyacentsFCellType = filter (\(_, (c, _, _)) -> not (isMember c obstaclesList)) adyacents
      adyacentsFDistance = filter (\((r, c), _) -> distance + 1 < getBoardCell (tableroDistance !! r !! c)) adyacentsFCellType
      adyacentsDistance = map (\(p, _) -> (p, distance + 1)) adyacentsFDistance
      dhouseR = replaceInBoardList adyacentsDistance dhouseRR
   in expandDistance (xs ++ adyacentsDistance) destinationList obstaclesList house dhouseR
