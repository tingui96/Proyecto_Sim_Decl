module Enviroment
where

import Board
import Child
import Robot
import Tools
import Constant
import System.Random


simulacion :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
simulacion turn row column obstacles kids trash robots ia = do
  let houseEmpty = house row column

  g <- newStdGen
  let seed = fst (random g)
  let houseWithCorrals = addCorralTablero seed kids houseEmpty
  let houseWithObstacles = generaXCelltype (seed + 1) "obstacle" obstacles houseWithCorrals
  let houseWithChild = generaXCelltype (seed + 2) "child" kids houseWithObstacles
  let houseWithRobot = generaXCelltype (seed + 3) "robot" robots houseWithChild
  let houseWithTrash = generaXCelltype (seed + 4) "trash" trash houseWithRobot

  let childMoveProbability = 1 / 2
  let trashProbability = 2 / 3
  let countTurn = 1
  let max_loop = 1000

  putStr "\nIMPRIMIR TABLERO INICIAL\n"
  printBoard houseWithTrash

  loop (seed + 5) houseWithTrash childMoveProbability trashProbability countTurn turn ia max_loop row column

putCorrales :: [((Int, Int), (String, Bool, Bool))] -> Int -> [[((Int, Int), (String, Bool, Bool))]] -> [[((Int, Int), (String, Bool, Bool))]]
putCorrales x amount house
  | amount == 0 = house
  | otherwise =
    let first = head x
        cuerpo = tail x
        ((row, col), (label, pick, drop)) = first
        houseR = replace house row (replace (house !! row) col ((row, col), (label, pick, drop)))
     in putCorrales cuerpo (amount - 1) houseR

loop :: Int -> Board -> Float -> Float -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
loop seed house1 childMoveProbability trashProbability countTurn turn ia max_loop row column = do
  let countTashes = length (houseCellTypeEncounter trashConstant house1)
  let countRobots = length (houseCellTypeEncounter robotConstant house1)
  let countKids = length (houseCellTypeEncounter childConstant house1)
  let countObstacles = length (houseCellTypeEncounter obstacleConstant house1)
  let countCorral = length (houseCellTypeEncounter corralConstant house1)
  let countRobotTrash = length (houseCellTypeEncounter robotTrashConstant house1)
  let countRobotChild = length (houseCellTypeEncounter robotChildConstant house1)
  let countRobotCorral = length (houseCellTypeEncounter robotCorralConstant house1)
  let countChildCorral = length (houseCellTypeEncounter childCorralConstant house1)
  let countRobotChildCorral = length (houseCellTypeEncounter robotChildCorralConstant house1)
  let countRobotChildTrash = length (houseCellTypeEncounter robotChildTrashConstant house1)
  

  let houseWithChangeEnviroment = 
        if mod countTurn turn == 0
          then
              let b1 = house row column
                  corrales_only = houseCellTypeEncounter corralConstant house1
                  corrales_child = houseCellTypeEncounter childCorralConstant house1
                  corrales_robot = houseCellTypeEncounter robotCorralConstant house1
                  corrales_child_robot = houseCellTypeEncounter robotChildCorralConstant house1
                  corrales = corrales_only ++ corrales_child ++ corrales_robot ++ corrales_child_robot
                  -- c3 = putCorrales corrales (length corrales) b1

                  b2 = addCorralTablero (seed + 10) countCorral b1
                  b3 = generaXCelltype (seed + 11) "obstacle" countObstacles b2
                  b4 = generaXCelltype (seed + 12) "child" countKids b3
                  b5 = generaXCelltype (seed + 13) "robot" countRobots b4
                  b6 = generaXCelltype (seed + 15) "trash" countTashes b5
                  b7 = generaXCelltype (seed + 16) "robot-trash" countRobotTrash b6
                  b8 = generaXCelltype (seed + 17) "robot-child" countRobotChild b7
                  b9 = generaXCelltype (seed + 18) "robot-corral" countRobotCorral b8
                  b10 = generaXCelltype (seed + 19) "child-corral" countChildCorral b9
                  b11 = generaXCelltype (seed + 20) "robot-child-corral" countRobotChildCorral b10
                  house1 = generaXCelltype (seed + 21) "robot-child-trash" countRobotChildTrash b11
               in house1
          else
              house1
  let houseWithChildMoved = moveChilds (seed + 25) childMoveProbability trashProbability houseWithChangeEnviroment
  let houseWithRobotMoved = moveRobots houseWithChildMoved ia

  putStr ("TURNO: " ++ show countTurn ++ "\n")
  printBoard houseWithRobotMoved

  let cleanCells = length (houseCellTypeEncounter emptyConstant houseWithRobotMoved)
  let corrals = length (houseCellTypeEncounter corralConstant houseWithRobotMoved)
  let robotkids = length (houseCellTypeEncounter robotChildConstant houseWithRobotMoved)
  let total_cell = row * column
  let got = (div cleanCells total_cell) * 10

  if (corrals /= 0 || robotkids /= 0 || cleanCells * 10 < total_cell * 6) && countTurn < max_loop
    then loop (seed + 31) houseWithRobotMoved childMoveProbability trashProbability (countTurn + 1) turn ia max_loop row column
    else
      if countTurn == max_loop
        then putStrLn "Se alcanzo el maximo de turnos!!!\n"
        else putStr "Habitacion Limpia!!!\n"

----------------------------------------------------------------------------
main :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
main turn row column obstacles kids trash robots ia
  | obstacles + kids + trash + robots > row * column = print "La cantidad de elementos supera al tamaño de la matriz"
  | turn < 0 || column < 0 || row < 0 || obstacles < 0 || kids < 0 || trash < 0 || robots < 0 || ia < 1 || ia > 2 = print "Valores invalidos en los parametros"
  | otherwise = simulacion turn row column obstacles kids trash robots ia
    
