module Constant
(
    cantPass,
    robotConstant,
    childConstant,
    trashConstant,
    emptyConstant,
    corralConstant,
    obstacleConstant,
    robotCorralConstant,
    robotChildConstant,
    robotChildCorralConstant,
    robotTrashConstant,
    robotChildTrashConstant,
    childCorralConstant
) where

robotConstant = "robot"

childConstant = "child"

trashConstant = "trash"

emptyConstant = "empty"

corralConstant = "corral"

obstacleConstant = "obstacle"

robotCorralConstant = "robot-corral"

robotChildConstant = "robot-child"

robotChildCorralConstant = "robot-child-corral"

robotTrashConstant = "robot-trash"

robotChildTrashConstant = "robot-child-trash"

childCorralConstant = "child-corral"
--

cantPass = [robotConstant, robotChildConstant, robotChildCorralConstant, robotTrashConstant, robotChildTrashConstant, robotCorralConstant, childConstant, childCorralConstant, obstacleConstant]
