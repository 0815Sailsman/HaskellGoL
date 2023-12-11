module GoLAlgorithmInternal where

import Data.List
import Cell
import RuleSet

-- This module contains internal functions concerned with the simulation of the game.

-- The highest level function in here, supposed to be the next "step" in the game. Applies game rules and logic to our board.
step:: [Cell] -> RuleSet -> [Cell]
step cellList (RuleSet al sl) = [ Cell{xCoord=x, yCoord=y, isAlive= True} | (Cell x y iA) <- cellList, if iA then 
                                                                                                let count = countSourroundingAliveCells (x,y) cellList in count `elem` al
                                                                                            else  
                                                                                                let count = countSourroundingAliveCells (x,y) cellList in count `elem` sl]

-- Takes the x and y coordinate of a cell and the board (cell List) and returns the amount of alive neighbouring cells
countSourroundingAliveCells:: (Integer, Integer) -> [Cell] -> Integer
countSourroundingAliveCells (_, _) [] = 0
countSourroundingAliveCells (myX, myY) ((Cell theirX theirY theirIsAlive):rest)
            | not theirIsAlive = countSourroundingAliveCells (myX,myY) rest
            | (abs (myX-theirX) <= 1 && abs (myY-theirY) <= 1) && (myX /= theirX || myY /= theirY) = 1 + countSourroundingAliveCells (myX,myY) rest
            | otherwise = countSourroundingAliveCells (myX,myY) rest
 
-- Create a list containing every cell that is alive and every dead cell neighbouring an alive cell including a copy of every original cell with isAlive=False
createBigCellList:: [Cell] -> [Cell]
createBigCellList [] = []
createBigCellList (cell:rest) = neighbours cell ++ createBigCellList rest
    where neighbours :: Cell -> [Cell]
          neighbours (Cell originX originY _) =
            Cell{xCoord= originX, yCoord= originY, isAlive= True}:[Cell{xCoord=originX+x, yCoord=originY+y, isAlive=False}  | x<-[-1..1], y<-[-1..1]]

-- Util function.
-- If two cells share the same coordinates, but one of them alive, the dead one is deleted
removeDeadTwins :: [Cell] -> [Cell]
removeDeadTwins [] = []
removeDeadTwins originalList = [cell | cell <- originalList, listDoesNotContainAliveTwin cell originalList]

-- Util function.
-- True if a list of cells doesnt contain another cell at the same coordinates that is alive
listDoesNotContainAliveTwin :: Cell -> [Cell] -> Bool
listDoesNotContainAliveTwin _ [] = True
listDoesNotContainAliveTwin (Cell _ _ True) _ = True
listDoesNotContainAliveTwin cell (x:xs) = isNotAliveTwin cell x && listDoesNotContainAliveTwin cell xs
    where isNotAliveTwin :: Cell -> Cell -> Bool
          isNotAliveTwin (Cell myX myY _) (Cell theirX theirY theirAlive) = not (myX == theirX && myY == theirY && theirAlive)