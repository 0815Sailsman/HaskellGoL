module GoLAlgorithmInternal where

import Data.List
import Cell
import RuleSet

step:: [Cell] -> RuleSet -> [Cell]
step cellList (RuleSet al sl) = [ Cell{xCoord=x, yCoord=y, isAlive= True} | (Cell x y iA) <- cellList, if iA then 
                                                                                                let count = countSourroundingAliveCells (x,y) cellList in count `elem` al
                                                                                            else  
                                                                                                let count = countSourroundingAliveCells (x,y) cellList in count `elem` sl]
 
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
            
-- If two cells share the same coordinates, but one of them alive, the dead one is deleted
removeDeadTwins :: [Cell] -> [Cell]
removeDeadTwins [] = []
removeDeadTwins originalList = [cell | cell <- originalList, listDoesNotContainAliveTwin cell originalList]

-- True if a list of cells doesnt contain another cell at the same coordinates that is alive
listDoesNotContainAliveTwin :: Cell -> [Cell] -> Bool
listDoesNotContainAliveTwin _ [] = True
listDoesNotContainAliveTwin (Cell _ _ True) _ = True
listDoesNotContainAliveTwin cell (x:xs) = isNotAliveTwin cell x && listDoesNotContainAliveTwin cell xs
    where isNotAliveTwin :: Cell -> Cell -> Bool
          isNotAliveTwin (Cell myX myY _) (Cell theirX theirY theirAlive) = not (myX == theirX && myY == theirY && theirAlive)