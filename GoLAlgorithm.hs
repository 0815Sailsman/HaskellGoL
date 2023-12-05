module GoLAlgorithm where

import Data.List
import Cell
import GoLAlgorithmInternal

gameOfLife:: [Cell] -> [Cell]
gameOfLife [] = []
gameOfLife list = step (nub (removeDeadTwins (createBigCellList list)))