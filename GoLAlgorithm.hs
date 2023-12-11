module GoLAlgorithm where

import Data.List
import Cell
import GoLAlgorithmInternal
import RuleSet

gameOfLife:: [Cell] -> RuleSet -> [Cell]
gameOfLife [] _ = []
gameOfLife list rl = step (nub (removeDeadTwins (createBigCellList list))) rl