module GoLAlgorithm where

import Data.List
import Cell
import GoLAlgorithmInternal
import RuleSet

-- This is more or less a facade for our game of life implementation. This is the only function, that is supposed to be visible to the outside,
-- so we split it from the internal functions in GoLAlgorithmInternal.hs. In here, we only do some calls down to more low level stuff in the internals.

-- Actual game of life function.
gameOfLife:: [Cell] -> RuleSet -> [Cell]
gameOfLife [] _ = []
gameOfLife list rl = step (nub (removeDeadTwins (createBigCellList list))) rl