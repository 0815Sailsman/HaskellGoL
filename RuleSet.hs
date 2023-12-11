module RuleSet where

-- Models a set of rules for the game. This can be expressed by:
--      1) The amount of cells necessary for an alive cell to survive. Here, alivelist contains all valid counts for that.
--      2) THe amount of cells necessary for a dead cell to spawn a new cell. Here, spawnlist contains all valid counts for that.
data RuleSet = RuleSet {aliveList:: [Integer], spawnList:: [Integer]}