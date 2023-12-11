module Cell where

-- custom data type for storing information about a single cell 
data Cell = Cell {xCoord::Integer, yCoord::Integer, isAlive::Bool} deriving (Eq, Show)