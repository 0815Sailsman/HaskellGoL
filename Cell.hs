module Cell where

data Cell = Cell {xCoord::Integer, yCoord::Integer, isAlive::Bool} deriving (Eq, Show)