module Util where
import Cell
import RuleSet

-- This module has no specific concern and just contains a collection of useful helper functions that came up in the development process.

-- Takes a string (in our case, mostly contents of ruleset files) and spits out a ruleSet object
createRuleSetFromString:: String -> RuleSet
createRuleSetFromString input = RuleSet (map (read :: String -> Integer) (words aliveString)) (map (read :: String -> Integer) (words spawnString))
  where
    aliveString = head(splitOn (=='\n') input)
    spawnString = last(splitOn (=='\n') input)

-- Takes a String (in our case mostly contentrs of board files) and returns a board (list of cells)
createBoardFromString:: String -> [Cell] 
createBoardFromString inString = [  let x = read (head (splitOn (=='|') word))::Integer
                                        y = read (last (splitOn (=='|') word))::Integer 
                                    in Cell x y True 
                                    | word <- words inString]

-- A copy of the internal 'words' function, that now takes an extra function a parameter to decide, on which condition to split the string. Basically java split.
splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'


-- When creating a new starting board, we want to save it to a text file. This function (and the next one) serialoize a list of cells into a string.
convertBoardToSaveString:: [Cell] -> String
convertBoardToSaveString = concatMap convertCellToSaveString

-- Used by the function above. Converts a single cell to its saving format. When didn't use a show implementation, because we needed different string conversions
-- in different places, so we just did it manually in here.
convertCellToSaveString:: Cell -> String
convertCellToSaveString (Cell originX originY _) = show originX ++ "|" ++ show originY ++ " "

-- Takes a list of elements and delets every element that appears more than once ([1,2,1,3] -> [2,3])
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates l = [x | x<-l, count x l ==1]
  where
    count:: Eq a =>a -> [a] -> Int
    count x = length . filter (== x)

-- Generates the String to print from our game rules, viewport and cell list. This is more or less the core visualization to get our game to display in the terminal.
-- Iterates through all visible cells (by starting coords and width / height) and determines their textual representation. Also appends a useful small infodump
-- containing minor details about the current state of the visualization.
boardToString :: [Cell] -> (Integer, Integer) -> (Integer, Integer) -> String
boardToString cl (x,y) (height, width) = concat [ (concat [getTileFill (xco, yco) | xco <- [x..(x+width-1)]]) ++ "\n" | yco <- [y..(y+height-1)] ] ++ infodump
    where getTileFill :: (Integer, Integer) -> String
          getTileFill (xTF,yTF) = if Cell xTF yTF True `elem` cl then "██" else "░░"
          infodump = "\nFieldsize:"++show(width)++"x"++show(height)++" ; from "++show(x)++"|"++show(y)++" to "++show(x+width-1)++"|"++show(y+height-1)