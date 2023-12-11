module Util where

import Cell
import RuleSet

createRuleSetFromString:: String -> RuleSet
createRuleSetFromString input = RuleSet (map (read :: String -> Integer) (words aliveString)) (map (read :: String -> Integer) (words spawnString))
  where
    aliveString = head(splitOn (=='\n') input)
    spawnString = last(splitOn (=='\n') input)

createBoardFromString:: String -> [Cell] 
createBoardFromString inString = [  let x = read (head (splitOn (=='|') word))::Integer
                                        y = read (last (splitOn (=='|') word))::Integer 
                                    in Cell x y True 
                                    | word <- words inString]


splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'


convertBoardToSaveString:: [Cell] -> String
convertBoardToSaveString = concatMap convertCellToSaveString


convertCellToSaveString:: Cell -> String
convertCellToSaveString (Cell originX originY _) = show originX ++ "|" ++ show originY ++ " "


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates l = [x | x<-l, count x l ==1]
  where
    count:: Eq a =>a -> [a] -> Int
    count x = length . filter (== x)


boardToString :: [Cell] -> (Integer, Integer) -> (Integer, Integer) -> String
boardToString cl (x,y) (height, width) = concat [ (concat [getTileFill (xco, yco) | xco <- [x..(x+width-1)]]) ++ "\n" | yco <- [y..(y+height-1)] ] ++ infodump
    where getTileFill :: (Integer, Integer) -> String
          getTileFill (xTF,yTF) = if Cell xTF yTF True `elem` cl then "██" else "░░"
          infodump = "\nFieldsize:"++show(width)++"x"++show(height)++" ; from "++show(x)++"|"++show(y)++" to "++show(x+width-1)++"|"++show(y+height-1)