module Util where

import Cell

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