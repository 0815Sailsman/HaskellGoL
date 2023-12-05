import GoLAlgorithm
import Cell
import System.IO

board:: [Cell]
-- for testing : delete later
board = [
    Cell 0 0 True,
    Cell 0 1 True, 
    Cell 0 2 True]


main :: IO ()
main = do
    contents <- readFile "board2.txt"
    playRound (createBoardFromString contents)


playRound:: [Cell] -> IO ()
playRound cl = do 
    putStrLn (displayBoard cl (-25, -25) (50, 50))
    input <- getLine
    playRound (gameOfLife cl)

displayBoard :: [Cell] -> (Integer, Integer) -> (Integer, Integer) -> String
displayBoard cl (x,y) (height, width) = concat [ (concat [getTileFill (xco, yco) | xco <- [x..(x+width-1)]]) ++ "\n" | yco <- [y..(y+height-1)] ]
    where getTileFill :: (Integer, Integer) -> String
          getTileFill (xTF,yTF) = if Cell xTF yTF True `elem` cl then "██" else "░░"


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