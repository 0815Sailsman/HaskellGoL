import GoLAlgorithm
import Cell
import System.IO
import Util


main :: IO ()
main = do
    contents <- readFile "boards/board1.txt"
    playRound (createBoardFromString contents)


playRound:: [Cell] -> IO ()
playRound cl = do
    putStrLn (playIRounds cl 50)

playIRounds :: (Num t, Eq t) => [Cell] -> t -> String
playIRounds cl 0 = displayBoard cl (-25, -25) (50, 50)
playIRounds cl i = playIRounds (gameOfLife cl (createRuleSetFromString "2 3\n3")) (i-1)



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

displayBoard :: [Cell] -> (Integer, Integer) -> (Integer, Integer) -> String
displayBoard cl (x,y) (height, width) = concat [ (concat [getTileFill (xco, yco) | xco <- [x..(x+width-1)]]) ++ "\n" | yco <- [y..(y+height-1)] ]
    where getTileFill :: (Integer, Integer) -> String
          getTileFill (xTF,yTF) = if Cell xTF yTF True `elem` cl then "██" else "░░"
