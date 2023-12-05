import GoLAlgorithm
import Cell

board:: [Cell]
-- for testing : delete later
board = [
    Cell 0 0 True,
    Cell 0 1 True, 
    Cell 0 2 True]
main :: IO ()

main = do
  putStrLn (displayBoard board (-10, -10) (20, 20))


displayBoard :: [Cell] -> (Integer, Integer) -> (Integer, Integer) -> String
displayBoard cl (x,y) (height, width) = concat [ (concat [getTileFill (xco, yco) | xco <- [x..(x+width)]]) ++ "\n" | yco <- [y..(y+height)] ]
    where getTileFill :: (Integer, Integer) -> String
          getTileFill (xTF,yTF) = if Cell xTF yTF True `elem` cl then "██" else "░░"
