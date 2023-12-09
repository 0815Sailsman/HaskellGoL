import GoLAlgorithm
import Cell
import System.IO
import Util
import Data.List
import Data.Char (isSpace)


rstrip :: [Char] -> [Char]
rstrip = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
    putStrLn "Main Menu:\n1: Play with default board\n2: Load board to play\n3: Create custom board"
    choice <- getLine
    runMode choice

runMode:: String -> IO ()
runMode s
    | s == "1" = playBoard "boards/board1.txt"
    | s == "2" = getBoardName
    | s == "3" = buildBoardFromCli [] ""

playBoard:: String -> IO()
playBoard s = do
    contents <- readFile s
    playRound (createBoardFromString contents)

getBoardName:: IO()
getBoardName = do
    putStrLn " Enter board name:\n"
    boardname <- getLine
    playBoard ("boards/"++boardname++".txt")

buildBoardFromCli::  [Cell] -> String -> IO ()
buildBoardFromCli l "quit" = do
    putStrLn "Enter name for new board:\n"
    filename <- getLine
    writeFile ("boards/"++filename++".txt") (convertBoardToSaveString l)
    main
buildBoardFromCli l "" = do
    putStrLn (displayBoard l (-25, -25) (50, 50))
    putStrLn "Type 'quit' to quit or Enter coordinates in format: x y"
    text <- getLine
    buildBoardFromCli l text
buildBoardFromCli l str = let x = read (head (words str))::Integer
                              y = read (last (words str))::Integer in
                              buildBoardFromCli (removeDuplicates (Cell x y True :l)) ""

convertBoardToSaveString:: [Cell] -> String
convertBoardToSaveString = concatMap convertCellToSaveString

convertCellToSaveString:: Cell -> String
convertCellToSaveString (Cell originX originY _) = show originX ++ "|" ++ show originY ++ " "

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates l = [x | x<-l, count x l ==1]
  where
    count:: Eq a =>a -> [a] -> Int
    count x = length . filter (== x)

playRound:: [Cell] -> IO ()
playRound cl = do
    putStrLn (displayBoard cl (-25, -25) (50, 50))
    input <- getLine
    playRound (gameOfLife cl)

displayBoard :: [Cell] -> (Integer, Integer) -> (Integer, Integer) -> String
displayBoard cl (x,y) (height, width) = concat [ (concat [getTileFill (xco, yco) | xco <- [x..(x+width-1)]]) ++ "\n" | yco <- [y..(y+height-1)] ]
    where getTileFill :: (Integer, Integer) -> String
          getTileFill (xTF,yTF) = if Cell xTF yTF True `elem` cl then "██" else "░░"

