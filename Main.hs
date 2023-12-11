import GoLAlgorithm
import Cell
import System.IO
import Util
import Data.List
import System.Exit


main :: IO ()
main = do
    putStrLn "Main Menu:\n1: Play with default board\n2: Load board to play\n3: Create custom board\n4: Quit"
    choice <- getLine
    runMode choice

runMode:: String -> IO ()
runMode s
    | s == "1" = playBoard "boards/board1.txt"
    | s == "2" = getBoardName
    | s == "3" = buildBoardFromCli [] ""
    | s == "4" = (exitWith ExitSuccess)

playBoard:: String -> IO()
playBoard s = do
    contents <- readFile s
    playRound (createBoardFromString contents) (-25, -25) (50, 50)

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
    putStrLn (boardToString l (-25, -25) (50, 50))
    putStrLn "Type 'quit' to quit or Enter coordinates in format: x y"
    text <- getLine
    buildBoardFromCli l text
buildBoardFromCli l str = let x = read (head (words str))::Integer
                              y = read (last (words str))::Integer in
                              buildBoardFromCli (removeDuplicates (Cell x y True :l)) ""


playRound:: [Cell] -> (Integer, Integer) -> (Integer, Integer) -> IO ()
playRound cl (xco, yco) (width, height)= do
    putStrLn (boardToString cl (xco, yco) (width, height))
    input <- getLine
    handleRoundInput cl (xco, yco) (width, height) input

handleRoundInput::[Cell] -> (Integer, Integer) -> (Integer, Integer) -> String -> IO()
handleRoundInput cl (xco, yco) (width, height) input
    | input == "quit" = main
    | (input == "w" || input ==  "up") = playRound cl (xco, yco-5) (width, height)
    | (input == "s" || input ==  "down") = playRound cl (xco, yco+5) (width, height)
    | (input == "a" || input ==  "left") = playRound cl (xco-5, yco) (width, height)
    | (input == "d" || input ==  "right") = playRound cl (xco+5, yco) (width, height)
    | (input == "+" || input ==  "in") && width >=15 = playRound cl (xco+2, yco+2) (width-4, height-4)
    | (input == "-" || input ==  "out") = playRound cl (xco-2, yco-2) (width+4, height+4)
    | otherwise = playRound (gameOfLife cl) (xco, yco) (width, height)