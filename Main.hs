import GoLAlgorithm
import Cell
import System.IO
import Util
import Data.List
import System.Exit
import RuleSet
import Text.XHtml (width)


main :: IO ()
main = do
    putStrLn "Main Menu:\n1: Play with default board\n2: Load board to play\n3: Create custom board\n4: Create custom ruleset\n5: Quit"
    choice <- getLine
    runMode choice

runMode:: String -> IO ()
runMode s
    | s == "1" = playBoardWithRuleSet "boards/board1.txt"
    | s == "2" = getBoardName
    | s == "3" = buildBoardFromCli [] ""
    | s == "4" = buildRuleSetFromCli
    | s == "5" = (exitWith ExitSuccess)
    | otherwise = do
        putStrLn "please choose one of the listed numbers"
        main

playBoardWithRuleSet:: String -> IO()
playBoardWithRuleSet s = do
    contents <- readFile s
    putStrLn "Enter RuleSet-name (default: defaultRules):"
    rlName <- getLine
    rlContent <- if rlName == "" then readFile ("rules/defaultRules.txt") else readFile ("rules/"++rlName++".txt")
    playRound (createBoardFromString contents) (-25, -25) (50, 50) (createRuleSetFromString rlContent)

getBoardName:: IO()
getBoardName = do
    putStrLn " Enter board name:\n"
    boardname <- getLine
    playBoardWithRuleSet ("boards/"++boardname++".txt")

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

buildRuleSetFromCli::  IO ()
buildRuleSetFromCli = do
    putStrLn "Enter the amount of neighbouring cells, that are needed for a cell to survive (e.g. '2 3 ...')"
    alString <- getLine
    putStrLn "Enter the amount of neighbouring cells, that are needed for a new cell to spawn (e.g. '3 ...')"
    slString <- getLine
    putStrLn "Enter name for new ruleset"
    filename <- getLine
    writeFile ("rules/"++filename++".txt") (alString++"\n"++slString)
    main

playRound:: [Cell] -> (Integer, Integer) -> (Integer, Integer) -> RuleSet -> IO ()
playRound cl (xco, yco) (width, height) rl= do
    putStrLn (boardToString cl (xco, yco) (width, height))
    input <- getLine
    handleRoundInput cl (xco, yco) (width, height) rl input

handleRoundInput::[Cell] -> (Integer, Integer) -> (Integer, Integer) -> RuleSet -> String -> IO()
handleRoundInput cl (xco, yco) (width, height) rl input
    | input == "quit" = main
    | (input == "w" || input ==  "up") = playRound cl (xco, yco-5) (width, height) rl
    | (input == "s" || input ==  "down") = playRound cl (xco, yco+5) (width, height) rl
    | (input == "a" || input ==  "left") = playRound cl (xco-5, yco) (width, height) rl
    | (input == "d" || input ==  "right") = playRound cl (xco+5, yco) (width, height) rl
    | (input == "+" || input ==  "in") && width >=15 = playRound cl (xco+2, yco+2) (width-4, height-4) rl
    | (input == "-" || input ==  "out") = playRound cl (xco-2, yco-2) (width+4, height+4) rl
    | otherwise = playRound (gameOfLife cl rl) (xco, yco) (width, height) rl