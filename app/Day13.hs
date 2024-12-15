module Main where

import Parser
import System.Environment
import Text.Regex.Posix

type Prize = (Int,Int)
data Button = Button Char Int Int
data ClawMachine = ClawMachine Button Button Prize

specificParse :: FilePath -> IO [[String]]
specificParse file = do
    fLines <- lineParser file
    let nonEmptyLines = filter (/= "") fLines
    let pattern = "Button ([A B]): [X]\\+([0-9]+), Y\\+([0-9]+)|Prize: X=([0-9]+), Y=([0-9]+)"
    return (map words nonEmptyLines)

part1 :: IO ()
part1 = do
    print "1"
part2 :: IO ()
part2 = do
    print "2"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
