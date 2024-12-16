module Main where

import Parser
import System.Environment
import Text.Read (readMaybe)
import Text.Regex.Posix

data Operation = Operation String Integer Integer 
                | Unknown
    deriving (Show, Eq)
createOperation :: [String] -> Operation
createOperation [op,s1,s2] =
    case (readMaybe s1 :: Maybe Integer, readMaybe s2 :: Maybe Integer) of
        (Just n1, Just n2)  -> Operation op n1 n2
        _                   -> Unknown
createOperation _  = Unknown

execOperation :: Operation -> Integer
execOperation (Operation "mul" x y)     = x * y 
execOperation Unknown                   = 0 
execOperation _                         = 0

part1 :: IO ()
part1 = do
    listOfLines <- lineParser "inputs/test3.txt"
    let fullLine = concat listOfLines
    let pattern = "(mul)\\(([0-9]{1,3}),([0-9]{1,3})\\)"
    let matches = map tail (fullLine =~ pattern :: [[String]]) -- Find all matches
    print matches
    let operations = map createOperation matches
    let total = sum (map execOperation operations)
    print total
    --let sumOfMul = sum (map performOperation (map constructOp listOfPossibleOps) )
    --print sumOfMul

part2 :: IO ()
part2 = do
    listOfLines <- lineParser "inputs/test3.txt"
    let fullLine = concat listOfLines
    let pattern = "(do(n't)?)\\(\\)|(mul)\\(([0-9]{1,3}),([0-9]{1,3})\\)"
    let matches = (fullLine =~ pattern :: [[String]]) -- Find all matches
    print matches
    let operations = map createOperation matches
    let total = sum (map execOperation operations)
    print total
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
