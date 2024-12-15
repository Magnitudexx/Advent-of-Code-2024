module Main where

import Parser
import System.Environment
import Map2D
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty = foldr (\x acc -> if x == "" then [] : acc else (x : head acc) : tail acc) [[]]

moveSequence :: Map2D Char -> Coordinates -> String -> Map2D Char
moveSequence gmap _ (x:xs) = case x of
    '^' -> gmap
    '>' -> gmap
    'v' -> gmap
    '<' -> gmap

part1 :: IO ()
part1 = do
    listOfLines <- lineParser "inputs/test15.txt"
    let seperated = splitOnEmpty listOfLines
    let warehouse = createFromList (head seperated)
    let moves = concat (concat (tail seperated))
    print warehouse
    print moves
    print "1"

part2 :: IO ()
part2 = do
    print "1"
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    --["part2"]   -> part2
    _           -> putStr "I do not know"
