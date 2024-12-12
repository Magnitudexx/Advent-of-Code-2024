module Main where

import Parser
import System.Environment

dropRed0 :: String -> String 
dropRed0 [] = []
dropRed0 [x] = [x]
dropRed0 str = show (read str :: Integer)
--stringToInteger is a good function
blinkStonesOnce :: [String] -> [String]
blinkStonesOnce = concatMap process
  where
    process x
        | x == "0"              = ["1"]
        | even (length x)       = [dropRed0 split1, dropRed0 split2]
        | otherwise             = [show ((read x :: Integer) * 2024)]
      where
        halfLength = length x `div` 2
        (split1, split2) = splitAt halfLength x

blinkStonesN :: [String] -> Int -> [String]
blinkStonesN xs n = memoized !! n
  where
    memoized = iterate blinkStonesOnce xs

part1 :: IO ()
part1 = do
    initialStones <- wordParser "./inputs/day11.txt"
    let blinkedStones = blinkStonesN initialStones 25
    --print blinkedStones
    let numOfStones = length blinkedStones 
    print numOfStones

    print "1"
part2 :: IO ()
part2 = do
    initialStones <- wordParser "./inputs/day11.txt"
    let blinkedStones = blinkStonesN initialStones 40
    --print blinkedStones
    let numOfStones = length blinkedStones 
    print numOfStones
    print "2"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
