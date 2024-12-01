module Main where


import Parser
import Data.List (sort, find)
import System.Environment

everyOther :: [Integer] -> ([Integer], [Integer])
everyOther []           = ([], [])
everyOther [x]          = ([x], [])
everyOther (x:y:rest)   = (x:every, y:other)
    where (every, other) = everyOther rest

sortPair :: ([Integer], [Integer]) -> ([Integer], [Integer])
sortPair (xs, ys) = (sort xs, sort ys)


getScores :: [Integer] -> [Integer] -> [Integer]
getScores [] _   = []
getScores (x:rest) y    = x * fromIntegral (length (filter (==x) y)) : getScores rest y 


part1 :: IO ()
part1 = do
    pf <- wordParser "./inputs/day1.txt"
    let numbers = toIntList pf
    let (s1, s2) = sortPair (everyOther numbers)
    let r = sum (map abs (zipWith (-) s1 s2))
    print r

part2 :: IO ()
part2 = do
    pf <- wordParser "./inputs/day1.txt"
    let numbers = toIntList pf
    let (s1, s2) = sortPair (everyOther numbers)
    let r = sum (getScores s1 s2)
    print r

    

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1
    ["part2"] -> part2
    _         -> putStrLn "Usage: runhaskell <file.hs> main1 | main2"
