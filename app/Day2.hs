module Main where

import Parser 
import System.Environment
listCheck :: (Integer -> Integer -> Bool) -> [Integer] -> Bool
listCheck _ [_] = True
listCheck _ []  = False
listCheck predicate (x:y:rest) 
    | ((abs (x - y) >= 1) && (abs (x - y) <= 3)) && predicate x y       = listCheck predicate (y:rest)
    | otherwise                                                         = False


isDescending :: [Integer] -> Bool
isDescending = listCheck (>) 

isAscending :: [Integer] -> Bool
isAscending = listCheck (<) 

checkAndFix :: [Integer] -> Bool
checkAndFix []  = False
checkAndFix [_] = True
checkAndFix xs
    | isAscending xs    = True
    | isDescending xs   = True
    | otherwise         = any canFix (removeElements xs)
  where
    -- Function to check if the list is ascending or descending
    canFix xss = isAscending xss || isDescending xss

    -- Function to remove each element from the list and check if it helps
    removeElements :: [Integer] -> [[Integer]]
    removeElements [] = []
    removeElements (x:xss) = xss : map (x:) (removeElements xss) 

-- Convert Bool to Int (True -> 1, False -> 0)
sumBoolList :: [Bool] -> Int
sumBoolList = sum . map fromEnum

part1 :: IO ()
part1 = do
    listOfLines <- lineParser "./inputs/day2.txt"
    let listOfIntLists = toIntListList listOfLines
    let numOfAscLists = sumBoolList (map isAscending listOfIntLists)
    let numOfDesLists = sumBoolList (map isDescending listOfIntLists)
    let r = numOfAscLists + numOfDesLists
    print r

part2 :: IO ()
part2 = do
    -- Read and parse input
    listOfLines <- lineParser "./inputs/day2.txt"
    let listOfIntLists = toIntListList listOfLines
    
    let numberOfLists = sumBoolList (map checkAndFix listOfIntLists)
    
    print numberOfLists


    -- Print results
    --print ("Total valid lists after fixes:", totalValidLists)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
