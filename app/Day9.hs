module Main where

import qualified Data.Map.Strict as Map
import Parser
import System.Environment
import Data.Char (digitToInt)
import Data.IntMap (restrictKeys)
import Data.Maybe 

generateFreeSpace :: [Int] -> [[Maybe Int]]
generateFreeSpace [] = []
generateFreeSpace [n] = [replicate n Nothing]
generateFreeSpace (n:rest) = replicate n Nothing : generateFreeSpace rest

generateFileBlocks :: [(Int, Int)] -> [[Maybe Int]]
generateFileBlocks [] = []
generateFileBlocks [(x,n)] = [replicate n (Just (x `div` 2))]
generateFileBlocks ((x,n):rest) = replicate n (Just (x `div` 2)) : generateFileBlocks rest

moveBlocks :: [Maybe Int] -> [Maybe Int]
moveBlocks [] = []
moveBlocks [n] = [n] -- Single-element list with a `Just`
moveBlocks (x:rest) = case x of
    Nothing -> lastElem rest : moveBlocks (realRest rest)
    Just n  -> Just n : moveBlocks rest
  where
    lastElem r= case last r of
        Nothing -> lastElem (init r)
        Just n -> Just n
    realRest r = case last r of
        Nothing -> realRest (init r)
        Just _  -> init r

-- Helper function to safely get the last element of a list
toSizeIndex :: [[Maybe Int]] -> [(Int, Maybe Int)]
toSizeIndex [] = []
toSizeIndex [x] = [(length x, getIndex x)]
toSizeIndex (x:rest) = (length x, getIndex x) : toSizeIndex rest




safeLast :: [(Int,Maybe Int)] -> (Int,Maybe Int)
safeLast [] = (1,Nothing)
safeLast xs = last xs

getIndex :: [Maybe Int] -> Maybe Int
getIndex [] = Nothing
getIndex [x] = x
getIndex (x:_) = x
part1 :: IO ()
part1 = do
    listOfLines <- lineParser "./inputs/day9.txt"
    let oneBigString = concat listOfLines
    let listOfIntegers = map digitToInt oneBigString
    let fileData = zipWith (*) (cycle [1,0]) listOfIntegers
    let freeSpaceList = zipWith (*) (cycle [0,1]) listOfIntegers
    let indexData = zip [0..] fileData
    let fileBlocks = generateFileBlocks indexData
    let freeSpace = generateFreeSpace freeSpaceList
    let disk = concat (zipWith (++) fileBlocks freeSpace)
    let sortedDisk = moveBlocks disk
    let checksum = sum (zipWith (*) [0..]  (map (fromMaybe 0) sortedDisk))
    print checksum
    print "1"
part2 :: IO ()
part2 = do
    listOfLines <- lineParser "./inputs/test9.txt"
    let oneBigString = concat listOfLines
    let listOfIntegers = map digitToInt oneBigString
    let fileData = zipWith (*) (cycle [1,0]) listOfIntegers
    let freeSpaceList = zipWith (*) (cycle [0,1]) listOfIntegers
    let indexData = zip [0..] fileData
    let fileBlocks = generateFileBlocks indexData
    let freeSpace = generateFreeSpace freeSpaceList
    let disk = zipWith (++) fileBlocks freeSpace
    let indexDisk = toSizeIndex disk
    --print checksum
    print "2"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
