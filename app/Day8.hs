module Main where

import Map2D
import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum)
import System.Environment
import Data.Ord (comparing)
import Data.List (sortBy)
import Debug.Trace (trace)

distance :: Coordinates -> Coordinates -> (Integer, Integer)
distance (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

addToCoordinates :: Coordinates -> Integer -> Integer -> Coordinates
addToCoordinates (x0, y0) x y = (x0+x,y0+y)

checkTowerLine :: Coordinates -> [Coordinates] -> Map2D -> Char
checkTowerLine _ [] _ = '!'  -- No valid tower found
checkTowerLine coords (tower:rest) amap
    | towerTile == '#'  || tower == coords = checkTowerLine coords rest amap  -- Skip walls
    | newPointTile == towerTile = '#' 
    | otherwise = checkTowerLine coords rest amap
  where
    (x, y) = distance coords tower
    newPoint = addToCoordinates coords (2 * x) (2 * y)
    towerTile = getTile tower amap
    newPointTile = getTile newPoint amap

checkTowerLineNoLimit :: Coordinates -> [Coordinates] -> Map2D -> Integer -> Char
checkTowerLineNoLimit _ [] _ _ = '!'  -- No valid tower found
checkTowerLineNoLimit coords (tower:rest) amap iter
    | newPointTile == towerTile = '#' 
    | newPointTile /= '!' = checkTowerLineNoLimit coords (tower:rest) amap (iter + 1)
    | towerTile == '#'  || tower == coords = checkTowerLineNoLimit coords rest amap 0 -- Skip walls
    | otherwise = checkTowerLineNoLimit coords rest amap 0
  where
    (x, y) = distance coords tower
    gcdxy 
        | gcd x y == 0  = 1 
        | otherwise     = gcd x y
    newPoint = addToCoordinates tower ((1+iter) * x `div`gcdxy) ((1+iter) * y `div` gcdxy)
    towerTile = getTile tower amap
    newPointTile = getTile newPoint amap
part1 :: IO ()
part1 = do
    completeMap <- createMap2D "./inputs/day8.txt"
    let completeList      = sortBy (comparing snd <> comparing fst) (Map.keys completeMap)
    let towerList       = Map.keys (Map.filter isAlphaNum completeMap)
    let antinodeMap      =  map (\x -> checkTowerLine x towerList completeMap) completeList
    let numOfAntinodes  = length (filter (== '#')antinodeMap)
    print numOfAntinodes


part2 :: IO ()
part2 = do
    completeMap <- createMap2D "./inputs/day8.txt"
    let completeList      = sortBy (comparing snd <> comparing fst) (Map.keys completeMap)
    let towerList       = Map.keys (Map.filter isAlphaNum completeMap)
    let antinodeMap      =  map (\x -> checkTowerLineNoLimit x towerList completeMap 0) completeList
    let numOfAntinodes  = length (filter (== '#')antinodeMap)
    print numOfAntinodes

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
