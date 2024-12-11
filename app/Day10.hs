module Main where

import qualified Data.Map.Strict as Map
import Parser
import System.Environment
import Map2D


exploreTo9p2 :: Coordinates -> Map2D Int -> Int
exploreTo9p2 (x,y) amap = case getTile (x,y) amap of
        Just 9 -> 1 
        Just _ -> sum (map (`exploreTo9p2` amap) neighbors)
        Nothing -> 0
    where 
    neighbors = filter validNeighbor [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    validNeighbor (nx, ny) = case getTile (nx, ny) amap of  -- Check if neighbor is valid
        Just v -> v == currentValue + 1  -- If neighbor value is current value + 1
        Nothing -> False  -- If no tile exists, it's not a valid neighbor
    currentValue = case getTile (x, y) amap of
        Just v -> v  -- Get the current tile value
        Nothing -> -1  -- If no tile exists at the position, assume invalid currentValue
combine :: Int -> Int -> Int
combine x y
  | x == -1 = -1
  | y == -1 = -1
  | otherwise = x 

exploreTo9 :: Coordinates -> Map2D Int -> Map2D Int
exploreTo9 (x,y) amap = case getTile (x,y) amap of
        Just 9 -> Map.insert (x,y) (-1) amap
        Just _ ->  foldr (Map.unionWith combine)amap (map (`exploreTo9` amap) neighbors)
        Nothing -> amap
    where 
    neighbors = filter validNeighbor [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    validNeighbor (nx, ny) = case getTile (nx, ny) amap of  -- Check if neighbor is valid
        Just v -> v == currentValue + 1  -- If neighbor value is current value + 1
        Nothing -> False  -- If no tile exists, it's not a valid neighbor
    currentValue = case getTile (x, y) amap of
        Just v -> v  -- Get the current tile value
        Nothing -> -1  -- If no tile exists at the position, assume invalid value

part1 :: IO ()
part1 = do
    map2d <- createMap2D "./inputs/day10.txt"
    let map2dInts = map2DCharToInt map2d
    let zeros = Map.keys (Map.filter (== 0) map2dInts)
    let trails = map (`exploreTo9` map2dInts) zeros
    let numOfTrails = sum (map length (map (Map.filter (==(-1))) trails)) 
    print numOfTrails
    print "1"
part2 :: IO ()
part2 = do
    map2d <- createMap2D "./inputs/day10.txt"
    let map2dInts = map2DCharToInt map2d
    let zeros = Map.keys (Map.filter (== 0) map2dInts)
    let trails = map (`exploreTo9p2` map2dInts) zeros
    let numOfTrails = sum trails 
    print numOfTrails
    print "2"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
