module Main where

import qualified Data.Map.Strict as Map
import Parser
import System.Environment

type TileType = (Char, Char)
type Coordinates = (Integer, Integer)
type GMap = Map.Map Coordinates TileType

insert :: String -> Integer -> Integer -> GMap -> GMap
insert [] _ _ a = a
insert [x] a b c = Map.insert (a,b) (x,'o') c
insert (x:rest) a b c =
    insert rest (a+1) b (Map.insert (a,b) (x,'o') c)

mapInsertWithIndex :: [(Integer, String)] -> GMap -> GMap
mapInsertWithIndex [] a = a
mapInsertWithIndex [(x,y)] a = insert y 0 x a
mapInsertWithIndex ((x,y):rest) a = 
    insert y 0 x (mapInsertWithIndex rest a)
    

findGuard :: [(Coordinates, TileType)] -> Coordinates
findGuard [] = (-1,-1)
findGuard [(a,(_,c))]
    | c `elem` ['^', '<', '>', 'v']     = a
    | otherwise                         = (-1,-1)
findGuard ((a,(_,c)):rest)
    | c `elem` ['^', '<', '>', 'v']     = a
    | otherwise                         = findGuard rest
getTile :: Coordinates -> GMap -> TileType
getTile a b = case Map.lookup a b of 
    Just value  -> value
    Nothing     -> ('o','o')

moveGuard :: Coordinates -> GMap -> GMap
moveGuard c@(x, y) a =
    case getTile c a of
        (_,'^') -> 
                if nextTile (x, y - 1) == 'o'  -- Stop if it's out of bounds
                then Map.insert c 'X' a  -- Stop the function by returning the map unchanged
                else if nextTile (x, y - 1) == '#'
                    then moveGuard c (Map.insert c '>' a)  -- Turn right if wall
                    else moveGuard (x, y - 1) (updateGuard c (x, y - 1) '^' a)
        (_,'>') -> 
                if nextTile (x + 1, y) == 'o'  -- Stop if it's out of bounds
                then Map.insert c 'X' a  -- Stop the function by returning the map unchanged
                else if nextTile (x + 1, y) == '#'
                    then moveGuard c (Map.insert c 'v' a)  -- Turn down if wall
                    else moveGuard (x + 1, y) (updateGuard c (x + 1, y) '>' a)
        (_,'v') -> 
                if nextTile (x, y + 1) == 'o'  -- Stop if it's out of bounds
                then Map.insert c 'X' a  -- Stop the function by returning the map unchanged
                else if nextTile (x, y + 1) == '#'
                     then moveGuard c (Map.insert c '<' a)  -- Turn left if wall
                     else moveGuard (x, y + 1) (updateGuard c (x, y + 1) 'v' a)
        (_,'<') -> 
                if nextTile (x - 1, y) == 'o'  -- Stop if it's out of bounds
                then Map.insert c 'X' a  -- Stop the function by returning the map unchanged
                else if nextTile (x - 1, y) == '#'
                     then moveGuard c (Map.insert c '^' a)  -- Turn up if wall
                     else moveGuard (x - 1, y) (updateGuard c (x - 1, y) '<' a)
        _   -> a  -- If the guard's direction is invalid, return the map unchanged
  where
    -- Helper to get the next tile at the new coordinates
    nextTile :: Coordinates -> TileType
    nextTile coord = getTile coord a

    -- Update the guard's position in the map
    updateGuard :: Coordinates -> Coordinates -> Char -> GMap -> GMap
    updateGuard oldCoord newCoord guardChar map1 =
        Map.insert oldCoord 'X' $ Map.insert newCoord guardChar map1

isIntersection :: Coordinates -> GMap -> Bool
isIntersection (x, y) gmap
    | length (filter (== 'X') nbs) >= 3     = True
    | otherwise                             = False
    where
        nbs = map (\c -> getTile c gmap) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

markCrossroads :: GMap -> GMap
markCrossroads gmap = Map.mapWithKey (\key value -> if (isIntersection key gmap) then '+' else value) gmap

part1 :: IO ()
part1 = do
    listOfLines <- lineParser "./inputs/day6.txt"
    let indexLines = zip [0..] listOfLines
    let inputMap = mapInsertWithIndex indexLines Map.empty
    let guard = findGuard (Map.toList inputMap)
    let surveyedMap = moveGuard guard inputMap
    let numOfX = length (Map.filter (=='X')  surveyedMap)
    print numOfX

part2 :: IO ()
part2 = do
    listOfLines <- lineParser "./inputs/day6.txt"
    let indexLines = zip [0..] listOfLines
    let inputMap = mapInsertWithIndex indexLines Map.empty
    let guard = findGuard (Map.toList inputMap)
    let surveyedMap = moveGuard guard inputMap 
    let path = Map.filter (=='X') surveyedMap
    let numOfLoops = length (Map.filter (=='+') (markCrossroads path))
    print numOfLoops

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
