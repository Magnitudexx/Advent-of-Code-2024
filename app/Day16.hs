module Main where

import System.Environment
import Map2D
import Data.Sequence (Seq, (|>), (<|), ViewL(..), viewl)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.RTS.Flags (DebugFlags(hpc))

type Score = Integer
data Node = Node Coordinates Char
            |Unknown
        deriving (Show, Eq)
type Queue = Seq Coordinates

enqueue :: Coordinates -> Queue -> Queue
enqueue x q = q |> x

infinity :: Integer
infinity = maxBound `div` 2

enqueueList :: [Coordinates] -> Queue -> Queue
enqueueList [] _ = Seq.empty
enqueueList [x] s = enqueue x s
enqueueList (x:xs) s = enqueue x (enqueueList xs s)

-- Remove an element from the queue (dequeue)
dequeue :: Queue  -> (Coordinates, Queue )
dequeue q =
  case viewl q of
    EmptyL   -> ((-1,-1), Seq.empty)  -- The queue is empty
    x :< xs -> (x, xs)  -- Dequeue the front UndefinedElement

getNodeChar :: Node -> (Coordinates, Char)
getNodeChar (Node c  a) = (c,a)
getNodeChar Unknown = ((-1,-1),'!')

updateGscore :: Map2D Score -> Coordinates -> [Coordinates] -> Map2D Score
updateGscore gScore current [] = gScore  -- Base case: no more neighbors to process
updateGscore gScore current (x:xs)
    | tgScore < gScoreNeighbor = updateGscore updatedGScore current xs
    | otherwise                = updateGscore gScore current xs
  where
    tgScore = fromMaybe infinity (Map.lookup current gScore) + 1
    gScoreNeighbor = fromMaybe infinity (Map.lookup x gScore)
    updatedGScore = Map.insert x tgScore gScore

updateFscore :: Map2D Score -> Map2D Score -> [Coordinates] -> Map2D Score
updateFscore fScore _ [] = fScore
updateFscore fScore gScore (x:xs) = updateFscore updatedFScore gScore xs
    where
        updatedFScore = fromMaybe infinity (Map.lookup x gScore) + h
        h = 
aStar :: Queue -> Map2D Char -> Map2D Coordinates -> Map2D Score -> Map2D Score -> Map2D Coordinates
aStar openset gameMap camefrom gscore fscore = case currentChar of
    'E' -> camefrom
    _ -> aStar updatedOpenSet gameMap camefrom gscore fscore 
    where
        ((x,y), emptiedOpenSet) = dequeue openset
        updatedOpenSet = enqueueList neighbors emptiedOpenSet
        currentChar = fromMaybe '!' (Map.lookup (x,y) gameMap)
        neighbors = filter validNeighbor [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        validNeighbor (nx, ny) = case getTile (nx, ny) gameMap of  -- Check if neighbor is valid
            Just v -> v == '.'  
            Nothing -> False  -- If no tile exists, it's not a valid neighbor


part1 :: IO ()
part1 = do
    listOfLines <- createMap2D "inputs/day16.txt"
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
