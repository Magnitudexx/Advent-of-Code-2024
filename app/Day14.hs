module Main where

import Parser
import System.Environment
import Text.Regex.Posix
import Map2D
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Counter
import Debug.Trace (trace)

type Speed = Integer
type Time = Integer
type Size = (Integer, Integer)
data Robot = Robot Coordinates Speed Speed
            |Unknown
    deriving (Show, Eq)

matchLine :: String -> String -> [[String]]
matchLine pattern pline =  pline =~ pattern :: [[String]]

constructRobot :: [String] -> Robot
constructRobot [] = Unknown
constructRobot [x, y, vx, vy] = 
  case (readMaybe x, readMaybe y, readMaybe vx, readMaybe vy) of
    (Just x', Just y', Just vx', Just vy') ->
      Robot (x', y') vx' vy'
    _ -> Unknown
constructRobot _ = Unknown

nMod :: Integer -> Integer -> Integer
nMod x y = let result = x `mod` y in if result < 0 then result + y else result

movementSim :: Robot -> Time -> Size -> Coordinates
movementSim (Robot (x, y) vx vy) t (sx, sy) = ((x + vx*t) `mod` sx,(y + vy*t) `mod` sy )
movementSim Unknown _ _ = (-1, -1)

quadrantSafety :: Counter Coordinates -> Size ->[Int]
quadrantSafety cmap (x,y)= trace (show (q1,q2,q3,q4)) [q1, q2, q3, q4]
    where 
        quad lx hx ly hy = sum (Map.elems (Map.filterWithKey (\k _-> isInBounds (lx,hx,ly,hy) k) cmap))
        isInBounds (xLower, xUpper, yLower, yUpper) (x', y') =x' > xLower && x' < xUpper && y' > yLower && y' < yUpper
        q1 = quad (-1) (x `div` 2) (-1) (y `div` 2)
        q2 = quad (x `div` 2) x (-1) (y `div` 2)
        q3 = quad (-1) (x `div` 2) (y `div` 2) y
        q4 = quad(x `div` 2) x (y `div` 2) y

part1 :: IO ()
part1 = do
    listOfLines <- lineParser "inputs/day14.txt"
    let pattern = "=([0-9]+),([0-9]+) v=(-[0-9]+|[0-9]+),(-[0-9]+|[0-9]+)"
    let sz = (101, 103)
    let matches = concatMap (matchLine pattern) listOfLines
    let tailedMatches = map tail matches
    let robots = map constructRobot tailedMatches
    let movedRobots = map (\k -> movementSim k 100 sz ) robots

    let counter = fromList movedRobots
    print counter
    let securityFactor = product (quadrantSafety counter sz)
    print securityFactor
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
