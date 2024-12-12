module Main where

import Parser
import System.Environment
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

type Level = Int
type Expansion = [String]

type LevelExpansion = Map.Map Level Expansion

type ExpansionMap = Map.Map String LevelExpansion
-- Lookup in the nested map
lookupInNestedMap :: String -> Level -> ExpansionMap -> Maybe Expansion
lookupInNestedMap outerKey innerKey nestedMap =
    case Map.lookup outerKey nestedMap of
        Nothing -> Nothing
        Just innerMap -> Map.lookup innerKey innerMap



insertInsideNestedMap :: String -> Level -> ExpansionMap -> Expansion -> ExpansionMap
insertInsideNestedMap str n emap expansion = Map.insert str (Map.singleton n expansion) emap


part1 :: IO ()
part1 = do
    initialStones <- wordParser "./inputs/test11.txt"
    print "1"
part2 :: IO ()
part2 = do
    initialStones <- wordParser "./inputs/day11.txt"
    print "2"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"]   -> part1
    ["part2"]   -> part2
    _           -> putStr "I do not know"
